-module(andy_db).
-include("andy.hrl").
-include_lib("kernel/include/logger.hrl").

-export([
    child_spec/0,
    start_link/0,
    put/2,
    get/1
]).

-behaviour(gen_server).
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    handle_continue/2
]).

-define(SERVER, ?MODULE).
-define(DEFAULT_DUMP_INTERVAL, timer:seconds(10)).

-record(state, {
    data = #{},
    dirty = false,
    timer,
    dump_interval,
    dumper_pid,
    dumper_monitor
}).

-record(put, {
    key,
    value
}).

-record(get, {
    key
}).

child_spec() ->
    #{
        id => ?MODULE,
        start => {?MODULE, start_link, []},
        restart => permanent
    }.

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, {}, []).

put(Key, Value) ->
    gen_server:call(?SERVER, #put{key = Key, value = Value}).

get(Key) ->
    gen_server:call(?SERVER, #get{key = Key}).

init({}) ->
    DumpInterval = application:get_env(andy, dump_interval, ?DEFAULT_DUMP_INTERVAL),
    {ok, #state{
        timer = start_dump_timer(DumpInterval),
        dump_interval = DumpInterval
    }, #continue{payload = load}}.

handle_call(#put{key = Key, value = Value}, _From, State) ->
    Data = maps:put(Key, Value, State#state.data),
    {reply, ok, State#state{data = Data, dirty = true}};
handle_call(#get{key = Key}, _From, State) ->
    case maps:find(Key, State#state.data) of
        {ok, Value} ->
            {reply, {ok, Value}, State};
        error ->
            {reply, {error, no_value}, State}
    end;
handle_call(_Request, _From, State) ->
    {reply, {error, unknown_call}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(dump, #state{data = Data, dump_interval = DumpInterval, dirty = true} = State) ->
    case application:get_env(andy, db_file) of
        {ok, FilePath} when is_binary(FilePath) ->
            Timer = start_dump_timer(DumpInterval),
            {Pid, Mon} = spawn_monitor(fun() ->
                ok = file:write_file(FilePath, term_to_binary(Data))
            end),
            {noreply, State#state{timer = Timer, dumper_pid = Pid, dumper_monitor = Mon}};
        _ ->
            ?LOG_INFO("No db_file defined in the env. No dump"),
            {noreply, State}
    end;
handle_info(dump, #state{dump_interval = DumpInterval, dirty = false} = State) ->
    ?LOG_DEBUG("No changes. Do nothing"),
    {noreply, State#state{timer = start_dump_timer(DumpInterval)}};
handle_info({'DOWN', Mon, process, Pid, normal}, #state{dumper_pid = Pid, dumper_monitor = Mon} = State) ->
    ?LOG_DEBUG("Dump successfully finished."),
    {noreply, State#state{dirty = false}};
handle_info({'DOWN', Mon, process, Pid, Reason}, #state{dumper_pid = Pid, dumper_monitor = Mon} = State) ->
    ?LOG_ERROR("Dump failed: ~p", [Reason]),
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

handle_continue(load, State) ->
    ?LOG_DEBUG("[load]"),
    case application:get_env(andy, db_file) of
        {ok, FilePath} when is_binary(FilePath) ->
            case file:read_file(FilePath) of
                {ok, Binary} ->
                    Data = binary_to_term(Binary, [safe]),
                    {noreply, State#state{data = Data}};
                {error, enoent} ->
                    ?LOG_INFO("Data file does not exist. Do nothing"),
                    {noreply, State};
                {error, Error} ->
                    ?LOG_ERROR("Cannot load data from file: ~p", [Error]),
                    {noreply, State}
            end;
        _ ->
            ?LOG_INFO("No db_file defined in the env. No dump"),
            {noreply, State}
    end.

start_dump_timer(DumpInterval) ->
    erlang:send_after(DumpInterval, self(), dump).
