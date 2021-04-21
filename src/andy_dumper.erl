-module(andy_dumper).
-include("andy.hrl").
-include_lib("kernel/include/logger.hrl").

-export([
    child_spec/0,
    start_link/0,
    dump_to_file/0,
    load_from_file/0,
    parse_binary/1
]).

-behaviour(gen_server).
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2
]).

-define(SERVER, ?MODULE).

-define(DB_FILE, "db.rdb").

-record(state, {
    data = #{}
}).

child_spec() ->
    #{
        id => ?MODULE,
        start => {?MODULE, start_link, []},
        restart => transient
    }.

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, {}, []).

init({}) ->
    timer:send_after(1000, self(), load),
    {ok, #state{}}.

handle_call(_Request, _From, State) ->
    {reply, State}.

%% @private
handle_cast(_, State) ->
    {noreply, State}.

handle_info(load, State) ->
    load_from_file(),
    timer:send_after(5000, self(), tick),
    {noreply, State};
handle_info(tick, State) ->
    % ?LOG_DEBUG("Time to dump some data"),
    dump_to_file(),
    timer:send_after(5000, self(), tick),
    {noreply, State};
handle_info(Info, State) ->
    io:format("Message received [~p]~n", [Info]),
    {noreply, State}.

dump_to_file() ->
    {ok, DataMap} = andy_db:dump(),
    % ?LOG_DEBUG("Dumping DB [~p]", [DataMap]),
    % ?LOG_DEBUG("Dumping DB. Size = [~p]", Size = maps:size(Data)), # why doesn't this work?
    Size = maps:size(DataMap),
    if
        Size > 0 ->
            Binary = term_to_binary(DataMap),
            ?LOG_DEBUG("Dumping DB: [~p]", [Binary]),
            file:write_file(?DB_FILE, Binary);
        true ->
            ?LOG_DEBUG("Nothing to dump"),
            false
    end.

load_from_file() ->
    case file:read_file(?DB_FILE) of
        {ok, Binary} ->
            case parse_binary(Binary) of
                {ok, Term} ->
                    DB_DATA = #state{data = Term}, % nahuya?
                    andy_db:load(DB_DATA),
                    ?LOG_DEBUG("Loading DB: [~p]", [DB_DATA]);
                {error, Reason} ->
                    ?LOG_DEBUG("Failed to parse the binary with reason: [~p]", [Reason])
            end;
        {error, enoent} ->
            ?LOG_DEBUG("Nothing to load");
        {error, Reason} ->
            ?LOG_DEBUG("Failed to load the file with reason: [~p]", Reason)
    end.

parse_binary(Binary) ->
    try binary_to_term(Binary) of
        Term ->
            {ok, Term} % если добавить ; то упадет с <src/andy_dumper.erl:100: syntax error before: 'end'>
    catch
        error:Reason -> % how to get the first line of log?
            {error, Reason}
    end.

% <<131,116,0,0,0,3,109,0,0,0,1,97,109,0,0,0,3,49,50,51,109,0,0,0,1,98,109,0,0,0,3,49,49,49,109,0,0,0,1,99,109,0,0,0,12,49,50,51,49,50,51,50,49,51,50,49,51>>
