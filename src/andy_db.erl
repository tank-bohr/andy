-module(andy_db).
-export([
    child_spec/0,
    start_link/0,
    put/2,
    get/1,
    dump/0,
    load/1
]).

-behaviour(gen_server).
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2
]).

-define(SERVER, ?MODULE).

-record(state, {
    data = #{}
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

dump() ->
  gen_server:call(?SERVER, dump).

load(Data) ->
  gen_server:call(?SERVER, {load, Data}).

%% @private
init({}) ->
    {ok, #state{}}.

%% @private
handle_call(#put{key = Key, value = Value}, _From, State) ->
    Data = maps:put(Key, Value, State#state.data),
    {reply, ok, State#state{data = Data}};
handle_call(#get{key = Key}, _From, State) ->
    case maps:find(Key, State#state.data) of
        {ok, Value} ->
            {reply, {ok, Value}, State};
        error ->
            {reply, {error, no_value}, State}
    end;
handle_call(dump, _From, State) ->
  {reply, {ok, State#state.data}, State};
handle_call({load, Data}, _From, State) ->
  {reply, ok, State#state{data = Data#state.data}};
handle_call(_Request, _From, State) ->
    {reply, {error, unknown_call}, State}.

%% @private
handle_cast(_Msg, State) ->
    {noreply, State}.

%% @private
handle_info(_Info, State) ->
    {noreply, State}.
