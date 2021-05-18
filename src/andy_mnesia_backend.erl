-module(andy_mnesia_backend).
-include_lib("kernel/include/logger.hrl").

-behaviour(andy_db).
-export([
    start_link/0,
    put/2,
    get/1
]).

-behaviour(gen_server).
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2
]).

-define(SERVER, ?MODULE).
-define(TABLE_NAME, item).
-record(item, {
    key,
    value
}).

-record(state, {}).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, {}, []).

put(Key, Value) ->
    {atomic, ok} = mnesia:transaction(fun() ->
        mnesia:write(#item{key = Key, value = Value})
    end),
    ok.

get(Key) ->
    case mnesia:transaction(fun() -> mnesia:read(?TABLE_NAME, Key) end) of
        {atomic, []} ->
            {error, no_value};
        {atomic, [#item{value = Value}]} ->
            {ok, Value}
    end.

init({}) ->
    Nodes = db_nodes(),
    {ok, _NodesAlive} = mnesia:change_config(extra_db_nodes, Nodes),
    case create_table(Nodes) of
        {atomic, ok} ->
            ?LOG_INFO("Table was successfully created");
        {aborted, {already_exists, ?TABLE_NAME}} ->
            ?LOG_INFO("Table already exists")
    end,
    {ok, #state{}}.

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_call}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

create_table(Nodes) ->
    mnesia:create_table(?TABLE_NAME, [
        {ram_copies, Nodes},
        {attributes, record_info(fields, ?TABLE_NAME)}
    ]).

db_nodes() ->
    Nodes = application:get_env(andy, db_nodes, nodes())
    [node() | Nodes].
