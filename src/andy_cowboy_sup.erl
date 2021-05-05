-module(andy_cowboy_sup).
-behaviour(supervisor_bridge).

-export([
    init/1,
    terminate/2
]).

-export([
    child_spec/0,
    start_link/0
]).

-define(SERVER, ?MODULE).
-define(DEFAULT_HTTP_PORT, 8082).
-define(LISTENER_NAME, andy_http_listener).

-record(state, {}).

-spec child_spec() -> supervisor:child_spec().
child_spec() ->
    #{
        id => ?MODULE,
        start => {?MODULE, start_link, []},
        restart => permanent,
        type => supervisor
    }.

start_link() ->
    supervisor_bridge:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    HttpPort = application:get_env(andy, web_port, ?DEFAULT_HTTP_PORT),
    Dispatch = cowboy_router:compile([
        {'_', [{'_', andy_handler, []}]}
    ]),
    TransportOpts = [{port, HttpPort}],
    ProtocolOpts = #{env => #{dispatch => Dispatch}},
    case cowboy:start_clear(?LISTENER_NAME, TransportOpts, ProtocolOpts) of
        {ok, Pid} ->
            {ok, Pid, #state{}};
        {error, Error} ->
            {error, Error}
    end.

terminate(_Reason, _State) ->
    cowboy:stop_listener(?LISTENER_NAME).
