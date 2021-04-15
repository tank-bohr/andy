-module(andy_connection).
-include("andy.hrl").
-include("andy_connection.hrl").
-include_lib("kernel/include/logger.hrl").

-export([
    child_spec/0,
    start_link/0
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

-record(state, {
    listen
}).

child_spec() ->
    #{
        id => ?MODULE,
        start => {?MODULE, start_link, []},
        restart => permanent
    }.

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, {}, []).

%% @private
init({}) ->
    {ok, Listen} = gen_tcp:listen(?PORT, ?OPTIONS),
    State = #state{listen = Listen},
    {ok, State, #continue{payload = accept}}.

%% @private
handle_call(_Request, _From, State) ->
    {reply, {error, unknown_call}, State}.

%% @private
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

handle_continue(accept, #state{listen = Listen} = State) ->
    {ok, Socket} = gen_tcp:accept(Listen),
    ?LOG_DEBUG("accepted"),
    {ok, Child} = supervisor:start_child(andy_acceptor_sup, [Socket]),
    ?LOG_DEBUG("acceptor worker started ~p", [Child]),
    {noreply, State, #continue{payload = accept}}.
