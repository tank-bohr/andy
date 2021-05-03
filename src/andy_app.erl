%%%-------------------------------------------------------------------
%% @doc andy public API
%% @end
%%%-------------------------------------------------------------------

-module(andy_app).

-behaviour(application).

-export([start/2, stop/1]).

-spec start(application:start_type(), term()) -> {ok, pid()}.
start(_StartType, _StartArgs) ->
    case application:get_env(andy, http_mode) of
        {ok, master} ->
            init_web_server();
        _Else ->
        false
    end,
    ok = andy_db:init_backend(),
    andy_sup:start_link().

-spec stop([]) -> ok.
stop(_State) ->
    ok.

init_web_server() ->
    Dispatch = cowboy_router:compile([
        {'_', [{'_', andy_handler, []}]}
    ]),
    {ok, _} = cowboy:start_clear(andy_http_listener,
        [{port, 8080}],
        #{env => #{dispatch => Dispatch}}
    ).

