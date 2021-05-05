%%%-------------------------------------------------------------------
%% @doc andy public API
%% @end
%%%-------------------------------------------------------------------

-module(andy_app).

-behaviour(application).

-export([start/2, stop/1]).

-spec start(application:start_type(), term()) -> {ok, pid()}.
start(_StartType, _StartArgs) ->
    ok = andy_db:init_backend(),
    andy_sup:start_link().

-spec stop([]) -> ok.
stop(_State) ->
    ok.
