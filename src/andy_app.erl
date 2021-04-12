%%%-------------------------------------------------------------------
%% @doc andy public API
%% @end
%%%-------------------------------------------------------------------

-module(andy_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    andy_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
