%%%-------------------------------------------------------------------
%% @doc andy top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(andy_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

-spec start_link() -> {ok, pid()}.
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    SupFlags = #{strategy => one_for_one,
                 intensity => 10,
                 period => 10000},
    ChildSpecs = [
        andy_db:child_spec(),
        andy_acceptor_sup:child_spec(),
        andy_connection:child_spec(),
        andy_cowboy_sup:child_spec()
    ],

    {ok, {SupFlags, ChildSpecs}}.
