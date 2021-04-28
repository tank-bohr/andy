-module (andy_acceptor_sup).

-behaviour(supervisor).

-export([
  start_link/0,
  child_spec/0
]).

-export([init/1]).

-define(SERVER, ?MODULE).

-spec start_link() -> {ok, pid()}.
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

-spec child_spec() -> supervisor:child_spec().
child_spec() ->
    #{
        id => ?MODULE,
        start => {?MODULE, start_link, []},
        restart => permanent,
        type => supervisor
    }.

init([]) ->
    SupFlags = #{strategy => simple_one_for_one,
                 intensity => 10,
                 period => 10000},
    ChildSpecs = [
        andy_acceptor:child_spec()
    ],

    {ok, {SupFlags, ChildSpecs}}.
