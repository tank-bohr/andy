%%%-------------------------------------------------------------------
%% @doc andy top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(andy_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%% sup_flags() = #{strategy => strategy(),         % optional
%%                 intensity => non_neg_integer(), % optional
%%                 period => pos_integer()}        % optional
%% child_spec() = #{id => child_id(),       % mandatory
%%                  start => mfargs(),      % mandatory
%%                  restart => restart(),   % optional
%%                  shutdown => shutdown(), % optional
%%                  type => worker(),       % optional
%%                  modules => modules()}   % optional
init([]) ->
    SupFlags = #{strategy => one_for_one,
                 intensity => 10,
                 period => 10000},
    ChildSpecs = [
        andy_db:child_spec(),
        andy_acceptor_sup:child_spec(),
        andy_connection:child_spec(),
        andy_dumper:child_spec()
        %, #{id => andy_server, start => {andy_server, start_link, []}}
        %, #{id => andy_playground, start => {andy_playground, start, [inital_state]}, restart => transient}
    ],

    {ok, {SupFlags, ChildSpecs}}.

%% internal functions
