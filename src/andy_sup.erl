%%%-------------------------------------------------------------------
%% @doc andy top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(andy_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).
-define(LIBCLUSTER_SUP, 'Elixir.Cluster.Supervisor').

-spec start_link() -> {ok, pid()}.
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    SupFlags = #{strategy => one_for_one,
                 intensity => 10,
                 period => 10000},
    ChildrenSpecs = [
        andy_db:child_spec(),
        andy_acceptor_sup:child_spec(),
        andy_connection:child_spec(),
        andy_cowboy_sup:child_spec()
    ],

    {ok, {SupFlags, maybe_add_cluster(ChildrenSpecs)}}.

maybe_add_cluster(ChildrenSpecs) ->
    case application:get_env(libcluster, topologies) of
        {ok, Topologies} ->
            InitArgs = [Topologies, [{name, andy_cluster_sup}]],
            ClusterSpec = ?LIBCLUSTER_SUP:child_spec(InitArgs),
            [ClusterSpec | ChildrenSpecs];
        _ ->
            ChildrenSpecs
    end.
