-module(andy_mnesia_backend_SUITE).

-include("andy_connection.hrl").
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("kernel/include/file.hrl").

-export([
    replication_test/1
]).

-export([
    all/0,
    init_per_suite/1,
    end_per_suite/1,
    init_per_testcase/2,
    end_per_testcase/2
]).

-define(APPS, [mnesia, andy]).
-define(APPLICATION_ENV(Port, Nodes), [
    {kernel, [
        {logger_level, error}
    ]},
    {andy, [
        {port, Port},
        {db_backend, andy_mnesia_backend},
        {db_nodes, Nodes}
    ]}
]).

-define(MASTER_PORT, 6379).
-define(MASTER_ENV(Nodes), ?APPLICATION_ENV(?MASTER_PORT, Nodes)).

-define(SLAVE_PORT, 6378).
-define(SLAVE_ENV(Nodes), ?APPLICATION_ENV(?SLAVE_PORT, Nodes)).

all() ->
    [replication_test].

init_per_suite(Config) ->
    MasterNode = node(),
    {ok, SlaveNode} = slave:start(net_adm:localhost(), slave),
    ok = setup_slave(SlaveNode),
    Nodes = [MasterNode, SlaveNode],
    NodeEnvs = [
        {MasterNode, ?MASTER_ENV(Nodes)},
        {SlaveNode, ?SLAVE_ENV(Nodes)}
    ],
    [start_applications(N, Env) || {N, Env} <- NodeEnvs],
    [{nodes, Nodes}, {slave_node, SlaveNode} | Config].

end_per_suite(Config) ->
    Nodes = ?config(nodes, Config),
    SlaveNode = ?config(slave_node, Config),
    [stop_appliations(N) || N <- Nodes],
    slave:stop(SlaveNode).

init_per_testcase(_TestCase, Config) ->
    {ok, MasterSocket} = gen_tcp:connect("localhost", ?MASTER_PORT, ?OPTIONS),
    {ok, SlaveSocket} = gen_tcp:connect("localhost", ?SLAVE_PORT, ?OPTIONS),
    [
        {master_socket, MasterSocket},
        {slave_socket, SlaveSocket}
        | Config
    ].

end_per_testcase(_TestCase, Config) ->
    lists:foreach(fun gen_tcp:close/1, [
        ?config(master_socket, Config),
        ?config(slave_socket, Config)
    ]).

replication_test(Config) ->
    MasterSocket = ?config(master_socket, Config),
    SlaveSocket = ?config(slave_socket, Config),
        ok = gen_tcp:send(MasterSocket, redis:encode([
        {bulk_string, <<"SET">>},
        {bulk_string, <<"A">>},
        {bulk_string, <<"123">>}
    ])),
    {ok, SetResp} = gen_tcp:recv(MasterSocket, 0),
    ?assertMatch({ok, <<"OK">>}, redis:decode(SetResp)),
    ok = gen_tcp:send(SlaveSocket, redis:encode([
        {bulk_string, <<"GET">>},
        {bulk_string, <<"A">>}
    ])),
    {ok, GetResp} = gen_tcp:recv(SlaveSocket, 0),
    {ok, Value} = redis:decode(GetResp),
    ?assertEqual(<<"123">>, Value, "Should return the value we put").

start_applications(Node, Env) ->
    ok = rpc:call(Node, application, set_env, [Env]),
    lists:foreach(fun(App) ->
        ok = rpc:call(Node, application, start, [App])
    end, ?APPS).

stop_appliations(Node) ->
    lists:foreach(fun(App) ->
        ok = rpc:call(Node, application, stop, [App])
    end, ?APPS).

setup_slave(Node) ->
    Path = lists:filter(fun is_directory/1, code:get_path()),
    true = rpc:call(Node, code, set_path, [Path]),
    ok.

is_directory(Path) ->
    case file:read_file_info(Path) of
        {ok, #file_info{type = directory}} ->
            true;
        _ ->
            false
    end.
