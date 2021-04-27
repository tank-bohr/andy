-module(andy_SUITE).
-include("andy_connection.hrl").
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("kernel/include/file.hrl").

-export([
    put_get_test/1
]).

-export([
    all/0,
    groups/0,
    init_per_group/2,
    end_per_group/2,
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
    [
        {group,local_backend},
        {group, mnesia_backend}
    ].

groups() ->
    [
        {local_backend,[shuffle], [put_get_test]},
        {mnesia_backend,[shuffle], [put_get_test]}
    ].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(local_backend, Config) ->
    ok = application:start(andy),
    [{backend, local} | Config];
init_per_group(mnesia_backend, Config) ->
    MasterNode = node(),
    {ok, SlaveNode} = slave:start(net_adm:localhost(), slave),
    ok = setup_slave(SlaveNode),
    Nodes = [MasterNode, SlaveNode],
    NodeEnvs = [
        {MasterNode, ?MASTER_ENV(Nodes)},
        {SlaveNode, ?SLAVE_ENV(Nodes)}
    ],
    [start_applications(N, Env) || {N, Env} <- NodeEnvs],
    [{backend, mnesia}, {nodes, Nodes}, {slave_node, SlaveNode} | Config].


end_per_group(local_backend, _Config) ->
    application:stop(andy);
end_per_group(mnesia_backend, Config) ->
    Nodes = ?config(nodes, Config),
    SlaveNode = ?config(slave_node, Config),
    [stop_appliations(N) || N <- Nodes],
    slave:stop(SlaveNode).

init_per_testcase(_TestCase, Config) ->
    {SocketForPut, SocketForGet} = case ?config(backend, Config) of
        local ->
            {ok, Socket} = gen_tcp:connect("localhost", ?DEFAULT_PORT, ?OPTIONS),
            {Socket, Socket};
        mnesia ->
            {ok, MasterSocket} = gen_tcp:connect("localhost", ?MASTER_PORT, ?OPTIONS),
            {ok, SlaveSocket} = gen_tcp:connect("localhost", ?SLAVE_PORT, ?OPTIONS),
            {MasterSocket, SlaveSocket}
    end,
    [
        {socket_for_put, SocketForPut},
        {socket_for_get, SocketForGet}
        | Config
    ].

end_per_testcase(_TestCase, Config) ->
    lists:foreach(fun gen_tcp:close/1, [
        ?config(socket_for_put, Config),
        ?config(socket_for_get, Config)
    ]).

put_get_test(Config) ->
    SocketForPut = ?config(socket_for_put, Config),
    SocketForGet = ?config(socket_for_get, Config),
        ok = gen_tcp:send(SocketForPut, redis:encode([
        {bulk_string, <<"SET">>},
        {bulk_string, <<"A">>},
        {bulk_string, <<"123">>}
    ])),
    {ok, SetResp} = gen_tcp:recv(SocketForPut, 0),
    ?assertMatch({ok, <<"OK">>}, redis:decode(SetResp)),
    ok = gen_tcp:send(SocketForGet, redis:encode([
        {bulk_string, <<"GET">>},
        {bulk_string, <<"A">>}
    ])),
    {ok, GetResp} = gen_tcp:recv(SocketForGet, 0),
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
