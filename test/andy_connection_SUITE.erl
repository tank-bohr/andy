-module(andy_connection_SUITE).
-include("andy_connection.hrl").
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([
    put_get_test/1
]).

-export([
  all/0,
  init_per_suite/1,
  end_per_suite/1,
  init_per_testcase/2,
  end_per_testcase/2
]).

all() ->
    [put_get_test].

put_get_test(Config) ->
    Socket = ?config(socket, Config),
    ok = gen_tcp:send(Socket, redis:encode([
        {bulk_string, <<"SET">>},
        {bulk_string, <<"A">>},
        {bulk_string, <<"123">>}
    ])),
    {ok, SetResp} = gen_tcp:recv(Socket, 0),
    ?assertMatch({ok, <<"OK">>}, redis:decode(SetResp)),
    ok = gen_tcp:send(Socket, redis:encode([
        {bulk_string, <<"GET">>},
        {bulk_string, <<"A">>}
    ])),
    {ok, GetResp} = gen_tcp:recv(Socket, 0),
    {ok, Value} = redis:decode(GetResp),
    ?assertEqual(<<"123">>, Value, "Should return the value we put").

init_per_testcase(_TestName, Config) ->
    {ok, Socket} = gen_tcp:connect("localhost", ?DEFAULT_PORT, ?OPTIONS),
    [{socket, Socket} | Config].

end_per_testcase(_TestName, Config) ->
    Socket = ?config(socket, Config),
    gen_tcp:close(Socket).

init_per_suite(Config) ->
    ok = application:start(andy),
    Config.

end_per_suite(_Config) ->
    application:stop(andy).
