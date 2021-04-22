-module(andy_mnesia_backend_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

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

all() ->
    [replication_test].

replication_test(_Config) ->
    ?assert(true).

init_per_suite(Config) ->
    Result = ct_slave:start(replica),
    ?debugVal(Result),
    Config.

end_per_suite(_Config) ->
    ok.

init_per_testcase(_TestCase, Config) ->
    Config.

end_per_testcase(_TestCase, _Config) ->
    ok.
