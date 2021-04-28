-module(andy_db).

-export([
    init_backend/0,
    child_spec/0,
    put/2,
    get/1
]).

-define(KEY, andy_db_backend).
-define(BACKEND, (persistent_term:get(?KEY))).
-define(DEFAULT_BACKEND, andy_local_backend).

-callback start_link() -> {ok, pid()}.
-callback put(binary(), binary()) -> ok.
-callback get(binary()) -> {ok, binary()} | {error, no_value}.

-spec child_spec() -> supervisor:child_spec().
child_spec() ->
    #{
        id => ?BACKEND,
        start => {?BACKEND, start_link, []},
        restart => permanent
    }.

-spec put(binary(), binary()) -> ok.
put(Key, Value) ->
    ?BACKEND:put(Key, Value).

-spec get(binary()) -> {ok, binary()} | {error, no_value}.
get(Key) ->
    ?BACKEND:get(Key).

-spec init_backend() -> ok.
init_backend() ->
    persistent_term:put(?KEY, application:get_env(andy, db_backend, ?DEFAULT_BACKEND)).
