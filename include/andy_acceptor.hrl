-define(GET_COMMAND, [
    {bulk_string, <<"get">>},
    2,
    [ <<"readonly">>, <<"fast">> ],
    1,
    1,
    1,
    [
      <<"@read">>,
      <<"@string">>,
      <<"@fast">>]
]).

-define(SET_COMMAND, [
    {bulk_string, <<"set">>},
    -3,
    [ <<"write">>, <<"denyoom">> ],
    1,
    1,
    1,
    [
      <<"@write">>,
      <<"@string">>,
      <<"@slow">>]
]).

-define(COMMAND_COMMAND, [
    {bulk_string, <<"command">>},
    -1,
    [ <<"random">>, <<"loading">>, <<"stale">>],
    0,
    0,
    0,
    [<<"@slow">>]
]).

-define(AVAILABLE_COMMANDS, [?GET_COMMAND, ?SET_COMMAND, ?COMMAND_COMMAND]).
