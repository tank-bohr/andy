-define(DEFAULT_TCP_PORT, 6379).
-define(OPTIONS, [
    binary,
    {packet, raw},
    {active, false},
    {reuseaddr, true}
]).
