-module(andy_handler).
-behavior(cowboy_handler).
-include_lib("kernel/include/logger.hrl").

-export([init/2]).

init(Req=#{method := <<"GET">>, path := <<"/">>}, State) ->
    Resp = cowboy_req:reply(200,
        #{<<"content-type">> => <<"text/plain">>},
        <<
            "Hello from Andy!\n",
            "Usage\n",
            "- GET /:key to fetch a value\n",
            "- POST /:key with a value in the body to record a pair\n"
        >>,
        Req),
    {ok, Resp, State};
init(Req=#{method := <<"GET">>}, State) ->
    <<"/", Key/binary>> = cowboy_req:path(Req),
    ?LOG_DEBUG(Key),
    case andy_db:get(Key) of
        {ok, Value} ->
            Code = 200,
            RespData = Value;
        {error, no_value} ->
            Code = 404,
            RespData = "Not found"
    end,
    Resp = cowboy_req:reply(Code,
        #{<<"content-type">> => <<"text/plain">>},
        RespData,
        Req),
    {ok, Resp, State};
init(Req=#{method := <<"POST">>}, State) ->
    <<"/", Key/binary>> = cowboy_req:path(Req),
    {ok, Value, _Req} = cowboy_req:read_body(Req),
    ?LOG_DEBUG("Incoming http. Updating key [~p]", [Key]),
    andy_db:put(Key, Value),
    Resp = cowboy_req:reply(200,
        #{<<"content-type">> => <<"text/plain">>},
        Value,
        Req),
    {ok, Resp, State}.

