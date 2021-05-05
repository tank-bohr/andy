%%%-------------------------------------------------------------------
%% @doc andy public API
%% @end
%%%-------------------------------------------------------------------

-module(andy_app).

-behaviour(application).

-export([start/2, stop/1]).

-export([set_logger/1]).

set_logger(Logger) ->
    logger:set_handler_config(default, formatter,  Logger).

-define(DEFAULT_HTTP_PORT, 8082).

-include_lib("kernel/include/logger.hrl").

-spec start(application:start_type(), term()) -> {ok, pid()}.
start(_StartType, _StartArgs) ->
    set_logger(#{config => #{file => ["~/Hobby/andy/slave.log"]}}),
    HttpPort = application:get_env(andy, web_port, ?DEFAULT_HTTP_PORT),
    HttpMode = application:get_env(andy, web_mode, zozozo),
    ?LOG_INFO("=============================starting server on port [~p]", [HttpPort]),
    ?LOG_INFO("=============================starting server on mode [~p]", [HttpMode]),
    Dispatch = cowboy_router:compile([
        {'_', [{'_', andy_handler, []}]}
    ]),
    {ok, _} = cowboy:start_clear(andy_http_listener,
        [{port, HttpPort}],
        #{env => #{dispatch => Dispatch}}
    ),
    % init_web_server(),
    ok = andy_db:init_backend(),
    andy_sup:start_link().

-spec stop([]) -> ok.
stop(_State) ->
    ok.

% init_web_server() ->
%     HttpPort = application:get_env(andy, http_port, ?DEFAULT_HTTP_PORT),
%     ?LOG_ERROR("=============================starting server on port [~p]", [HttpPort]),
%     Dispatch = cowboy_router:compile([
%         {'_', [{'_', andy_handler, []}]}
%     ]),
%     {ok, _} = cowboy:start_clear(andy_http_listener,
%         [{port, HttpPort}],
%         #{env => #{dispatch => Dispatch}}
%     ).

