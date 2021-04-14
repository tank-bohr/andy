-module(andy_connection).
-include_lib("kernel/include/logger.hrl").

-export([
    child_spec/0,
    start_link/0
]).

-behaviour(gen_server).
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    handle_continue/2
]).

-define(SERVER, ?MODULE).
-define(PORT, 5678).
-define(OPTIONS, [
    binary,
    {packet, line},
    {active, false}
]).

-record(state, {
    listen,
    data = #{}
}).

-record(continue, {
    payload
}).

child_spec() ->
    #{
        id => ?MODULE,
        start => {?MODULE, start_link, []},
        restart => permanent
    }.

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, {}, []).

%% @private
init({}) ->
    {ok, Listen} = gen_tcp:listen(?PORT, ?OPTIONS),
    State = #state{listen = Listen},
    {ok, State, #continue{payload = accept}}.

%% @private
handle_call(_Request, _From, State) ->
    {reply, {error, unknown_call}, State}.

%% @private
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

handle_continue(accept, #state{listen = Listen} = State) ->
    {ok, Socket} = gen_tcp:accept(Listen),
    ?LOG_DEBUG("accepted"),
    NewState = State,
    Continue = {recv, Socket},
    {noreply, NewState, #continue{payload = Continue}};
handle_continue({recv, Socket}, State) ->
    ?LOG_DEBUG("I'm going to receive some data from socket"),
    case gen_tcp:recv(Socket, 0) of
        {ok, Packet} ->
            process_command(Packet, Socket, State);
        {error, closed} ->
            ?LOG_ERROR("Socket closed"),
            {noreply, State, #continue{payload = accept}}
    end.

process_command(<<"PUT ", Payload/binary>>, Socket, State) ->
    ?LOG_DEBUG("put command received with payload [~p]", [Payload]),
    Chomped = string:chomp(Payload),
    [Key, Value] = string:split(Chomped, <<" ">>),
    Data = maps:put(Key, Value, State#state.data),
    {noreply, State#state{data = Data}, #continue{payload = {recv, Socket}}};
process_command(<<"GET ", Payload/binary>>, Socket, State) ->
    ?LOG_DEBUG("get command received with payload [~p]", [Payload]),
    Key = string:chomp(Payload),
    Value = maps:get(Key, State#state.data, <<"ERROR:unknown_key">>),
    ok = gen_tcp:send(Socket, <<Value/binary, "\n">>),
    {noreply, State, #continue{payload = {recv, Socket}}};
process_command(Command, Socket, State) ->
    ?LOG_ERROR("Unknown commant [~p]", [Command]),
    ok = gen_tcp:send(Socket, <<"ERROR\n">>),
    {noreply, State, #continue{payload = {recv, Socket}}}.
