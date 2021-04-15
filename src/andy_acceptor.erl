-module(andy_acceptor).
% -include("andy_acceptor.hrl").
-include_lib("kernel/include/logger.hrl").

-export([
    child_spec/0,
    start_link/1
]).

-behaviour(gen_server).
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    handle_continue/2
]).

-record(state, {
    socket
}).

-record(continue, {
    payload
}).

child_spec() ->
    #{
        id => ?MODULE,
        start => {?MODULE, start_link, []},
        restart => transient
    }.

start_link(Socket) ->
    gen_server:start_link(#{socket => Socket}, []).

%% @private
init(#{socket := Socket}) ->
    % {ok, Listen} = gen_tcp:listen(?PORT, ?OPTIONS),
    State = #state{socket = Socket},
    {ok, State, #continue{payload = recv}}.

%% @private
handle_call(_Request, _From, State) ->
    {reply, {error, unknown_call}, State}.

%% @private
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

handle_continue(recv, #state{socket = Socket} = State) ->
  ?LOG_DEBUG("I'm going to receive some data from socket"),
  case gen_tcp:recv(Socket, 0) of
      {ok, Packet} ->
          process_command(Packet, State);
      {error, closed} ->
          ?LOG_ERROR("Socket closed"),
          {stop, normal, State}
  end.

process_command(<<"PUT ", Payload/binary>>, #state{socket = _Socket} = State) ->
    ?LOG_DEBUG("put command received with payload [~p]", [Payload]),
    % Chomped = string:chomp(Payload),
    % [Key, Value] = string:split(Chomped, <<" ">>),
    % Data = maps:put(Key, Value, State#state.data),
    {noreply, State, #continue{payload = recv}};
process_command(<<"GET ", Payload/binary>>, #state{socket = _Socket} = State) ->
    ?LOG_DEBUG("get command received with payload [~p]", [Payload]),
    % Key = string:chomp(Payload),
    % Value = maps:get(Key, State#state.data, <<"ERROR:unknown_key">>),
    % ok = gen_tcp:send(Socket, <<Value/binary, "\n">>),
    {noreply, State, #continue{payload = recv}};
process_command(Command, #state{socket = Socket} = State) ->
    ?LOG_ERROR("Unknown commant [~p]", [Command]),
    ok = gen_tcp:send(Socket, <<"ERROR\n">>),
    {noreply, State, #continue{payload = recv}}.



