-module(andy_acceptor).
-include("andy.hrl").
-include("andy_acceptor.hrl").
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
    handle_continue/2,
    terminate/2
]).

-record(state, {
    socket
}).

child_spec() ->
    #{
        id => ?MODULE,
        start => {?MODULE, start_link, []},
        restart => transient
    }.

start_link(Socket) ->
    gen_server:start_link(?MODULE, #{socket => Socket}, []).

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
          process_packet(Packet, State);
        {error, closed} ->
          ?LOG_ERROR("Socket closed"),
          {stop, normal, State}
    end.

terminate(_Reason, #state{socket = Socket}) ->
    gen_tcp:close(Socket).

process_packet(Packet, #state{socket = Socket} = State) ->
    try redis:decode(Packet) of
        {ok, [Command | Args]} ->
            process_command([string:uppercase(Command) | Args], Socket)
    catch
        error:Reason ->
            ?LOG_ERROR("invalid packet caused error: [~p]", [Reason]),
            ErrorResp = redis:encode({error, <<"ERROR">>}),
            ok = gen_tcp:send(Socket, ErrorResp)
    end,
    {noreply, State, #continue{payload = recv}}.

process_command([<<"COMMAND">>], Socket) ->
    ok = gen_tcp:send(Socket, redis:encode(?AVAILABLE_COMMANDS));
process_command([<<"GET">>, Key], Socket) ->
    Response = case andy_db:get(Key) of
        {ok, Value} ->
            Value;
        {error, no_value} ->
            {error, <<"No value.">>}
    end,
    ok = gen_tcp:send(Socket, redis:encode(Response));
process_command([<<"SET">>, Key, Value], Socket) ->
    ok = andy_db:put(Key, Value),
    ok = gen_tcp:send(Socket, redis:encode({bulk_string, <<"OK">>}));
process_command(Command, Socket) ->
    ?LOG_ERROR("Unsupported command received", [Command]),
    ok = gen_tcp:send(Socket, redis:encode({error, <<"Unsupported command.">>})).
