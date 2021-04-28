-module(andy_server).
-export([start_link/0]).

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).

-define(SERVER, ?MODULE).

-spec start_link() -> {ok, pid()}.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, {}, []).

%% @private
init({}) ->
    timer:send_after(5000, self(), tick),
    {ok, #{count => 0}}.

%% @private
handle_call(Request, _From, State) ->
    io:format("Message received via call [~p]~n", [Request]),
    {reply, pizda, State}.

%% @private
handle_cast(Msg, State) ->
    io:format("Message received via cast [~p]~n", [Msg]),
    {noreply, State}.

%% @private
handle_info(tick, #{count := Count}) ->
    io:format("Tick from andy_server: ~p~n", [Count]),
    timer:send_after(5000, self(), tick),
    {noreply, #{count => Count + 1}};
handle_info(Info, State) ->
    io:format("Message received [~p]~n", [Info]),
    {noreply, State}.

