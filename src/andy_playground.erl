-module(andy_playground).

-export([
    hello/0,
    hello/1,
    hello_case/1,
    start/1,
    send_after2/3
]).

-spec hello() -> ok.
hello() ->
    io:format("Hello world~n").

-spec hello(term()) -> ok.
hello(Name) ->
    io:format("Hello, ~s~n", [Name]).

-spec hello_case(term()) -> ok.
hello_case(ok) ->
    io:format("Everything is ok!~n");
hello_case(Arg) ->
    case Arg of
        foo ->
            io:format("Foo is coming...~n");
        bar ->
            io:format("Fuck you!~n");
        {ok, Result} ->
            io:format("Result is [~p]~n", [Result]);
        SomethingElse ->
            io:format("Don't know what you want, stupid!~n~p~n", [SomethingElse])
    end.

%% application -> module -> function


pizda(State) ->
    receive
        {get_state, From} ->
            From ! State,
            pizda(State);
        {put_state, NewState} ->
            pizda(NewState);
        exit ->
            io:format("Goodbye~n");
        Msg ->
            io:format("Message received [~p]~n", [Msg]),
            pizda(State)
    after 5000 ->
        io:format("Tick~n"),
        pizda(State)
    end.

start(InitialState) ->
    Pid = spawn_link(fun() -> pizda(InitialState) end),
    io:format("My name is ~p", [Pid]),
    {ok, Pid}.

send_after2(Timeout, Pid, Msg) ->
    Ref = make_ref(),
    spawn(fun() ->
        receive
            {cancel, Ref} ->
                ok
        after Timeout ->
            Pid ! Msg
        end
    end),
    Ref.
