-module(msgs).

-export([start/1]).
-export([p/1]).

start(X) ->
    P1 = spawn(fun() -> p("\\") end),
    P2 = spawn(fun() -> p("/") end),
    P1 ! {P2, X},
    1.

p(A) ->
  receive
    {Pid, 0} ->
      io:format("~n~p received 0 from ~p at ~p ~n", [self(), Pid, now()]),
      Pid ! {self(), 0};
    {Pid, X} ->
      %io:format("~s", [A]),
      Pid ! {self(), X - 1},
      p(A)
  after 1000 ->
    io:format("~p timedout~n", [self()])
  end.
