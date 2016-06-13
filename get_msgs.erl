-module(get_msgs).

-export([run/0]).

run() ->
  Pid = spawn(fun() -> receive stop -> ok end end),
  Pid ! {message, 1},
  Pid ! {message, 2},
  Pid ! {message, 3},
  io:format("Messages in ~p's mailbox:~n\t~p~n", [Pid, process_info(Pid, messages)]),
  Pid ! stop.

