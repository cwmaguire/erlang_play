-module(a).

% 2023-09-01
%
% Trying out complicated--for me, anyway--trace patterns along with the
% {message,_} function.
%
% Run with a:s()
%
% Cancels any running tracer and clears trace patterns.

-export([s/0]).
-export([flush/0]).
-export([t/0]).
-export([t1/0]).
-export([t2/0]).
-export([t3/0]).
-export([t4/0]).
-export([t5/0]).
-export([p/0]).

s() ->
  dbg:stop_clear(),
  Self = self(),
  F = fun(Msg, State) -> Self ! {msg, Msg, State}, State end,
  dbg:tracer(process, {F, state}),
  dbg:p(all, [c]),
  TracePatterns = [{['_','$1'],[{'==',{hd,'$1'},traceme}],[{message,'$1'}]},
                   {['_','$1'],[{'==',{hd,'$1'},traceme2}],[{message,'$1'}]},
                   %% This will match all calls to io:format/2
                   %%{['_','$1'],[{'/=',{hd,'$1'},traceme2}],[{message,"no match"}]},
                   {['_', [a]], [], [{message, a}]}],
  dbg:tpl(io, format, 2, TracePatterns),
  t(),
  dbg:stop_clear().

flush() ->
    flush([]).

flush(Acc) ->
  receive
      X ->
          flush([X | Acc])
      after 100 ->
          Acc
  end.

check(Args, Msg) ->
  Result = flush(),
  Expected = [{msg, {trace, self(), call, {io, format, Args}, Msg}, state}],

  case Result of
      Expected_ when Expected_ == Expected ->
          pass;
      [{msg,
        {trace,
         OtherPid,
         call,
         {io, format, Args_},
         Msg_},
        state}] when Args_ == Args,
                     Msg_ == Msg ->
          {fail, unexpected_pid, self(), '!=', OtherPid};
      _Other ->
          {fail, {result, Result}, {expected, Expected}}
  end.

check_empty() ->
    case flush() of
        [] ->
            pass;
        Other ->
            {fail, expected, [], got, Other}
    end.

t1() ->
    io:format("~p~n", [traceme]),
    check(["~p~n", [traceme]], [traceme]).

t2() ->
    io:format("~p~n", [traceme2]),
    check(["~p~n", [traceme2]], [traceme2]).

t3() ->
    io:format("~p~n", [a]),
    check(["~p~n", [a]], a).

t4() ->
    io:format("~p, ~p~n", [traceme, x]),
    check(["~p, ~p~n", [traceme, x]], [traceme, x]).

t5() ->
    io:format("~p, ~p~n", [a, b]),
    check_empty().

t() ->
    [{t1, t1()},
     {t2, t2()},
     {t3, t3()},
     {t4, t4()},
     {t5, t5()}].

%% get a bunch of different process messages
%% (no match required)
p() ->
    dbg:stop_clear(),
    Pid = spawn(fun() -> f(fun f/1) end),
    Self = self(),
    dbg:tracer(process, {fun(Msg, State) -> Self ! Msg, State end, state}),
    dbg:p(Pid, [all]),
    Pid ! message,
    Pid ! done,
    flush([]).

f(F) ->
    Pid = spawn(fun() -> receive X -> X end end),
    link(Pid),
    unlink(Pid),
    Pid ! anything,

    receive
        done ->
            io:format("f done~n"),
            ok;
        _ ->
            io:format("f called~n"),
            F(F)
    end.
