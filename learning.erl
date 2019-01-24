-module(learning).

-export([a/1]).
-export([b/1]).
-export([repeat/2]).
-export([timer/1]).
-export([func/1]).

% TESTS
-export([test/0]).

a(List) ->
    a(List, []).

a([H | T], List) when H rem 2 == 0 ->
  a(T, [H | List]);
a([_ | T], List) ->
  a(T, List);
a([], List) ->
  List;
a(_, _List) ->
  io:format("1st param is not a list~n").

b({[1], "B", X, {}}) ->
    io:format("X is ~p~n", [X]);
b(_Nope) ->
    io:format("NOPE!~n").

test() ->
    [2] = a([1,2,3]),
    [] = a([1,3]),
    [2] = a([2,3,4]).

repeat(0, _) ->
    ok;
repeat(X, Output) when is_integer(X), X > 0 ->
    case Output of
        true ->
            io:format(".");
        _ ->
            ok
    end,
    repeat(X - 1, Output);
repeat(Other, _) ->
    io:format("Bad input: ~p~n", [Other]).

timer(NumFuns) ->
    [spawn(func(I)) || I <- lists:seq(0, NumFuns)].

func(I) ->
    fun() ->
        timer:sleep(5000 + rand:uniform(5000)),
        case I of
            X when X rem 100 == 0 ->
                io:format(".");
            _ ->
                ok
        end
    end.
