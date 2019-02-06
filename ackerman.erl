%% https://en.wikipedia.org/wiki/Ackermann_function
%% https://xuanji.appspot.com/isicp/1-2-procedures.html
%%
%% I found this while reading SICP.
%% I haven't yet groked how it works.
%% a(4,3) takes a very long time.
-module(ackerman).

-export([a/2]).

a(X, Y) ->
    a(X, Y, 0).

a(X, 0, N) ->
    Pad = lists:flatten(string:pad("", N * 2, leading)),
    io:format("~sa(~p, 0) = 0~n", [Pad, X]),
    0;
a(0, Y, N) ->
    Pad = lists:flatten(string:pad("", N * 2, leading)),
    io:format("~sa(0, ~p) = ~p~n", [Pad, Y, 2 * Y]),
    2 * Y;
a(X, 1, N) ->
    Pad = lists:flatten(string:pad("", N * 2, leading)),
    io:format("~sa(~p, 1) = 2~n", [Pad, X]),
    2;
a(X, Y, N) ->
    Pad = lists:flatten(string:pad("", N * 2, leading)),
    io:format("~sa(~p, ~p) = a(~p, (a, ~p, ~p))~n", [Pad, X, Y, X - 1, X, Y - 1]),
    a(X - 1, a(X, Y - 1, N + 2), N + 1).
