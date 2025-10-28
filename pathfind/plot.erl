-module(plot).

-export([plot/0]).

plot() ->
    register(plot, self()),

    io:put_chars([27, $[, $?, "1049", $h]),

    io:put_chars([27, $[, "10", $;, "1", $H]),

    io:put_chars("here"),

    plot_(0),

    timer:sleep(3000),

    io:put_chars([27, $[, $?, "1049", $l]),

    unregister(plot).

plot_(N) ->
    receive
        {graph, W, H} ->
            graph(W, H),
            plot_(N),
            timer:sleep(500);
        {X, Y} ->
            step(X, Y, N),
            plot_(N + 1)
    after 5000 ->
            io:put_chars("done")
    end.


-define(A, "┌").
-define(C, "┐").
-define(D, "└").
-define(E, "┘").
-define(H, "─").
-define(V, "│").
-define(L, "├").
-define(R, "┤").
-define(T, "┬").
-define(B, "┴").
-define(X, "┼").

graph(W, H) ->
    io:put_chars([27, $[, "10", $;, "1", $H]),
    HBorder = [?H || _ <- lists:seq(1, (W * 3) * 2)],
    Top = [?A, HBorder, ?C, 10, 13],
    Bot = [?D, HBorder, ?E],
    Row = [?V, [" " || _ <- lists:seq(1, (W * 3) * 2)], ?V, 10], % took out 13
    Rows = [Row || _ <- lists:seq(1, (H * 2) + 1)],

    io:put_chars([Top, Rows, Bot]).

step(XInt, YInt, N) ->
    XStr = integer_to_list(2 +  ((XInt - 1) * 6)),
    YStr = integer_to_list(10 + (YInt * 2)),
    NStr = integer_to_list(N),

    io:put_chars([27, $[, YStr, $;, XStr, $H]),
    io:format("~3.. s", [NStr]).
