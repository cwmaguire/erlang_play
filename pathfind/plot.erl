-module(plot).

-export([plot/0]).
-export([wall/5]).

plot() ->
    register(plot, self()),

    io:put_chars([27, $[, $?, "1049", $h]),

    io:put_chars([27, $[, "10", $;, "1", $H]),

    io:put_chars("here"),

    plot_(1, 0),

    timer:sleep(500),

    io:put_chars([27, $[, $?, "1049", $l]),

    unregister(plot).

plot_(N, H0) ->
    receive
        {graph, W, H} ->
            graph(W, H),
            plot_(N, H);
        {wall, Orientation, W, H, X, Y} ->
            wall(Orientation, W, H, X, Y),
            plot_(N, H0);
        {X, Y} ->
            step(X, Y, N, H0),
            plot_(N + 1, H0);
        clear ->
            clear(),
            plot_(1, H0);
        quit ->
            io:put_chars("done")
    %after 5000 ->
            %io:put_chars("done")
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

wall(h, _W, H, X, Y) ->
    YStr = integer_to_list(10 + 2 + (H * 2) - (Y * 2) - 1),
    XStr = integer_to_list(1 + (X - 1) * 6 + 1),
    io:put_chars([27, $[, YStr, $;, XStr, $H]),
    io:put_chars([?H || _ <- lists:seq(1, 6)]);
    %io:put_chars([?H || _ <- lists:seq(1, 50)]).
wall(v, _W, H, X, Y) ->
    YStr = integer_to_list(10 + (H * 2) + 2 - (2 * Y)),
    XStr = integer_to_list(1 + X * 6),
    io:put_chars([27, $[, YStr, $;, XStr, $H]),
    io:put_chars([?V]).
    %io:put_chars([?H || _ <- lists:seq(1, 50)]).

step(XInt, YInt, N, H) ->
    Col = integer_to_list(2 +  ((XInt - 1) * 6)),
    Row = integer_to_list(10 + (H * 2) + 2 - (YInt * 2)),  %% TODO replace "8" with ((height * 2) + 2)
    NStr = integer_to_list(N),

    io:put_chars([27, $[, Row, $;, Col, $H]),
    io:format("~3.. s", [NStr]),

    Y2Str = integer_to_list(20 + N),
    X2Str = "1",
    io:put_chars([27, $[, Y2Str, $;, X2Str, $H]).

    %XStr = integer_to_list(XInt),
    %YStr = integer_to_list(YInt),
    %io:put_chars([XStr, ", ", YStr, " = ", NStr]).

clear() ->
    io:put_chars([27, $[, $2, $J]).
