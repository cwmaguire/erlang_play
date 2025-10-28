-module(plot).

-export([plot/0]).

plot() ->
    register(plot, self()),

    io:put_chars([27, $[, $?, "1049", $h]),

    io:put_chars([27, $[, "10", $;, "1", $H]),

    io:put_chars("here"),

    plot_(),

    timer:sleep(3000),

    io:put_chars([27, $[, $?, "1049", $l]),

    unregister(plot).

plot_() ->
    receive
        {graph, W, H} ->
            graph(W, H);
        {X, _Y} ->
            io:put_chars(integer_to_list(X)),
            plot_()
    after 3000 ->
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
    HBorder = [?H || _ <- lists:seq(1, W * 3)],
    Top = [?A, HBorder, ?C, 10, 13],
    Bot = [?D, HBorder, ?E],
    Row = [?V, [" " || _ <- lists:seq(1, W * 3)], ?V, 10, 13],
    Rows = [Row || _ <- lists:seq(1, H)],

    io:put_chars([Top, Rows, Bot]).
