-module(pathfind).

-export([start_proc/3]).
-export([reg_name/2]).
-export([path1/0]).
-export([path2/0]).
-export([path3/0]).
-export([path4/0]).
-export([path4_simplified/0]).
-export([path4_remote/0]).


start_proc(X, Y, Plotter) ->
    Self = self(),
    P = spawn(fun() -> p({X, Y, [], Self, Plotter}) end),
    Name = reg_name(X, Y),
    Return = erlang:register(Name, P),
    io:format("~p,~p is ~p, ~p [~p]~n", [X, Y, P, Name, Return]),
    P.

reg_name(X, Y) ->
  list_to_atom(lists:flatten(io_lib:format("p_~p_~p", [X, Y]))).

wait() ->
    receive
        {done, X, Y} ->
            io:format("Found ~p, ~p~n", [X, Y])
    after 1000 ->
        failed
    end.

-define(ADD, n).
-define(DEL, d).

p(State = {X, Y, done}) ->
    receive {start, X_, Y_} ->
        io:format("~p,~p is done, ignoring ~p,~p~n", [X, Y, X_, Y_]),
        p(State)
    after 1000 ->
        io:format("~p ended done~n", [self()])
    end;
p(State = {X, Y, N, P, Plotter}) ->
    io:format("~p,~p (~p) listening ...~n", [X, Y, self()]),
    receive
        {?ADD, NX, NY, NPid} ->
            N2 = [{NX, NY, NPid} | N],
            io:format("~p (~p, ~p) adds ~p,~p (~p): ~p~n", [self(), X, Y, NX, NY, NPid, N2]),
            p({X, Y, N2, P, Plotter});
        {?DEL, DX, DY, DPid} ->
            N2 = lists:filter(fun({X_, Y_, Pid_})
                                when DX == X_,
                                     DY == Y_,
                                     DPid == Pid_ ->
                                      false;
                                 (_) ->
                                      true
                              end,
                              N),
            io:format("~p (~p, ~p) deletes ~p,~p (~p): ~p~n", [self(), X, Y, DX, DY, DPid, N2]),
            p({X, Y, N2, P, Plotter});
        {start, X_, Y_} when X_ == X, Y_== Y ->
            Plotter ! {X, Y},
            P ! {done, X, Y},
            p({X, Y, done});
        {start, X_, Y_} ->
            Plotter ! {X, Y},
            io:format("~p,~p: ~p,~p~n", [X, Y, X_, Y_]),
            path_(X_, Y_, State),
            p({X, Y, done})
    after 1000 ->
            io:format("~p ended listening~n", [self()])
    end.

plot(G) ->
    plot_(G),
    receive
        {X, Y} ->
            plot([{X, Y} | G])
    after 1000 ->
        ok
    end.


plot_(G) ->
    H = max(4, lists:max([Y || {_, Y} <- G])),
    W = max(4, lists:max([X || {X, _} <- G])),

    Row = [$- || _ <- lists:seq(1, W)],
    Table = [Row || _ <- lists:seq(1, H)],

    G2 = lists:foldl(fun plot/2, Table, G),
    [io:format("~p~n", [R]) || R <- lists:reverse(G2)],
    io:format("~n").


plot({X, Y}, Graph) ->
    %io:format("plot: ~p, ~p~n~p~n~n", [X, Y, Graph]),
    {R1, [R | R2]} = lists:split(Y - 1, Graph),
    {C1, [_ | C2]} = lists:split(X - 1, R),
    R1 ++ [C1 ++ [$* | C2]] ++ R2.

%split(1, [X]) ->
    %{[], [X]};
%split(N, List) ->
    %lists:split(N, List).

path_(X, Y, {_, _, N, _, _}) ->
    [P ! {start, X, Y} || {_, _, P} <- sort(X, Y, N)].

sort(X, Y, N) ->
    lists:sort(fun({Xa, Ya, _},
                   {Xb, Yb, _}) ->
                    dist(X, Y, Xa, Ya) =< dist(X, Y, Xb, Yb)
               end,
               N).

dist(X1, Y1, X2, Y2) ->
    erlang:abs(X1 - X2) + erlang:abs(Y1 - Y2).


path1() ->
    io:format("Self is ~p~n", [self()]),

    Plotter = spawn(fun() -> plot([{1, 1}]) end),
    P1_1 = start_proc(1, 1, Plotter),
    P1_2 = start_proc(1, 2, Plotter),


    P1_1 ! {n, 1, 2, P1_2},

    P1_2 ! {n, 1, 1, P1_1},

    P1_1 ! {start, 1, 2},

    wait().

path2() ->
    io:format("Self is ~p~n", [self()]),

    Plotter = spawn(fun() -> plot([{1, 1}]) end),
    P1_1 = start_proc(1, 1, Plotter),
    P1_2 = start_proc(1, 2, Plotter),
    P2_1 = start_proc(2, 1, Plotter),


    P1_1 ! {n, 1, 2, P1_2},
    P1_1 ! {n, 2, 1, P2_1},

    P1_2 ! {n, 1, 1, P1_1},
    P2_1 ! {n, 1, 1, P1_1},

    P1_1 ! {start, 1, 2},

    wait().

path3() ->
    Plotter = spawn(fun() -> plot([{1, 1}]) end),
    io:format("Self is ~p~n", [self()]),
    io:format("Plotter is ~p~n", [Plotter]),

    P1_1 = start_proc(1, 1, Plotter),
    P1_2 = start_proc(1, 2, Plotter),
    P2_1 = start_proc(2, 1, Plotter),
    P2_2 = start_proc(2, 2, Plotter),


    P1_1 ! {n, 1, 2, P1_2},
    P1_1 ! {n, 2, 1, P2_1},

    P1_2 ! {n, 1, 1, P1_1},
    P1_2 ! {n, 2, 2, P2_2},

    P2_1 ! {n, 1, 1, P1_1},
    P2_1 ! {n, 2, 2, P2_2},

    P2_2 ! {n, 1, 2, P1_2},
    P2_2 ! {n, 2, 1, P2_1},

    P1_1 ! {start, 2, 2},

    wait().

path4() ->
    Plotter = spawn(fun() -> plot([{1, 1}]) end),
    io:format("Self is ~p~n", [self()]),
    io:format("Plotter is ~p~n", [Plotter]),

    P1_1 = start_proc(1, 1, Plotter),
    P1_2 = start_proc(1, 2, Plotter),
    P1_3 = start_proc(1, 3, Plotter),

    P2_1 = start_proc(2, 1, Plotter),
    P2_2 = start_proc(2, 2, Plotter),
    P2_3 = start_proc(2, 3, Plotter),

    P3_1 = start_proc(3, 1, Plotter),
    P3_2 = start_proc(3, 2, Plotter),
    P3_3 = start_proc(3, 3, Plotter),

    % ┌───────────────┐
    % │1,3   2,3   3,3│
    % ├──────────┐    │
    % │1,2   2,2 │ 3,2│
    % │    ──────┘    │
    % │1,1   2,1   3,1│
    % └───────────────┘

    P1_1 ! {n, 1, 2, P1_2},
    P1_1 ! {n, 2, 1, P2_1},

    P1_2 ! {n, 1, 1, P1_1},
    P1_2 ! {n, 2, 2, P2_2},

    P1_3 ! {n, 2, 3, P2_3},

    P2_1 ! {n, 1, 1, P1_1},
    P2_1 ! {n, 3, 1, P3_1},

    P2_2 ! {n, 1, 2, P1_2},

    P2_3 ! {n, 1, 3, P1_3},
    P2_3 ! {n, 3, 3, P3_3},

    P3_1 ! {n, 2, 1, P2_1},
    P3_1 ! {n, 3, 2, P3_2},

    P3_2 ! {n, 3, 1, P3_1},
    P3_2 ! {n, 3, 3, P3_3},

    P3_3 ! {n, 2, 3, P2_3},
    P3_3 ! {n, 3, 2, P3_2},

    P1_1 ! {start, 1, 3},

    wait().

path4_simplified() ->
    Plotter = spawn(fun() -> plot([{1, 1}]) end),
    io:format("Self is ~p~n", [self()]),
    io:format("Plotter is ~p~n", [Plotter]),

    % ┌───────────────┐
    % │1,3   2,3   3,3│
    % ├──────────┐    │
    % │1,2   2,2 │ 3,2│
    % │    ──────┘    │
    % │1,1   2,1   3,1│
    % └───────────────┘

    graph:create(3, 3, Plotter),
    graph:disconnect(1, 3, 1, 2),
    graph:disconnect(2, 3, 2, 2),
    graph:disconnect(2, 2, 3, 2),
    graph:disconnect(2, 2, 2, 1),

    timer:sleep(10),

    io:format("~p~n", [registered()]),

    timer:sleep(10),

    p_1_1 ! {start, 1, 3},

    wait().


path4_remote() ->
    Plotter = {plot, 'plot@a'},
    io:format("Self is ~p~n", [self()]),
    io:format("Plotter is ~p~n", [Plotter]),

    % ┌───────────────┐
    % │1,3   2,3   3,3│
    % ├──────────┐    │
    % │1,2   2,2 │ 3,2│
    % │    ──────┘    │
    % │1,1   2,1   3,1│
    % └───────────────┘

    graph:create(3, 3, Plotter),
    graph:disconnect(1, 3, 1, 2),
    graph:disconnect(2, 3, 2, 2),
    graph:disconnect(2, 2, 3, 2),
    graph:disconnect(2, 2, 2, 1),

    Plotter ! {graph, 3, 3},

    timer:sleep(10),

    io:format("~p~n", [registered()]),

    timer:sleep(10),

    p_1_1 ! {start, 1, 3},

    wait().
