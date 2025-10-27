-module(graph).

-export([create/3]).
-export([disconnect/4]).


create(H, W, Plotter) ->
  [pathfind:start_proc(X, Y, Plotter) || X <- lists:seq(1,W), Y <- lists:seq(1,H)],

  [link(X, Y, W, H) || X <- lists:seq(1,W), Y <- lists:seq(1,H)].

link(X, Y, W, H) ->

    Name = pathfind:reg_name(X, Y),

    % Figure out valid up/down/left/right neighbours
    % CardinalDirections = [{X_, Y_} || X_ <- [X -1, X, X + 1],
    %                                          Y_ <- [Y - 1, Y, Y + 1],
    %                                          X_ /= Y_,
    %                                          X_ == X orelse Y_ == Y,
    %                                          X_ > 0,
    %                                          Y_ > 0,
    %                                          X_ =< W,
    %                                          Y_ =< H],

    Neighbours = [{X + 1, Y},
                  {X - 1, Y},
                  {X, Y - 1},
                  {X, Y + 1}],

    ValidNeighbours = [D || D = {X_, Y_} <- Neighbours, X_ > 0, Y_ > 0, X_ =< W, Y_ =< H],

    [reg(Name, X_, Y_) || {X_, Y_} <- ValidNeighbours].

reg(Name, X, Y) ->
    Name ! {n, X, Y, pathfind:reg_name(X, Y)}.

disconnect(X1, Y1, X2, Y2) ->
    Name1 = pathfind:reg_name(X1, Y1),
    Name2 = pathfind:reg_name(X2, Y2),
    Name1 ! {d, X2, Y2, Name2},
    Name2 ! {d, X1, Y1, Name1}.
