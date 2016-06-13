-module(mines).

-export([solutions/1]).

solutions(Groups) when is_list(Groups) ->
    solutions({_Picked = [], _MaxedOut = [], Groups});
solutions({Picked, _, _NoMoreGroups = []}) ->
    Picked;
solutions({Picked, MaxedOut, [{Spots, 0} | Groups]}) ->
    solutions({Picked, lists:usort(Picked ++ MaxedOut ++ Spots), Groups});
solutions({Picked, MaxedOut, [Group = {Spots, NumPicks} | Groups]}) ->
    case lists:foldl(fun valid_solutions/2, {Picked, MaxedOut, Group, []}, Spots) of
        {_, _, _, []} ->
            [bad_solution | Picked];
        {_, _, _, NewSolutions} ->
            NewArgs = [{NewPicked, MaxedOut, [{SpotsLeft, NumPicks - 1} | Groups]} || {NewPicked, SpotsLeft} <- NewSolutions],
            [solutions(NewArg) || NewArg <- NewArgs]
    end.

valid_solutions(Spot, {CurrSpots, MaxedOut, Group = {PossibleSpots, Max}, Solutions}) ->
    case is_valid(Spot, PossibleSpots, CurrSpots, MaxedOut, Max) of
        false ->
            {CurrSpots, MaxedOut, Group, Solutions};
        _ ->
            NewSolution = {[Spot | CurrSpots], PossibleSpots -- [Spot]},
            {CurrSpots, MaxedOut, Group, [NewSolution | Solutions]}
    end.

%is_valid(_, Spots, _Picked, MaxedOut, _Max = 1) ->
    %is_disjoint(Spots, MaxedOut);
is_valid(Spot, _, Picked, MaxedOut, _) ->
    not lists:member(Spot, Picked ++ MaxedOut).

%is_disjoint(A, B) ->
    %A -- B == A.
