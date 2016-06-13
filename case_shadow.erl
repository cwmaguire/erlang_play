-module(case_shadow).

-export([a/2]).

a(Y, X) ->
    case Y of
        X when X /= 1 ->
            "Impossibru?";
        X ->
            {x, 1};
        W ->
            {not_x, W}
    end.
