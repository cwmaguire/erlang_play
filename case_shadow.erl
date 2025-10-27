-module(case_shadow).

-export([a/2]).
-export([b/1]).
-export([c/1]).

%% NOTE: as of 2025-01-18 I have case shadowing warnings disabled
%% for erlang_ls: ~/.config/erlang_ls/erlang_ls.config
%%
%% diagnostics:
%%   enabled:
%%     - crossref
%%   disabled:
%%     - bound_var_in_pattern   <------

a(Y, X) ->
    case Y of
        X when X /= 1 ->
            "Impossibru?";
        X ->
            {x, 1};
        W ->
            {not_x, W}
    end.

b(Y) ->
    X = Y,
    case Y of
        X ->
            ok
    end.

c(Y) ->
    X = a,
    case Y of
        X ->
            ok;
        _ ->
            no
    end.
