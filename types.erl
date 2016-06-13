%% Checking to make sure a wierdly named type used only in a spec
%% doesn't generate a warning
-module(types).

-export([a/0]).
-export([b/0]).
-export([c/0]).
-export(['#e'/0]). %% no problem with funky function names
-export(['#f'/1]).

-type '#abc'() :: {}. %% no warning
-type '#d_e-f'() :: {}. %% no warning
-type x() :: {}. %% warning, never used, expected
-type y() :: {}. %% no warning
-type z() :: {}. %% no warning
-type v() :: {}. %% no warning
-type u() :: {}. %% no warning
-type '#abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ'() :: {}.

-spec a() -> '#abc'().
a() -> {}.

-spec b() -> '#d_e-f'().
b() -> ok.

-spec c() -> [y(), ...].  %% Making sure the ,... syntax doesn't mess it up
c() -> [{}, a].

%% messing around to see what dialyzer will pick up
%% (not an unused d/1 or an unreachable case apparently)
d(X) ->   %% not exported, causes warning (expected)
    case X of _ -> ok;
              1 -> not_ok  %% causes warning (expected)
    end.

-spec '#e'() -> z().
'#e'() -> {}.

%% Multiply clauses in the spec work fine
%% Also compiles and works fine
-spec '#f'(1) -> [v(), ...]; (2) -> [u(), ...].
'#f'(1) -> [{}];
'#f'(2) -> [{}].

