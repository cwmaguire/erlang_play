%% Checking to make sure a wierdly named type used only in a spec
%% doesn't generate a warning
-module(types).

-export([a/0]).
-export([b/0]).
-export([c/0]).
-export([d2/1]).
-export(['#e'/0]). %% no problem with funky function names
-export(['#f'/1]).
-export([g/1]).
-export([h/1, h2/1]).
-export([i/1, i2/1, i2b/1, i3/1, i4/1, i5/1, i6/1]).
-export([j/1]).

-type '#abc'() :: {}. %% no warning
-type '#d_e-f'() :: {}. %% no warning
-type x() :: {}. %% warning, never used, expected
-type y() :: {}. %% no warning
-type z() :: {}. %% no warning
-type v() :: {}. %% no warning
-type u() :: {}. %% no warning
-type '#abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ'() :: {}.
-type map_() :: #{a => integer(), b => atom()}.

-spec a() -> '#abc'().
a() -> {}.

%% Dialyzer: Invalid type specification for function types:b/0. The success typing is () -> 'ok'
-spec b() -> '#d_e-f'().
b() -> ok.

-spec c() -> [y(), ...].  %% Making sure the ,... syntax doesn't mess it up
c() -> [{}, a].

%% messing around to see what dialyzer will pick up
%% (not an unreachable case apparently if the function is unused)
d(X) ->   %% not exported, causes warning (expected)
    case X of _ -> ok;
              1 -> not_ok  %% causes warning (expected)
    end.

d2(X) ->   %% not exported, causes warning (expected)
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

%% MAPS

%% works fine
%% No way for dialyzer to check value of Map passed in
-spec g(map_()) -> integer() | atom().
g(Map) ->
    maps:get(a, Map, undefined).

%% works fine
-spec h(map_()) -> {integer() | atom(), atom()}.
h(Map) ->
    {maps:get(a, Map, undefined), maps:get(b, Map, undefined)}.

%% no error even though the 'b' value wouldn't match the type
-spec h2(map_()) -> integer().
h2(Map) ->
    maps:get(b, Map, 0).

%% No error about 'c' not being a specific key in map_()
-spec i(map_()) -> integer() | atom().
i(Map) ->
    maps:get(c, Map, undefined).

%% Dialyzer: Invalid type specification for function types:i2/1. The success typing is (#{'c':=_, _=>_}) -> any()
-spec i2(map_()) -> integer().
i2(#{c := C}) ->
    C.

%% Catches match inside the function as well
%% Dialyzer: Invalid type specification for function types:i2/1. The success typing is (#{'c':=_, _=>_}) -> any()
-spec i2b(map_()) -> integer().
i2b(Map) ->
    #{c := C} = Map,
    C.

%% Dialyzer: Invalid type specification for function types:i3/1. The success typing is (#{'a':=_, 'c':=_, _=>_}) -> {_,_}
-spec i3(map_()) -> {integer(), integer()}.
i3(#{a := A, c := C}) ->
    {A, C}.

%% Not sure what the problem is here
%% Dialyzer: Invalid type specification for function types:i4/1. The success typing is (#{'a':=_, 'b':=_, 'c':=_, _=>_}) -> {_,_,_}
-spec i4(map_()) -> {integer(), integer(), integer()}.
i4(#{a := A, b := B, c := C}) ->
    {A, B, C}.

%% No error
-spec i5(map_()) -> integer().
i5(#{a := A}) ->
    A.

%% No error
-spec i6(map_()) -> {integer(), integer()}.
i6(#{a := A, b := B}) ->
    {A, B}.

%% Dialyzer: Invalid type specification for function types:j/1. The success typing is (#{'a':='atom', _=>_}) -> 'ok'
-spec j(map_()) -> ok.
j(#{a := atom}) ->
    ok.
