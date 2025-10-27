-module(rec_types).

-export([a/0]).
-export([b/0]).
-export([c/0]).
-export([d/0]).

%% Anything not commented out works

-record(a, {a :: tuple()}).

a() ->
    #a{}.

-record(b, {a :: {}}).

b() ->
    #b{}.

%% Only record fields can be named
%% syntax error before: '::"
%%-record(c, {a :: {x :: integer()}}).

-record(c, {a :: {integer()}}).

c() ->
    #c{}.

-record(d, {a :: {integer(), binary()}}).

d() ->
    #d{}.
