-module(d).

-behaviour(a).
-behaviour(b).

-export([a/0]).
-export([b/0]).

a() -> a.
b() -> b.
