-module(tuple_type).

-export([a/0]).

-spec a() -> list({atom(), atom()}).
a() -> [{a,b}].
