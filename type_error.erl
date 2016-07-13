-module(type_error).

-export([b/0]).

-record(person, {name, age}).

a(_Person = #person{}) ->
    ok.

b() ->
    a(1).
