-module(map_type_spec).

-type a() :: #{b := atom()}.

-export([c/0]).

-spec c() -> a().
c() ->
  return #{b => foo}.
