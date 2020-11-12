%% Confirming that an escript module can reference
%% a compiled erlang module in the same directory.
%% (I'm on a plane and don't have access to the
%% internet to check)
%%
%% See escript_use_module
-module(escript_module).

-export([test/0]).

test() ->
    "result from escript module".
