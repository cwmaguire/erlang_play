%% Testing out the -on_load directive.
%%
%% Seems like only arity 0 functions can be specified.
%%
%% Output:
%%
%% $erl
%% Erlang/OTP 18 [erts-7.2] [source] [64-bit] [smp:4:4] [async-threads:10] [hipe] [kernel-poll:false]
%%
%% Eshell V7.2  (abort with ^G)
%% 1> l(on_load).
%% Module on_load loaded.
%% {module,on_load}
%% 2>
%%
-module(on_load).

-on_load(a/0).

-export([a/0]).

a() ->
    io:format("Module ~p loaded.~n", [?MODULE]),
    ok.
