%% use meck to change the a/0 function then uninstall meck
%% to see if the regular a/0 is restored.
%% Worked as expected:

%% $erl -pa ~/dev/voalte/audit/deps/meck/ebin
%% Erlang/OTP 18 [erts-7.2] [source] [64-bit] [smp:4:4] [async-threads:10] [hipe] [kernel-poll:false]
%%
%% Eshell V7.2  (abort with ^G)
%% 1> c(uninstall_meck).
%% {ok,uninstall_meck}
%%
%% ...
%%
%% 7> uninstall_meck:a().
%% 1
%% 8> meck:new(uninstall_meck, []).
%% ok
%% 9> meck:expect(uninstall_meck, a, 0, 2).
%% ok
%% 10> uninstall_meck:a().
%% 2
%% 11> meck:unload(uninstall_meck).
%% ok
%% 12> uninstall_meck:a().
%% 1
-module(uninstall_meck).

-export([a/0]).

a() -> 1.
