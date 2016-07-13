%% July 7, 2016
%%
%% I wanted to know if I could use the ?MODULE: notation
%% in order to meck out a function without exported it.
%%
%% Nope.
-module(access_non_exported_fully_qualified_fun).

-export([exported_fun/0]).

% Will not work unless the non_exported_fun is exported
%-export([non_exported_fun/0]).

exported_fun() ->
    ?MODULE:non_exported_fun().

non_exported_fun() ->
    ok.
