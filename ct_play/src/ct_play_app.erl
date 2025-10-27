%%%-------------------------------------------------------------------
%% @doc ct_play public API
%% @end
%%%-------------------------------------------------------------------

-module(ct_play_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    ct_play_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
