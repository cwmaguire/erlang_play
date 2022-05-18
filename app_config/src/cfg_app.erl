-module(cfg_app).
-behaviour(application).

-export([start/2, stop/1]).

start(_Type, _Args) ->
    cfg_sup:start_link().

stop(_State) ->
    ok.
