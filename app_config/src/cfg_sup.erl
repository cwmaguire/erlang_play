-module(cfg_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
    supervisor:start_link(cfg_sup, []).

init(_Args) ->
    SupFlags =
        #{strategy => one_for_one,
          intensity => 1,
          period => 5},
    ChildSpecs =
        [#{id => cfg,
           start => {cfg, start_link, []},
           restart => permanent,
           shutdown => brutal_kill,
           type => worker,
           modules => [cg3]}],
    {ok, {SupFlags, ChildSpecs}}.
