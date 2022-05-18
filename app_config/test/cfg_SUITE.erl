-module(cfg_SUITE).

-include_lib("common_test/include/ct.hrl").

-export([all/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).
-export([t/1]).

all() ->
    [t].

init_per_suite(Config) ->
    ct:pal("~p:~p: Config~n\t~p~n", [?MODULE, init_per_suite, Config]),
    application:load(cfg),
    C = ct:get_config(cfg),
    ct:pal("~p:~p: C~n\t~p~n", [?MODULE, init_per_suite, C]),
    A = application:get_env(cfg, a),
    App = application:get_env(cfg, app),
    Test = application:get_env(cfg, test),
    Sys = application:get_env(cfg, sys),
    ct:pal("Config:~n"
           "~p: ~p~n"
           "~p: ~p~n"
           "~p: ~p~n"
           "~p: ~p~n",
           [a, A, app, App, test, Test, sys, Sys]),

    application:start(cfg),
    Config.

end_per_suite(_Config) ->
    application:stop(cfg).

t(Config) ->
    ct:pal("~p:~p: Config~n\t~p~n", [?MODULE, t, Config]),
    io:format("~p:~p: Config~n\t~p~n", [?MODULE, t, Config]),
    A = application:get_env(cfg, a),
    B = application:get_env(cfg, b),
    io:format("A: ~p, B: ~p~n", [A, B]),
    ok.
