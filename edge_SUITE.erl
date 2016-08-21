-module(edge_SUITE).
-compile(export_all).

all() ->
    [component].

init_per_suite(Config) ->
    random:seed(os:timestamp()),
    Config.

component(_Config) ->
    A = random:uniform(255),
    B = random:uniform(255),
    C = random:uniform(255),
    <<A>> = edge:component(0, <<A>>),
    <<B>> = edge:component(1, <<A, B>>),
    <<C>> = edge:component(2, <<A, B, C>>),
    undefined = edge:component(2, <<0, 0>>),
    undefined = edge:component(1, <<0>>),
    undefined = edge:component(0, <<>>).
