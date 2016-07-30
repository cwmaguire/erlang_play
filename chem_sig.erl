-module(chem_sig).

-export([run/0]).

run() ->
    random:seed(os:timestamp()),
    %% receptors
    %%   release molecules after a variable timeout
    %% molecules
    %%   random kind, random amount?
    %% nerves
    %%  constant signal? boolean? flip a switch? update a counter?
    %%  increment on signals? threshold?
    %% blood vessel
    %%   holds all the receptors

    SiteFun = site_fun(10, a, 3000),
    SitePid = spawn(SiteFun),
    register(receptor_site, SitePid),
    MoleculeProducerFun = molecule_fun(a, 1000),
    MoleculeProducerPid = spawn(MoleculeProducerFun),
    MoleculeProducerPid ! tick,
    register(producer, MoleculeProducerPid).

molecule_fun(Type0, Rate0) ->
    fun() ->
        link(whereis(receptor_site)),
        Fun0 = fun(Type, Rate, Fun) ->
                   receive
                       tick ->
                           receptor_site ! {type, Type},
                           erlang:send_after(wiggle(Rate), self(), tick),
                           Fun(Type, Rate, Fun);
                       {rate, NewRate} ->
                           io:format("new molecule rate: ~p -> ~p~n", [Rate, NewRate]),
                           erlang:send_after(wiggle(Rate), self(), tick),
                           Fun(Type, NewRate, Fun)
                   end
               end,
        Fun0(Type0, Rate0, Fun0)
    end.

wiggle(Rate) ->
    clamp(Rate + 500 - random:uniform(1000)).

clamp(X) when X < 0 ->
    0;
clamp(X) ->
    X.

site_fun(Max0, Type0, Age0) ->
    fun() ->
        Fun0 = fun(NumReceptors, Max, Type, Age, Fun) ->
                   receive
                       {type, _} when NumReceptors >= Max ->
                           io:format("site maxed~n"),
                           Fun(NumReceptors, Max, Type, Age, Fun);
                       {type, Type} ->
                           io:format("molecule arrived: strength = ~p~n", [NumReceptors + 1]),
                           receptor(Age),
                           Fun(NumReceptors + 1, Max, Type, Age, Fun);
                       molecule_left when NumReceptors > 0 ->
                           io:format("molecule left: strength = ~p~n", [NumReceptors - 1]),
                           Fun(NumReceptors - 1, Max, Type, Age, Fun);
                       {age, NewAge} ->
                           io:format("new molecule age: ~p -> ~p~n", [Age, NewAge]),
                           Fun(NumReceptors - 1, Max, Type, NewAge, Fun)
                   end
               end,
        Fun0(0, Max0, Type0, Age0, Fun0)
    end.

receptor(Age) ->
    erlang:send_after(Age + random:uniform(100), self(), molecule_left).
