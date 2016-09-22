%% @doc I was reading "Reinventing Man: the robot becomes reality" and got
%% inspired to model some chemical signaling. The idea is that molecules
%% are coming through the blood stream to a receptor site. The more
%% receptors that fill up the stronger the nerve signal gets. The receptor
%% only has a certain number of receptors so the strength gets maxed out
%% when the receptors all get full. The molecules only last for so long
%% before disappearing (for whatever reason) and the signal starts to
%% degrade. The rates of the molecules entering the blood stream and the
%% "average" age of the molecules can be controlled.
%%
%% c("chem_sig.erl").
%% chem_sig:run().
%% producer ! {rate, 900}.
-module(chem_sig).

-export([run/0]).

run() ->
    random:seed(os:timestamp()),

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
