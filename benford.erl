-module(benford).

-export([avg/1]).
-export([avg/2]).
-export([stats/2]).
-export([stats/3]).
-export([empty_stats/0]).
-export([inc_counts/2]).
-export([digits/1]).
-export([inc/2]).

avg(A) when A >= 0->
    avg(1,A);
avg(A) ->
    avg(A, 0).

avg(A, B) ->
    {[StatsInit | StatsRest], _} = lists:foldl(fun stats/2, {[], []}, lists:seq(min(A,B), max(A,B))),
    lists:foldl(fun avg_/2, StatsInit, StatsRest).

avg_(Stats1, Stats2) ->
    [{Digit1, (X + Y) / 2} || {Digit1, X} <- Stats1, {Digit2, Y} <- Stats2, Digit1 == Digit2].

stats(N, {Acc, Cache}) when is_list(Cache) ->
    {Stats, Cache2} = stats(N, proplists:get_value(N, Cache), Cache),
    {[Stats | Acc], Cache2}.

stats(N, undefined, Cache) ->
    Stats = lists:foldl(fun inc_counts/2, empty_stats(), lists:seq(min(0, N), max(0, N))),
    {Stats, [{N, Stats} | Cache]};
stats(_, Cached, Cache) ->
    {Cached, Cache}.

inc_counts(N, Counts) ->
    lists:foldl(fun inc/2, Counts, digits(N)).

digits(N) ->
    [list_to_integer([X]) || X <- integer_to_list(N), X /= $-].

inc(N, Counts) ->
    {Digit, Count} = lists:keyfind(N, 1, Counts),
    lists:keyreplace(N, 1, Counts, {Digit, Count + 1}).

empty_stats() ->
    [{X, 0} || X <- lists:seq(0, 9)].
