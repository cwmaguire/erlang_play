-module(a_SUITE).

-export([all/0]).

-export([init_per_testcase/2]).
-export([end_per_testcase/2]).

-export([one/1]).
-export([two/1]).

all() ->
    [one,
     two].

init_per_testcase(_, Config) ->
    Config.

end_per_testcase(_, Config) ->
    ct:pal("~p:~p: Config~n\t~p~n", [?MODULE, ?FUNCTION_NAME, Config]),
    Config.

one(Config) ->
    {save_config, [{one, 1} | Config]}.

two(Config) ->
    ct:pal("~p:~p: Config~n\t~p~n", [?MODULE, ?FUNCTION_NAME, Config]),
    case proplists:get_value(saved_config, Config) of
        {one, [{one, 1} | _]} ->
            ok;
        _ ->
            ct:fail("No saved config")
    end,
    {save_config, [{two, 2} | Config]}.
