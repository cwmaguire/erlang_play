%% I'm getting "** exception exit: function clause" in the shell and I want
%% to make sure I can capture that.
%%
%% See: http://erlang.org/doc/reference_manual/errors.html
%%
-module(throw_catch).

-export([a/0]).
-export([a2/0]).
-export([a3/0]).
-export([a4/0]).

a() ->
    try
        %% causes {error, function_clause}
        b(1)
    catch
        E:R ->
            [E,R]
    end.

b(2) ->
    ok.

a2() ->
    try
        %% causes {error, {case_clause, 1}} to be caught
        c(1)
    catch
        E:R ->
            [E,R]
    end.

c(X) ->
    case X of
        2 -> ok
    end.

a3() ->
    try
        c(1)
    catch
        X ->
            io:format("Caught ~p~n", [X]),
            X;
        % illegal
        %X = _:_ ->
            %io:format("Caught ~p with X = _:_~n", [X]),
            %X;
        E:R ->
            io:format("Couldn't catch ~p:~p with X~n", [E, R]),
            {E, R}
    end.

a4() ->
    try
        c(1)
    catch
        % won't match
        % You can leave out:
        % - the class AND the stacktrace e.g. {case_clause, 1} ... the class is assumed to be error
        % - the stacktrace e.g. error:{case_clause, 1} ... the stacktrace is optional
        % You can't leave out just the class and still try and match the stacktrace: erlang will assume the
        % class is supposed to match the "ExceptionPattern" and that the error data is supposed to match the stacktrace pattern
        ExceptionPattern:Stk when is_tuple(ExceptionPattern) ->
            io:format("Matched exception pattern ~p and stacktrace, but not class\n~p", [ExceptionPattern, Stk]);
        ExceptionPattern:Stk ->
            io:format("Matched class ~p and exception pattern ~p", [ExceptionPattern, Stk])
        % Matches, but above always matches
        % error:{case_clause, 1}:Stk ->
        %     io:format("Matched class, exception pattern and stacktrace\n~p", [Stk])
    end.
