%% I was reading in the Erlang/OTP system docs
%% and the examples for if and case bound variables
%% within the if/case statement. I wanted to see if the
%% compiler would complain since I seemed to remember
%% it doing so.
%% It doesn't, but erlvim does.
%% I don't know how. Even with -Wall it doesn't squeak.
%% Even
%%   dialyzer --src unsafe_bind.erl
%%     doesn't pick it up.
-module(unsafe_bind).

-export([f/1]).
-export([g/0]).
-export([try_catch/0]).

f(X) ->
    if X == 1 ->
           Y = true;
       true ->
           Y = false
    end,
    Y.

g() ->
    case 1 == 1 of
        true ->
            X = true;
        _ ->
            X = false
    end,
    X.

%% Can't bind to anything that's potentially already bound in the try-catch block.

try_catch() ->
    try 1 of
        X ->
            1
    catch
        _E:_R:_S ->
            fail
    end,
    %% won't compile, even with -W0 (no warnings)
    %% varialbe 'X' unsafe in 'try'
    %X,
    try 2 of
        %% won't compile, even with -W0 (no warnings)
        %% variable 'X' unsafe in try
        %X ->
        Y ->
            2
    catch
        _E2:_R2:_S2 ->
            fail
    end.
