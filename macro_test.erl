%% @doc Looks like you can output anything but you can't take in a comma
-module(macro_test).

-export([fields_macro/0]).
-export([pass_var_to_macro/0]).
-export([shadow_macro_arg/0]).
-export([block_macro_one/0]).
-export([block_macro_two/0]).
-export([block_macro_three/0]).
-export([block_macro_four/0]).
-export([comma_macro/0]).
-export([macro_punctuation/0]).
-export([finish_punctuation/0]).
-export([punctuation_var/0]).
-export([lc/0]).
-export([lc2/0]).
-export([chars/0]).

%% Simply wanted to see if there was a problem calling record_info
%% from inside a macro
-record(a, {b, c}).
-define(REC_FIELDS(Record), record_info(fields, Record)).
fields_macro() ->
  Fields = ?REC_FIELDS(a),
  io:format("Fields: ~p~n", [Fields]).


%% Any argument in a macro definition shadows any argument
%% in the scope of the resulting code. Any argument not in the macro
%% definition is written out verbatim.
%% In case of a variable name referenced in the macro, it will be
%% the same variable name in the resulting code.
-define(VAR(Var), {??Var, Var, A, B}).
pass_var_to_macro() ->
    A = [1,2,3],
    B = [4,5,6],
    {"A", [1,2,3], [1,2,3], [4,5,6]} = ?VAR(A).

-define(SHADOW(A), A).
-define(SHADOW2(B), B).
-define(DOUBLE_SHADOW(A), ?SHADOW(A)).
shadow_macro_arg() ->
    A = [1,2,3],
    B = [4,5,6],
    {[1,2,3], [1,2,3], [1,2,3], [4,5,6], [4,5,6]} = {?SHADOW(A), ?SHADOW2(A), ?DOUBLE_SHADOW(A), ?DOUBLE_SHADOW(B), B}.


%% Notice that there's nothing between ??A and "..."
-define(BLOCK(A), io:format(??A " (~p) is ~p~n", [??A, A]), A).
block_macro_one() ->
    A = [1,2,3],
    %% the io:format is performed and the resulting 'ok' is NOT thrown away
    %% (I had previously written that it was. Tested in R16B02 and 18.2)
    %% since this equates to:
    %%    io:format(...),
    %%    A
    %% i.e. two distinct operations; the macro is replaced with a string of code
    ok = ?BLOCK(A),
    {ok, [1,2,3]} = { ?BLOCK(A) }. %% <- added spaces to make the tuple more obvious

block_macro_two() ->
    A = [1,2,3],
    %% This equates to:
    %%    {io:format(...), A}
    {ok, [1,2,3]} = { ?BLOCK(A) }.

%% Notice that there's nothing between ??A and "..."
-define(BLOCK2(A), begin io:format(??A " (~p) is ~p~n", [??A, A]), A end).
block_macro_three() ->
    A = [1,2,3],
    % The last result of the begin...end block is returned as the only value
    {[1,2,3]} = {?BLOCK2(A)}.

block_macro_four() ->
    A = [1,2,3],
    % Equates to {io:format(...), A, begin io:format(...), A end}
    {ok, [1,2,3], [1,2,3]} = {?BLOCK(A), ?BLOCK2(A)}.

-define(COMMA_END(Var), Var,).
comma_macro() ->
    %1 = A = ?COMMA_END(1) 2, % 2 is never used
    {1,2} = {?COMMA_END(1) 2}.

%% No problem having the macro output partial code
-define(LC(Var, Num), [Var || Var <- lists:seq(1,Num)],).
-define(LC2(Var, Num), [Var || Var <- [lists:seq(1,Num)).
macro_punctuation() ->
    {[1,2,3,4], ok} = {?LC(A, 4) ok},
    [[1,2,3,4]] = ?LC2(A, 4) ]].

-define(l(Fun, Gen), [X Fun || X <- Gen]).
lc() ->
    [2,4,6] = ?l(* 2, [1,2,3]),
    [2,4,6] = ?l(+ X, lists:seq(1,3)).

-define(l(F1, F2, Gen), [F1 X F2 || X <- Gen]).
lc2() ->
    %[[2], [4], [6]] = ?l([, * 2], [1,2,3]),
    %[[2], [4], [6]] = ?l([, * 2, [1,2,3]),
    nope.

-define(CHARS(Chars), Chars).
-define(CHARS(Char1,Char2), Char1, Char2).
%% -define(CHARS2(Char1,Char2), Char1 Char2). %% Nope, missing comma won't compile
chars() ->
    [] = ?CHARS([]).
    %[] = ?CHARS([, ]). % nope
    %{} = ?CHARS({, }). % nope
    %[] = ?CHARS2($[, $]). nope, macro won't compile

%% NOPE (unfinished string)
%%-define(PARTIAL_STRING, "aa).
-define(HALF_PUNCTUATION, [{begin).
finish_punctuation() ->
    [{[a]}] = ?HALF_PUNCTUATION [a] end}].

-define(PUNCTUATION_VAR(PUNCTUATION), PUNCTUATION).
punctuation_var() ->
    [{[a]}] = [{begin [a ?PUNCTUATION_VAR(]end}]).
