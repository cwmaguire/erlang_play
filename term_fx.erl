%% @doc
%% Screwing around with terminal effects
%% https://en.wikipedia.org/wiki/ANSI_escape_code
-module(term_fx).

-export([print/0]).
-export([error/1]).
-export([format/2]).
-export([format/3]).
-export([format/4]).

-define(I2S(X), integer_to_list(X)).

print() ->
  BGs = lists:seq(30,37),
  FGs = lists:seq(40,47),
  Fx = [1,4,7,9],
  io:format(lists:flatten([["\e[", ?I2S(BG), $;, ?I2S(FG), $;, ?I2S(E), "m The lazy brown whatever with (", ?I2S(BG), ",", ?I2S(FG), ",", ?I2S(E), ") \e[0m~n"] || BG <- BGs, FG <- FGs, E <- Fx])).

error(Error) ->
    format(Error, w, r, [bold]).

format(String, FG) ->
    format(String, FG, b).

format(String, FG, BG) ->
    format(String, FG, BG, []).

format(String, FG, BG, Effects) ->
    FxString = lists:flatten([code(Effect) || Effect <- Effects]),
    io:format("\e[" ++ ?I2S(fg(FG)) ++ ";" ++ ?I2S(bg(BG)) ++ FxString ++ "m" ++ String ++ "\e[0m~n").

fg(bk) ->
    30;
fg(r) ->
    31;
fg(g) ->
    32;
fg(y) ->
    33;
fg(b) ->
    34;
fg(m) ->
    35;
fg(c) ->
    36;
fg(w) ->
    37.

bg(Background) ->
    10 + fg(Background).

%% Trailing semi-colons will screw up the ANSI escape code
code(bold) ->
    ";1";
code(underline) ->
    ";4";
code(reverse) ->
    ";7";
code(strike) ->
    ";9".
