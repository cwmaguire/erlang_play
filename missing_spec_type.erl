-module(missing_spec_type).

-export([a/1]).

-spec a(dict()) -> dict().
a(_B) -> c.

%% Trying to see if I can get away with missing types (e.g. b()) using
%% different erlc switchs (e.g. -W0 .. set warning level to zero).
%%
%% Nothing worked in R16B02 or 18.2.
