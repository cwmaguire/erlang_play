-module(rec_info).

-record(a, {b, c}).

-export([fields/0]).

%% "To each module using records, a psuedo function is added during compilation to obtain information about records."
%% record_info can't take a variable because it isn't available at runtime.

%fields(RecordName) ->
    %record_info(fields, RecordName).

fields() ->
    record_info(fields, a).
