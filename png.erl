-module(png).

-export([decode/1]).

decode(Path) ->
    {ok, Data} = file:read_file(Path),
    <<A:8, B:8, C:8, D:8, E:8, F:8, G:8, H:8, Rest/binary>> = Data,
    io:format("~p, ~p, ~p, ~p, ~p, ~p, ~p, ~p~n", [A, B, C, D, E, F, G, H]),
    <<ChunkLength1:32/integer, Chunk1Type:4/binary, Rest2/binary>> = Rest,
    io:format("Chunk 1 length: ~p, Chunk 1 type: ~p~n", [ChunkLength1, Chunk1Type]),
    <<Chunk1Data:ChunkLength1/binary, CRC1:4/binary, _Rest3/binary>> = Rest2,
    io:format("Chunk 1 data: ~p~n", [Chunk1Data]),
    io:format("Chunk 1 CRC: ~p~n", [CRC1]).
