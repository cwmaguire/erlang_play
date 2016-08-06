-module(png).

-export([decode/1]).

decode(Path) ->
    {ok, Data} = file:read_file(Path),
    <<A:8, B:8, C:8, D:8, E:8, F:8, G:8, H:8, Rest/binary>> = Data,
    io:format("Header: ~p, ~p, ~p, ~p, ~p, ~p, ~p, ~p~n~n", [A, B, C, D, E, F, G, H]),
    read_chunks(Rest).

read_chunks(<<>>) ->
    ok;
read_chunks(<<ChunkLength:32/integer,
              "tEXt",
              Data:ChunkLength/binary,
              _CRC:4/binary,
              Rest/binary>>) ->
    io:format("tEXt: ~p~n", [binary:replace(Data, <<0>>, <<":">>)]),
    read_chunks(Rest);
read_chunks(<<ChunkLength:32/integer,
              ChunkType1:8/integer, ChunkTypeRest:3/binary,
              Data:ChunkLength/binary,
              CRC:4/binary,
              Rest/binary>>) ->
    IsAncillary = ChunkType1 band 32 == 32,
    SampleSize = min(ChunkLength, 100),

    <<Sample:SampleSize/binary, _/binary>> = Data,
    io:format("Chunk length: ~p~n"
              "Chunk Type: ~p~n"
              "Ancillary? ~p~n"
              "Chunk CRC: ~p~n"
              "Chunk Data Sample: ~p~n~n",
              [ChunkLength,
               <<ChunkType1/integer, ChunkTypeRest/binary>>,
               IsAncillary,
               CRC,
               Sample]),
    read_chunks(Rest).
