-module(png).

-export([decode/1]).

-define(MAX_SAMPLE_SIZE, 10).

decode(Path) ->
    {ok, Data} = file:read_file(Path),
    <<A:8, B:8, C:8, D:8, E:8, F:8, G:8, H:8, Rest/binary>> = Data,
    io:format("Preamble: ~p, ~p, ~p, ~p, ~p, ~p, ~p, ~p~n~n", [A, B, C, D, E, F, G, H]),
    read_chunks(Rest).

read_chunks(<<>>) ->
    ok;
read_chunks(<<13:32,
              "IHDR",
              Width:32/integer,
              Height:32/integer,
              BitDepth:8/integer,
              ColorType:8/integer,
              Compression:8/integer,
              Filter:8/integer,
              Interlace:8/integer,
              _CRC:4/binary,
              Rest/binary>>) ->
    io:format("IHDR~n\tW: ~p, H: ~p~n\t"
              "Bit depth: ~p, Color type: ~p~n\t"
              "Compression Method: ~p~n\t"
              "Filter type: ~p~n\t"
              "Interlace Method: ~p~n~n",
              [Width, Height, BitDepth,
               ColorType, Compression,
               Filter, Interlace]),
    read_chunks(Rest);
read_chunks(<<ChunkLength:32/integer,
              "tEXt",
              Data:ChunkLength/binary,
              _CRC:4/binary,
              Rest/binary>>) ->
    io:format("tEXt: ~p~n", [binary:replace(Data, <<0>>, <<":">>)]),
    read_chunks(Rest);
read_chunks(<<ChunkLength:32/integer,
              "IDAT",
              Data:ChunkLength/binary,
              _CRC:4/binary,
              Rest/binary>>) ->
    io:format("IDAT:~p, ", [size(Data)]),
    read_chunks(Rest);
read_chunks(<<ChunkLength:32/integer,
              ChunkType1:8/integer, ChunkTypeRest:3/binary,
              Data:ChunkLength/binary,
              CRC:4/binary,
              Rest/binary>>) ->

    ChunkTypeFull = <<ChunkType1, ChunkTypeRest/binary>>,
    IsAncillary = ChunkType1 band 32 == 32,
    SampleSize = min(ChunkLength, ?MAX_SAMPLE_SIZE),

    <<Sample:SampleSize/binary, _/binary>> = Data,
    io:format("Chunk length: ~p~n"
              "Chunk Type: ~p~n"
              "Ancillary? ~p~n"
              "Chunk CRC: ~p~n"
              "Chunk Data Sample: ~p~n~n",
              [ChunkLength,
               ChunkTypeFull,
               IsAncillary,
               CRC,
               Sample]),
    read_chunks(Rest);
read_chunks(_Data) ->
    io:format("Unrecognized data, decode failed~n").
