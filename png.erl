-module(png).

-export([decode/1]).

-define(MAX_SAMPLE_SIZE, 10).
-define(UNKNOWN_UNIT, 0).
-define(METER, 1).

decode(Path) ->
    {ok, Data} = file:read_file(Path),
    <<A:8, B:8, C:8, D:8, E:8, F:8, G:8, H:8, Rest/binary>> = Data,
    io:format("Preamble: ~p, ~p, ~p, ~p, ~p, ~p, ~p, ~p~n~n", [A, B, C, D, E, F, G, H]),
    read_chunks(Rest).

read_chunks(<<>>) ->
    io:format("Ran out of data, missing IEND.~n~n"),
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
              "Bit depth: ~p, Color type: ~p (~s)~n\t"
              "Compression Method: ~p~n\t"
              "Filter type: ~p (~s)~n\t"
              "Interlace Method: ~p (~s)~n~n",
              [Width, Height, BitDepth,
               ColorType, color_type(ColorType),
               Compression,
               Filter, filter_type(Filter),
               Interlace, interlace_method(Interlace)]),
    read_chunks(Rest);
read_chunks(<<0:32/integer,
              "IEND",
              _CRC:4/binary>>) ->
    io:format("~nEND~n~n");
read_chunks(<<6:32/integer,
              "bKGD",
              R:16/integer,
              G:16/integer,
              B:16/integer,
              _CRC:4/binary,
              Rest/binary>>) ->
    io:format("Background colour: ~s,~s,~s~n~n",
              [integer_to_binary(R),
               integer_to_binary(G),
               integer_to_binary(B)]),
    read_chunks(Rest);
read_chunks(<<9:32/integer,
              "pHYs",
              X:32/integer,
              Y:32/integer,
              Unit:8/integer,
              _CRC:4/binary,
              Rest/binary>>) ->
    io:format("pHYs: ~s~n~n",
              [aspect(X, Y, Unit)]),
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

color_type(0) ->
    <<"Grayscale (1,2,4,8,16)">>;
color_type(2) ->
    <<"RGB triples (8,16)">>;
color_type(3) ->
    <<"Palate index (1,2,4,8)">>;
color_type(4) ->
    <<"Grayscale /w alpha (8,16)">>;
color_type(6) ->
    <<"RGB /w alpha (8,16)">>.

filter_type(0) ->
   <<"none">>;
filter_type(1) ->
   <<"sub">>;
filter_type(2) ->
   <<"up">>;
filter_type(3) ->
   <<"average">>;
filter_type(4) ->
   <<"paeth">>.

interlace_method(0) ->
    <<"None">>;
interlace_method(1) ->
    <<"Adam7">>.

aspect(X, Y, _Unit = ?UNKNOWN_UNIT) ->
    <<(integer_to_binary(X))/binary, $:, (integer_to_binary(Y))/binary>>;
aspect(X, Y, _Unit = ?METER) ->
    <<"X - ", (integer_to_binary(X))/binary, "/meter; "
      "Y - ", (integer_to_binary(Y))/binary, "/meter">>.
