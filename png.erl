-module(png).

-export([decode/1,
         data/1]).

-define(MAX_SAMPLE_SIZE, 10).
-define(UNKNOWN_UNIT, 0).
-define(METER, 1).

-record(header, {width :: integer(),
                 height :: integer(),
                 bit_depth :: integer(),
                 color_type :: integer(),
                 compression :: integer(),
                 filter :: integer(),
                 interlace :: integer()}).

-record(chunk, {type :: binary(),
                data :: binary()}).

-record(png, {header :: #header{},
              background,
              physical,
              text = [] :: list(binary()),
              data = <<>> :: binary(),
              other = [] :: list(#chunk{})}).

data(#png{data = Data}) ->
    Data.

decode(Path) ->
    {ok, Data} = file:read_file(Path),
    <<A:8, B:8, C:8, D:8, E:8, F:8, G:8, H:8, Rest/binary>> = Data,
    io:format("Preamble: ~p, ~p, ~p, ~p, ~p, ~p, ~p, ~p~n~n", [A, B, C, D, E, F, G, H]),
    _Png = read_chunks(Rest, #png{}).

read_chunks(<<>>, Png = #png{text = Text, other = Other}) ->
    io:format("Ran out of data, missing IEND.~n~n"),
    Png#png{text = lists:reverse(Text),
            other = lists:reverse(Other)};
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
              Rest/binary>>,
           Png) ->
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
    Header = #header{width = Width,
                     height = Height,
                     bit_depth = BitDepth,
                     color_type = ColorType,
                     compression = Compression,
                     filter = Filter,
                     interlace = Interlace},
    read_chunks(Rest, Png#png{header = Header});
read_chunks(<<0:32/integer,
              "IEND",
              _CRC:4/binary>>,
           Png = #png{text = Text, other = Other}) ->
    io:format("~nEND~n~n"),
    Png#png{text = lists:reverse(Text),
            other = lists:reverse(Other)};
read_chunks(<<6:32/integer,
              "bKGD",
              R:16/integer,
              G:16/integer,
              B:16/integer,
              _CRC:4/binary,
              Rest/binary>>,
           Png) ->
    io:format("Background colour: ~s,~s,~s~n~n",
              [integer_to_binary(R),
               integer_to_binary(G),
               integer_to_binary(B)]),
    read_chunks(Rest, Png#png{background = {R, G, B}});
read_chunks(<<9:32/integer,
              "pHYs",
              X:32/integer,
              Y:32/integer,
              Unit:8/integer,
              _CRC:4/binary,
              Rest/binary>>,
            Png) ->
    io:format("pHYs: ~s~n~n",
              [aspect(X, Y, Unit)]),
    read_chunks(Rest, Png#png{physical = {X, Y, Unit}});
read_chunks(<<ChunkLength:32/integer,
              "tEXt",
              Data:ChunkLength/binary,
              _CRC:4/binary,
              Rest/binary>>,
            Png) ->
    io:format("tEXt: ~p~n", [binary:replace(Data, <<0>>, <<":">>)]),
    Text = [Data | Png#png.text],
    read_chunks(Rest, Png#png{text = Text});
read_chunks(<<ChunkLength:32/integer,
              "IDAT",
              Data0:ChunkLength/binary,
              _CRC:4/binary,
              Rest/binary>>,
            Png) ->
    io:format("IDAT:~p, ", [size(Data0)]),
    Data = <<(Png#png.data)/binary, Data0/binary>>,
    read_chunks(Rest, Png#png{data = Data});
read_chunks(<<ChunkLength:32/integer,
              ChunkType1:8/integer, ChunkTypeRest:3/binary,
              Data:ChunkLength/binary,
              CRC:4/binary,
              Rest/binary>>,
           Png = #png{other = Other}) ->

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
    Chunk = #chunk{type = ChunkTypeFull,
                   data = Data},
    read_chunks(Rest, Png#png{other = [Chunk | Other]});
read_chunks(_Data, Png = #png{text = Text, other = Other}) ->
    io:format("Unrecognized data, decode failed~n"),
    Png#png{text = lists:reverse(Text),
            other = lists:reverse(Other)}.

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
