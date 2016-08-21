-module(edge).

-export([edge/1]).

%% Testing
-export([component/2]).

edge([PixelLine | PixelLines]) ->
    edge([], PixelLine, [PixelLines], []).

edge([PrevLine | _], LastLine, [], EdgeLines) ->
    lists:reverse([edge_({PrevLine, LastLine, LastLine}) | EdgeLines]);
edge([], Line, [NextLine | Rest], EdgeLines) ->
    edge([Line], NextLine, Rest, [edge_({Line, Line, NextLine}) | EdgeLines]);
edge(PrevLines = [PrevLine | _], Line, [NextLine | Rest], EdgeLines) ->
    edge([Line | PrevLines], NextLine, Rest, [edge_({PrevLine, Line, NextLine}) | EdgeLines]).

edge_({<<TopPixel:4/binary, TopLineRest/binary>>,
      <<MidPixel:4/binary, MidLineRest/binary>>,
      <<BottomPixel:4/binary, BottomLineRest/binary>>}) ->
    edge_({<<TopPixel/binary, TopPixel/binary, TopLineRest/binary>>,
           <<MidPixel/binary, MidPixel/binary, MidLineRest/binary>>,
           <<BottomPixel/binary, BottomPixel/binary, BottomLineRest/binary>>},
          _EdgePixels = []).

edge_({<<TL:4/binary, TM:4/binary, TR:4/binary>>,
       <<ML:4/binary, MM:4/binary, MR:4/binary>>,
       <<BL:4/binary, BM:4/binary, BR:4/binary>>},
      EdgePixels) ->
    NextEdgePixel = edge([TL, TM, TR, ML, MM, MR, BL, BM, BR]),
    LastEdgePixel = edge([TM, TR, TR, MM, MR, MR, BM, BR, BR]),
    lists:reverse([LastEdgePixel, NextEdgePixel | EdgePixels]);
edge_({<<TL:4/binary, TM:4/binary, TR:4/binary, TopRest/binary>>,
       <<ML:4/binary, MM:4/binary, MR:4/binary, MidRest/binary>>,
       <<BL:4/binary, BM:4/binary, BR:4/binary, BottomRest/binary>>},
      EdgePixels) ->
    edge_({TopRest, MidRest, BottomRest},
          [edge__([TL, TM, TR, ML, MM, MR, BL, BM, BR]) | EdgePixels]).

edge__([TL, TM, TR, ML, MM, MR, BL, BM, BR]) ->
    Surrounding = [TL, TM, TR, ML, MR, BL, BM, BR],
    <<(math:abs(avg(red(Surrounding)) - component(0, MM)))/integer,
      (math:abs(avg(green(Surrounding)) - component(1, MM)))/integer,
      (math:abs(avg(blue(Surrounding)) - component(2, MM)))/integer,
       255/integer>>.

avg(Nums) ->
    lists:foldl(fun(X, Acc) -> X + Acc end, 0, Nums) div length(Nums).

red(Pixels) ->
    components(0, Pixels).

green(Pixels) ->
    components(1, Pixels).

blue(Pixels) ->
    components(2, Pixels).

components(Index, Pixels) ->
   [component(Index, Pixel) || Pixel <- Pixels].

component(Index, Pixel) when size(Pixel) > Index ->
    <<_:Index/binary, C:1/binary, _/binary>> = Pixel,
    C;
component(_, _) ->
    undefined.
