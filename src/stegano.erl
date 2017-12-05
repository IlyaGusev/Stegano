%%%-------------------------------------------------------------------
%%% @author yallen
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 05. Дек. 2017 1:12
%%%-------------------------------------------------------------------
-module(stegano).
-author("yallen").
-include_lib("stdlib/include/assert.hrl").

%% API
-export([main/0]).

read_bmp(FileName) ->
  {ok, Bin} = file:read_file(FileName),
  <<_:8, _:8, _:32/little, 0:16, 0:16, Offset:32/little, _:32/little,
    Width:32/little, Height:32/little, _:16/little, BitCount:16/little, Compression:32/little,
    _:32/little, _:32/little, _:32/little, _:32/little, _:32/little, _/binary>> = Bin,
  ?assert(BitCount =:= 24),
  ?assert(Compression =:= 0),
  BitOffset = Offset * 8,
  <<Info:BitOffset/bitstring, PixelsBin/binary>> = Bin,
  [Info, Width, Height, read_pixels(PixelsBin, 0, Width, Height)].

write_bmp(FileName, Info, PixelBytes) ->
  {ok, File} = file:open(FileName,[write]),
  file:write(File, Info),
  Bin = list_to_binary(PixelBytes),
  file:write(File, Bin).

read_pixels(PixelsBin, Index, Width, Height) when Index < Width * Height ->
  <<B:8, G:8, R:8, NextPixelsBin/binary>> = PixelsBin,
  [[B, G, R] |  read_pixels(NextPixelsBin, Index + 1, Width, Height)];
read_pixels(_, _, _, _) -> [].

insert_bits(BitsUsedCount, Text, Pixels) when bit_size(Text) >= BitsUsedCount * 3 ->
  <<BlueBits:BitsUsedCount, GreenBits: BitsUsedCount, RedBits: BitsUsedCount, CurrentText/bitstring>> = <<Text/bitstring>>,
  [FirstPixel | OtherPixels] = Pixels,
  [B|[G|[R]]] = FirstPixel,
  BitsBaseCount = 8 - BitsUsedCount,
  <<BBase:BitsBaseCount, _:BitsUsedCount>> = <<B:8>>,
  <<GBase:BitsBaseCount, _:BitsUsedCount>> = <<G:8>>,
  <<RBase:BitsBaseCount, _:BitsUsedCount>> = <<R:8>>,
  <<BNew:8/integer>> = <<BBase:BitsBaseCount, BlueBits:BitsUsedCount>>,
  <<GNew:8/integer>> = <<GBase:BitsBaseCount, GreenBits:BitsUsedCount>>,
  <<RNew:8/integer>> = <<RBase:BitsBaseCount, RedBits:BitsUsedCount>>,
  [[BNew, GNew, RNew] | insert_bits(BitsUsedCount, CurrentText, OtherPixels)];
insert_bits(_, _, Pixels) -> Pixels.

cipher(InputImgFilename, OutputImgFilename, Text, BitsUsedCount) ->
  [Info, _, _, Pixels] = read_bmp(InputImgFilename),
  TextSize = bit_size(Text),
  FlaggedText = <<TextSize:8, Text/bitstring>>,
  NewPixels = insert_bits(BitsUsedCount, FlaggedText, Pixels),
  write_bmp(OutputImgFilename, Info, lists:flatten(NewPixels)).

triples_to_bin(T) ->
  triples_to_bin(T, <<>>).

triples_to_bin([{X,Y,Z} | T], Acc) ->
  triples_to_bin(T, <<Acc/bitstring, X:2, Y:2, Z:2>>);
triples_to_bin([], Acc) ->
  Acc.

get_bits(BitsUsedCount, Pixels, Index, Width, Height) when Index < Width * Height ->
  [FirstPixel | OtherPixels] = Pixels,
  [B|[G|[R]]] = FirstPixel,
  BitsBaseCount = 8 - BitsUsedCount,
  <<_:BitsBaseCount, BUsed:BitsUsedCount>> = <<B:8>>,
  <<_:BitsBaseCount, GUsed:BitsUsedCount>> = <<G:8>>,
  <<_:BitsBaseCount, RUsed:BitsUsedCount>> = <<R:8>>,
  [{BUsed, GUsed, RUsed} | get_bits(BitsUsedCount, OtherPixels, Index + 1, Width, Height)];
get_bits(_, _, _, _, _) -> [].

decipher(ImgFilename) ->
  [_, Width, Height, Pixels] = read_bmp(ImgFilename),
  Triples = get_bits(2, Pixels, 0, Width, Height),
  Bin = triples_to_bin(Triples),
  <<Size:8, _/binary>> = Bin,
  <<_:8, Text:Size/bitstring, _/binary>> = Bin,
  Text.

main() ->
  cipher("lenna.bmp", "lenna_c.bmp", <<"Привет, мир!"/utf8>>, 2),
  decipher("lenna_c.bmp").