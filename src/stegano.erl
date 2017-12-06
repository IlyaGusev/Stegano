-module(stegano).
-author("Ilya Gusev").
-include_lib("stdlib/include/assert.hrl").

-export([cipher/3, cipher/4, decipher/2]).

cipher(InputImgFilename, Text, BitsUsedCount) ->
  ?assert(BitsUsedCount =< 8 ),
  ?assert(8 rem BitsUsedCount =:= 0),
  [Info, _, _, PixelBytes] = read_bmp(InputImgFilename),
  TextSize = bit_size(Text),
  FlaggedText = <<TextSize:32, Text/bitstring>>,
  NewPixelBytes = insert_bits(BitsUsedCount, FlaggedText, PixelBytes),
  Bin = list_to_binary(NewPixelBytes),
  <<Info/bitstring, Bin/bitstring>>.

cipher(InputImgFilename, Text, BitsUsedCount, OutputImgFilename) ->
  {ok, File} = file:open(OutputImgFilename, [write]),
  Content= cipher(InputImgFilename, Text, BitsUsedCount),
  file:write(File, Content).

decipher(ImgFilename, BitsUsedCount) ->
  ?assert(BitsUsedCount =< 8 ),
  ?assert(8 rem BitsUsedCount =:= 0),
  [_, _, _, Pixels] = read_bmp(ImgFilename),
  BitsList = get_bits(BitsUsedCount, Pixels),
  Bin = list_to_bits(BitsList, BitsUsedCount),
  <<Size:32, _/binary>> = Bin,
  <<_:32, Text:Size/bitstring, _/binary>> = Bin,
  Text.

read_bmp(FileName) ->
  {ok, Bin} = file:read_file(FileName),
  <<_:8, _:8, _:32/little, 0:16, 0:16, Offset:32/little, _:32/little,
    Width:32/little, Height:32/little, _:16/little, BitCount:16/little, Compression:32/little,
    _:32/little, _:32/little, _:32/little, _:32/little, _:32/little, _/binary>> = Bin,
  ?assert(BitCount =:= 24),
  ?assert(Compression =:= 0),
  BitOffset = Offset * 8,
  <<Info:BitOffset/bitstring, PixelsBin/bitstring>> = Bin,
  [Info, Width, Height, PixelsBin].

insert_bits(BitsUsedCount, Text, PixelBytes) when bit_size(Text) >= BitsUsedCount ->
  <<NewBits:BitsUsedCount, CurrentText/bitstring>> = <<Text/bitstring>>,
  <<FirstByte:8, OtherBytes/binary>> = PixelBytes,
  BitsBaseCount = 8 - BitsUsedCount,
  <<Base:BitsBaseCount, _:BitsUsedCount>> = <<FirstByte:8>>,
  <<NewByte:8/integer>> = <<Base:BitsBaseCount, NewBits:BitsUsedCount>>,
  [NewByte | insert_bits(BitsUsedCount, CurrentText, OtherBytes)];
insert_bits(_, _, PixelBytes) -> PixelBytes.

get_bits(BitsUsedCount, PixelBytes) when bit_size(PixelBytes) > 0 ->
  <<FirstByte:8, OtherPixels/binary>> = PixelBytes,
  BitsBaseCount = 8 - BitsUsedCount,
  <<_:BitsBaseCount, Bits:BitsUsedCount>> = <<FirstByte:8>>,
  [Bits | get_bits(BitsUsedCount, OtherPixels)];
get_bits(_, _) -> [].

list_to_bits(T, BitsUsedCount) ->
  list_to_bits(T, <<>>, BitsUsedCount).
list_to_bits([X | T], Acc, BitsUsedCount) ->
  list_to_bits(T, <<Acc/bitstring, X:BitsUsedCount>>, BitsUsedCount);
list_to_bits([], Acc, _) ->
  Acc.

%%main() ->
%%  cipher("lenna.bmp", "lenna_c.bmp", "text.txt", 2),
%%  decipher("lenna_c.bmp", "text1.txt", 2).