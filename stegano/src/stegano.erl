-module(stegano).
-author("Ilya Gusev").
-include_lib("stdlib/include/assert.hrl").

-export([cipher/3, cipher/4, decipher/2]).

% Шифрование в бинарные данные.
cipher(InputImgFilename, Text, BitsUsedCount) ->
  ?assert(BitsUsedCount =< 8 ),
  ?assert(8 rem BitsUsedCount =:= 0),
  [Info, _, _, PixelBytes] = read_bmp(InputImgFilename),
  TextSize = bit_size(Text),
  FlaggedText = <<TextSize:32, Text/bitstring>>,
  NewPixelBytes = insert_bits(BitsUsedCount, FlaggedText, PixelBytes),
  Bin = list_to_binary(NewPixelBytes),
  <<Info/bitstring, Bin/bitstring>>.

% Шифрование с выходом в конкретное изображение.
cipher(InputImgFilename, Text, BitsUsedCount, OutputImgFilename) ->
  {ok, File} = file:open(OutputImgFilename, [write]),
  Content= cipher(InputImgFilename, Text, BitsUsedCount),
  file:write(File, Content).

% Дешифрование с обезкой битых utf8 символов.
decipher(ImgFilename, BitsUsedCount) ->
  ?assert(BitsUsedCount =< 8 ),
  ?assert(8 rem BitsUsedCount =:= 0),
  [_, _, _, Pixels] = read_bmp(ImgFilename),
  BitsList = get_bits(BitsUsedCount, Pixels),
  Bin = list_to_bits(BitsList, BitsUsedCount),
  <<Size:32, _/binary>> = Bin,
  CroppedSize = min(bit_size(Bin)-32, Size),
  <<_:32, Text:CroppedSize/bitstring, _/binary>> = Bin,
  crop_text(Text, CroppedSize).

% Считывание метаданных и пикселей bmp.
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

% Вставка секретных битов.
insert_bits(BitsUsedCount, Text, PixelBytes) when bit_size(Text) >= BitsUsedCount, bit_size(PixelBytes) >= 8 ->
  <<NewBits:BitsUsedCount, CurrentText/bitstring>> = <<Text/bitstring>>,
  <<FirstByte:8, OtherBytes/binary>> = PixelBytes,
  BitsBaseCount = 8 - BitsUsedCount,
  <<Base:BitsBaseCount, _:BitsUsedCount>> = <<FirstByte:8>>,
  <<NewByte:8/integer>> = <<Base:BitsBaseCount, NewBits:BitsUsedCount>>,
  [NewByte | insert_bits(BitsUsedCount, CurrentText, OtherBytes)];
insert_bits(_, _, PixelBytes) -> PixelBytes.

% Получение секретных битов.
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

% Обрезка последних символов, которые в итоге не влезли в изображение
crop_text(Text, Size) ->
  NewTextSize = Size - 8,
  <<NewText: NewTextSize/bitstring, LastByte:8/bitstring>> = <<Text/bitstring>>,
  CleanText = crop_text(<<NewText/bitstring>>, <<LastByte/bitstring>>, <<>>, NewTextSize),
  CleanText.
crop_text(Text, LastByte, _, _) when LastByte < <<128>> ->
  <<Text, LastByte>>;
crop_text(Text, LastByte, LastSymbol, TextSize) when LastByte >= <<128>>, LastByte < <<192>>  ->
  NewTextSize = TextSize - 8,
  <<NewText:NewTextSize/bitstring, NewLastByte:8/bitstring>> = <<Text/bitstring>>,
  NewLastSymbol = <<LastByte/bitstring, LastSymbol/bitstring>>,
  crop_text(NewText, NewLastByte, NewLastSymbol, NewTextSize);
crop_text(Text, LastByte, LastSymbol, _) when LastByte >= <<192>>, LastByte < <<224>>, bit_size(LastSymbol) =:= 8->
  <<Text/bitstring, LastByte/bitstring, LastSymbol/bitstring>>;
crop_text(Text, LastByte, LastSymbol, _) when LastByte >= <<192>>, LastByte < <<224>>, bit_size(LastSymbol) =/= 8->
  Text;
crop_text(Text, LastByte, LastSymbol, _) when LastByte >= <<224>>, LastByte < <<240>>, bit_size(LastSymbol) =:= 16->
  <<Text/bitstring, LastByte/bitstring, LastSymbol/bitstring>>;
crop_text(Text, LastByte, LastSymbol, _) when LastByte >= <<224>>, LastByte < <<240>>, bit_size(LastSymbol) =/= 16->
  Text;
crop_text(Text, LastByte, LastSymbol, _) when LastByte >= <<240>>, bit_size(LastSymbol) =:= 24 ->
  <<Text/bitstring, LastByte/bitstring, LastSymbol/bitstring>>;
crop_text(Text, _, _, _) ->
  Text.