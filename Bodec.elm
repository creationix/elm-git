module Bodec (..) where
import Graphics.Element exposing (show)

import Char exposing (fromCode, toCode)
import String exposing (fromChar, uncons)
import Native.Utf8

type Bytes = Bytes (List Int)
type Raw = Raw String

{-| Uses native bindings to implement this.
The JS version is `window.unescape(encodeURIComponent(str))`

    encodeUtf8 "░:░" --> Raw "\xe2\x96\x91\x3a\xe2\x96\x91"
-}
encodeUtf8: String -> Raw
encodeUtf8 str = Raw (Native.Utf8.encode str)

{-| Uses native bindings to implement this.
The JS version is `decodeURIComponent(window.escape(raw))`

    decodeUtf8 (Raw "\xe2\x96\x91\x3a\xe2\x96\x91") --> "░:░"
-}
decodeUtf8: Raw -> String
decodeUtf8 (Raw str) = Native.Utf8.decode str

{-| Convert byte list to raw encoded string.

    toRaw (Bytes [72, 101, 108, 108, 111]) --> Raw "Hello"
-}
toRaw: Bytes -> Raw
toRaw (Bytes list) = Raw (toRaw' list)
toRaw' list = case list of
  [] -> ""
  head :: tail -> (head |> fromCode |> fromChar) ++ (toRaw' tail)

{-| Convert from raw encoded string to byte list.

    fromRaw (Raw "Hello") --> Bytes [72, 101, 108, 108, 111]
-}
fromRaw: Raw -> Bytes
fromRaw (Raw str) = Bytes (fromRaw' str)
fromRaw' str = case uncons str of
  Nothing -> []
  Just (head, tail) -> (head |> toCode) :: (fromRaw' tail)


{-| Convert from unicode string to UTF-8 encoded byte list

    toUnicode (Bytes [226, 150, 145, 58, 226, 150, 145]) --> "░:░"
-}
toUnicode: Bytes -> String
toUnicode = toRaw >> decodeUtf8

{-| Convert from UTF-8 encoded byte list to unicode string

    fromUnicode "░:░" --> Bytes [226, 150, 145, 58, 226, 150, 145]
-}
fromUnicode: String -> Bytes
fromUnicode = encodeUtf8 >> fromRaw

nibbleToHex: Int -> String
nibbleToHex num = case num of
  0 -> "0"
  1 -> "1"
  2 -> "2"
  3 -> "3"
  4 -> "4"
  5 -> "5"
  6 -> "6"
  7 -> "7"
  8 -> "8"
  9 -> "9"
  10 -> "a"
  11 -> "b"
  12 -> "c"
  13 -> "d"
  14 -> "e"
  15 -> "f"
  _ -> "-"

byteToHex: Int -> String
byteToHex num =
  (nibbleToHex (num // 16)) ++ (nibbleToHex (num % 16))

{-| Convert bytes to hex string

    toHex (Bytes [0xde, 0xad, 0xbe, 0xef]) --> "deadbeef"
-}
toHex: Bytes -> String
toHex (Bytes list) = toHex' list
toHex' list = case list of
  [] -> ""
  head :: tail -> (byteToHex head) ++ (toHex' tail)

hexToNibble: Char -> Int
hexToNibble c = case c of
  '0' -> 0
  '1' -> 1
  '2' -> 2
  '3' -> 3
  '4' -> 4
  '5' -> 5
  '6' -> 6
  '7' -> 7
  '8' -> 8
  '9' -> 9
  'a' -> 10
  'b' -> 11
  'c' -> 12
  'd' -> 13
  'e' -> 14
  'f' -> 15
  'A' -> 10
  'B' -> 11
  'C' -> 12
  'D' -> 13
  'E' -> 14
  'F' -> 15
  _ -> 0

{-| Convert hex string to byte list

    fromHex "deadbeef" -> Bytes [0xde, 0xad, 0xbe, 0xef]
-}
fromHex: String -> Bytes
fromHex str = Bytes (fromHex' str)
fromHex' str = case uncons str of
  Nothing -> []
  Just (c1, t1) -> case uncons t1 of
    Nothing -> []
    Just (c2, t2) -> ((hexToNibble c1) * 16 + (hexToNibble c2)) :: (fromHex' t2)

{-| Convert a raw string into the hex representation of it's bytes

    encodeHex (Raw "Hello") --> "48656c6c6f"
-}
encodeHex: Raw -> String
encodeHex = fromRaw >> toHex

{-| Convert a hex string to raw string

    decodeHex "48656c6c6f" --> Raw "Hello"
-}
decodeHex: String -> Raw
decodeHex = fromHex >> toRaw

main = show
  -- (fromRaw (Raw "Hello")) --> Bytes [72, 101, 108, 108, 111]
  -- (toRaw (Bytes [72, 101, 108, 108, 111])) --> Raw "Hello"
  -- (decodeUtf8 (Raw "\xe2\x96\x91\x3a\xe2\x96\x91")) --> "░:░"
  -- (encodeUtf8 "░:░") --> Raw "\xe2\x96\x91\x3a\xe2\x96\x91"
  -- (fromUnicode "░:░") --> Bytes [226, 150, 145, 58, 226, 150, 145]
  -- (toUnicode (Bytes [226, 150, 145, 58, 226, 150, 145])) --> "░:░"
  -- (toHex (Bytes [0xde, 0xad, 0xbe, 0xef])) --> "deadbeef"
  -- (fromHex "deadbeef") -->  Bytes [0xde, 0xad, 0xbe, 0xef]
  (encodeHex (Raw "Hello")) --> "48656c6c6f"
  -- (decodeHex "48656c6c6f") --> Raw "Hello"
