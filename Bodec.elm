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

main = show
  -- (fromRaw (Raw "Hello")) --> Bytes [72, 101, 108, 108, 111]
  -- (toRaw (Bytes [72, 101, 108, 108, 111])) --> Raw "Hello"
  -- (decodeUtf8 (Raw "\xe2\x96\x91\x3a\xe2\x96\x91")) --> "░:░"
  -- (encodeUtf8 "░:░") --> Raw "\xe2\x96\x91\x3a\xe2\x96\x91"
  -- (fromUnicode "░:░") --> Bytes [226, 150, 145, 58, 226, 150, 145]
  (toUnicode (Bytes [226, 150, 145, 58, 226, 150, 145])) --> "░:░"

--
-- encodeHex:
-- {-| Convert between a byte list and a raw encoded string
--     raw = bytesToRaw
-- -}
-- bytesToRaw : Bytes -> Raw
-- bytesToRaw = Raw (
--     List.map (fromCode >> fromChar) >> String.concat)
--
-- decodeHex : String -> Raw
-- decodeHex =
--     hexToList >> listToRaw
--
--
--
-- -- Given a hexadecimal character return the integer value.
-- -- Invalid characters are treated as 0's
--
--
-- hexCodeToInt : Int -> Int
-- hexCodeToInt c =
--     if c >= 48 && c < 64 then
--         c - 48
--     else if c > 96 && c <= 102 then
--         c - 87
--     else if c > 64 && c <= 70 then
--         c - 55
--     else
--         0
--
--
-- hexToInt : Char -> Int
-- hexToInt =
--     Char.toCode >> hexCodeToInt
--
--
-- hexToList : String -> ByteList
-- hexToList hex =
--     case String.uncons hex of
--         Nothing ->
--             []
--
--         Just ( c, r ) ->
--             case String.uncons r of
--                 Nothing ->
--                     []
--
--                 Just ( c', r' ) ->
--                     (hexToInt c * 16 + hexToInt c') :: (hexToList r')
