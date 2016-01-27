module Bodec where

import Char
import String

type alias ByteList = List Int

codeToString = Char.fromCode >> String.fromChar

listToRaw: ByteList -> String
listToRaw = List.map codeToString >> String.concat

decodeHex: String -> String
decodeHex = hexToList >> listToRaw

hexToList:  String -> ByteList
hexToList hex = hexToList' [] hex

-- Given a hexadecimal character return the integer value.
-- Invalid characters are treated as 0's
hexCodeToInt: Int -> Int
hexCodeToInt c =
  if c >= 0x30 && c < 0x40 then c - 0x30
  else if c > 0x60 && c <= 0x66 then c - 0x57
  else if c > 0x40 && c <= 0x46 then c - 0x37
  else 0

hexToInt: Char -> Int
hexToInt = Char.toCode >> hexCodeToInt

-- Internal recursive hexToList
-- Consumes two chars at a time parsing as hex and accumulating list.
hexToList':  ByteList -> String -> ByteList
hexToList' accum hex = case String.uncons hex of
  Nothing -> List.reverse accum
  Just (c, r) -> case String.uncons r of
      Nothing -> [] -- Invalid length, return empty list
      Just (c', r') -> hexToList' (hexToInt c * 16 + hexToInt c' :: accum) r'
