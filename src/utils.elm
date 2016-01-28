mapResult : (a -> Result e b) -> List a -> Result e (List b)
mapResult fn list =
    case list of
        [] ->
            Ok []

        x :: xs ->
            case fn x of
                Err err ->
                    Err err

                Ok value ->
                    case mapResult fn xs of
                        Err err ->
                            Err err

                        Ok l ->
                            Ok (value :: l)

intToHex: Int -> String
intToHex num =
  if num < 0 then
    "-" ++ (intToHex' -num)
  else
    (intToHex' num)
intToHex' num =
  if num < 16 then
    nibbleToHex num
  else
    (intToHex' (num // 16)) ++ (nibbleToHex (num % 16))

-- (intToHex -0x76543210) --> "-76543210"
-- (intToHex -0xfdecba98) --> "-fdecba98"
