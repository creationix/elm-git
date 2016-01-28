module Encoder (encode, frame) where

import String exposing (padLeft)
import Regex exposing (regex)

import Bodec exposing (encodeUtf8, decodeHex)
import Git exposing (..)


safe : String -> String
safe =
    let
        pattern = regex "(?:^[.,:;<>\"']+|[\x00\n<>]+|[.,:;<>\"']+$)"
    in
        encodeUtf8
            >> Regex.replace Regex.All pattern (\_ -> "")


encodePerson : Person -> String
encodePerson person =
    (safe person.name)
        ++ " <"
        ++ (safe person.email)
        ++ "> "
        ++
        (let
            ( neg, offset ) =
                if person.offset < 0 then
                    ( " -", -person.offset )
                else
                    ( " +", person.offset )
        in
            (toString person.seconds)
                ++ neg
                ++ (padLeft 2 '0' (toString (offset // 60)))
                ++ (padLeft 2 '0' (toString (offset % 60))))



treeSort : Entry -> Entry -> Order
treeSort a b =
    compare
        (case a.mode of
          Directory -> a.name ++ "/"
          _ -> a.name)
        (case b.mode of
          Directory -> b.name ++ "/"
          _ -> b.name)

encodeEntry : Entry -> String
encodeEntry node =
    ((case node.mode of
        Directory ->
            "40000 "

        File ->
            "100644 "

        Executable ->
            "100755 "

        Symlink ->
            "120000 "

        Submodule ->
            "160000 "
     )
        ++ node.name
        ++ "\x00"
        ++ (node.hash |> fromHash |> decodeHex)
    )

encodeType t = case t of
    CommitType ->
        "commit"

    TagType ->
        "tag"

    TreeType ->
        "tree"

    BlobType ->
        "blob"

encodeCommit object =
  (object.parents
    |> List.map (\p -> "parent " ++ (fromHash p) ++ "\n")
    |> String.concat
  )
    ++ "tree " ++ (fromHash object.tree)
    ++ "\nauthor " ++ (encodePerson object.author)
    ++ "\ncommitter " ++ (encodePerson object.author)
    ++ "\n\n" ++ (encodeUtf8 object.message)

encodeTag object =
  "object " ++ (fromHash object.object)
    ++ "\ntype " ++ (encodeType object.objectType)
    ++ "\ntag " ++ object.tag
    ++ "\ntagger " ++ (encodePerson object.tagger)
    ++ "\n\n" ++ (encodeUtf8 object.message)

encodeTree =
  List.sortWith treeSort
    >> List.map encodeEntry
    >> String.concat

encode: Object -> (ObjectType, String)
encode obj = case obj of
  Commit commit -> (CommitType, encodeCommit commit)
  Tag tag -> (TagType, encodeTag tag)
  Tree tree -> (TreeType, encodeTree tree)
  Blob blob -> (BlobType, blob)

frame: (ObjectType, String) -> String
frame (t, str) =
  (encodeType t) ++ " "
    ++ (str |> String.length |> toString)
    ++ "\x00" ++ str
