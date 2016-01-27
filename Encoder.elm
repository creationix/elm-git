module Encoder (..) where

import Bodec

encodeDate : GitDate -> Raw
encodeDate date =
    let
        ( neg, offset ) =
            if date.offset < 0 then
                ( "-", -date.offset )
            else
                ( "+", date.offset )
    in
        (toString date.seconds)
            ++ " "
            ++ neg
            ++ (String.padLeft 2 '0' (toString (offset // 60)))
            ++ (String.padLeft 2 '0' (toString (offset % 60)))


safe : String -> String
safe =
    let
        pattern = Regex.regex "(?:^[.,:;<>\"']+|[\x00\n<>]+|[.,:;<>\"']+$)"
    in
        Bodec.encodeUtf8
            >> Regex.replace Regex.All pattern (\_ -> "")


encodePerson : GitPerson -> String
encodePerson person =
    (safe person.name)
        ++ " <"
        ++ (safe person.email)
        ++ "> "
        ++ (encodeDate person.date)


encodeCommit : GitCommit -> Result String String
encodeCommit commit =
    if isHash commit.tree then
        Ok
            ((String.concat (List.map (\p -> "parent " ++ p ++ "\n") commit.parents))
                ++ "tree "
                ++ commit.tree
                ++ "\nauthor "
                ++ (encodePerson commit.author)
                ++ "\ncommitter "
                ++ (encodePerson commit.author)
                ++ "\n\n"
                ++ (Bodec.encodeUtf8 commit.message)
            )
    else
        Err ("invalid commit tree hash: " ++ commit.tree)


encodeTag : GitTag -> Result String String
encodeTag tag =
    if isHash tag.object then
        Ok
            ("object "
                ++ tag.object
                ++ "\ntype "
                ++ (case tag.objectType of
                        CommitType ->
                            "commit"

                        TreeType ->
                            "tree"

                        BlobType ->
                            "blob"
                   )
                ++ "\ntag "
                ++ tag.tag
                ++ "\ntagger "
                ++ (encodePerson tag.tagger)
                ++ "\n\n"
                ++ (Bodec.encodeUtf8 tag.message)
            )
    else
        Err ("Invalid object hash:" ++ tag.object)


treeSort : GitTreeNode -> GitTreeNode -> Order
treeSort a b =
    compare
        (case a.mode of
            Directory ->
                a.name ++ "/"

            _ ->
                a.name
        )
        (case b.mode of
            Directory ->
                b.name ++ "/"

            _ ->
                b.name
        )


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


encodeTreeNode : GitTreeNode -> Result String String
encodeTreeNode node =
    if isHash node.hash then
        Ok
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
                ++ (Bodec.decodeHex node.hash)
            )
    else
        Err ("Not a valid hash: " ++ node.hash)


encodeTree : GitTree -> Result String String
encodeTree =
    List.sortWith treeSort
        >> mapResult encodeTreeNode
        >> Result.map String.concat
