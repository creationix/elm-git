module Git (Hash, fromHash, toHash, sha1, EntryMode(..), Person, Entry, Object(..), ObjectType(..)) where

import Native.Sha1
import Regex exposing (regex, contains)

type Hash
    = Hash String

type ObjectType
    = CommitType
    | TagType
    | TreeType
    | BlobType


type EntryMode
    = Submodule
    | Directory
    | File
    | Symlink
    | Executable


type alias Person =
    { name : String
    , email : String
    , seconds : Int
    , offset : Int
    }


type alias Entry =
    { name : String
    , mode : EntryMode
    , hash : Hash
    }


type Object
    = Commit
        { parents : List Hash
        , tree : Hash
        , author : Person
        , committer : Person
        , message : String
        }
    | Tag
        { object : Hash
        , objectType : ObjectType
        , tag : String
        , tagger : Person
        , message : String
        }
    | Tree (List Entry)
    | Blob String


toHash : String -> Maybe Hash
toHash str =
    if Regex.contains (Regex.regex "^[0-9a-f]{40}$") str then
        Just (Hash str)
    else
        Nothing

fromHash: Hash -> String
fromHash (Hash str) = str

sha1: String -> Hash
sha1 str = Hash (Native.Sha1.sha1 str)
