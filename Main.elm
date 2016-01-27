module Main (..) where

import Graphics.Element exposing (show)
import Bodec
import String
import Char
import Regex
import List


type Hash
    = Hash String

type GitType
    = CommitType
    | TreeType
    | BlobType


type GitMode
    = Submodule
    | Directory
    | File
    | Symlink
    | Executable


type GitDate
    = GitDate
        { seconds : Int
        , offset : Int
        }


type Email
    = Email String


type GitPerson
    = GitPerson
        { name : String
        , email : Email
        , date : GitDate
        }


type GitTreeNode
    = GitTreeNode
        { name : String
        , mode : GitMode
        , hash : Hash
        }


type GitObject
    = GitCommit
        { parents : List Hash
        , tree : Hash
        , author : GitPerson
        , committer : GitPerson
        , message : String
        }
    | GitTag
        { object : Hash
        , objectType : GitType
        , tag : String
        , tagger : GitPerson
        , message : String
        }
    | GitTree (List GitTreeNode)
    | GitBlob String


mkHash : String -> Maybe Hash
mkHash str =
  if Regex.contains (Regex.regex "^[0-9a-fA-F]{40}$") str then
    Just Hash str
  else
    Nothing


tim =
    { name = "Tim"
    , email = "tim@creationix.com"
    , date =
        { seconds = 1453874664
        , offset = -6 * 60
        }
    }


commit =
    { parents = []
    , tree = "9bd2ca396909383054d9e7e7e3ded6c9858d5c9c"
    , author = tim
    , committer = tim
    , message = "Initial commit\n"
    }


tag =
    { object = "2a1d907f65f20318292200f8bc9dcb412c11a4f8"
    , objectType = CommitType
    , tag = "v1.0.2"
    , tagger = tim
    , message = "Test tag\n"
    }


tree =
    [ { name = "README.md"
      , mode = File
      , hash = "a9970646b5636df47a4ca888e71ba75505940cfe"
      }
    ]


main =
    encodeTag tag
        |> show
