module Main (..) where

import Graphics.Element exposing (show)
import Encoder exposing (..)
import Git exposing (..)
import Bodec exposing (fromRaw)

tim =
    { name = "Tim"
    , email = "tim@creationix.com"
    , seconds = 1453874664
    , offset = -6 * 60
    }

save = encode >> frame >> sha1

file1 = Blob "Hello World\n"
file2 = Blob "# Sample Project\n"

tree =
  Tree [
    {
      name = "greeting.txt",
      mode = File,
      hash = save file1
    },
    {
      name = "README.md",
      mode = File,
      hash = save file2
    }
  ]

commit1 =
  Commit
    { parents = []
    , tree = save tree
    , author = tim
    , committer = tim
    , message = "Initial commit\n"
    }

commit2 =
  Commit
    { parents = [save commit1]
    , tree = save tree
    , author = tim
    , committer = tim
    , message = "Second commit\n"
    }

tag =
  Tag
    { object = save commit2
    , objectType = CommitType
    , tag = "v1.0.2"
    , tagger = tim
    , message = "Test tag\n"
    }

main = file1
  |> encode
  |> frame
  |> Bodec.fromRaw
  -- |> sha1
  -- |> fromHash
  |> show
