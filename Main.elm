import Graphics.Element exposing (show)

import Bodec
import String
import Char
import Regex
import List

-- import Regex exposing (replace, regex, split)
-- import List exposing (map, sortWith, reverse)
-- import String exposing (padLeft, concat, fromChar, foldl, uncons)
-- import Char exposing (fromCode)

type GitType = CommitType | TreeType | BlobType

type GitMode = Submodule | Directory | File | Symlink | Executable

type alias GitDate = {
  seconds: Int,
  offset: Int }

type alias GitPerson = {
  name: String,
  email: String,
  date: GitDate }

type alias GitCommit = {
  parents: List String,
  tree: String,
  author: GitPerson,
  committer: GitPerson,
  message: String }

type alias GitTag = {
  object: String,
  objectType: GitType,
  tag: String,
  tagger: GitPerson,
  message: String }

type alias GitTreeNode = {
  name: String,
  mode: GitMode,
  hash: String
}

type alias GitTree = List GitTreeNode

type alias GitBlob = String

isHash: String -> Bool
isHash hex =
  String.length hex == 40 &&
  String.all Char.isHexDigit hex

encodeDate: GitDate -> String
encodeDate date =
  let (neg, offset) =
    if date.offset < 0 then ("-", -date.offset)
    else ("+", date.offset)
  in (toString date.seconds) ++ " " ++ neg ++
    (String.padLeft 2 '0' (toString (offset // 60))) ++
    (String.padLeft 2 '0' (toString (offset % 60)))

-- Given a string, strip out any dangerous characters.
safe: String -> String
safe = Regex.replace Regex.All (Regex.regex "(?:^[.,:;<>\"']+|[\0\n<>]+|[.,:;<>\"']+$)") (\_ -> "")

encodePerson: GitPerson -> String
encodePerson person =
  (safe person.name) ++
  " <" ++ (safe person.email) ++ "> " ++
  (encodeDate person.date)

encodeCommit: GitCommit -> String
encodeCommit commit =
  (String.concat (List.map (\p -> "parent " ++ p ++ "\n") commit.parents)) ++
  "tree " ++ commit.tree ++
  "\nauthor " ++ (encodePerson commit.author) ++
  "\ncommitter " ++ (encodePerson commit.author) ++
  "\n\n" ++ commit.message

encodeTag: GitTag -> String
encodeTag tag =
  "object " ++ tag.object ++
  "\ntype " ++ (case tag.objectType of
    CommitType -> "commit"
    TreeType -> "tree"
    BlobType -> "blob") ++
  "\ntag " ++ tag.tag ++
  "\ntagger " ++ (encodePerson tag.tagger) ++
  "\n\n" ++ tag.message


treeSort: GitTreeNode -> GitTreeNode -> Order
treeSort a b = compare
  (case a.mode of
    Directory -> a.name ++ "/"
    _ -> a.name)
  (case b.mode of
    Directory -> b.name ++ "/"
    _ -> b.name)


mapResult: (a -> Result e b) -> List a -> Result e (List b)
mapResult fn list = case list of
  []    -> Ok []
  x::xs -> case fn x of
    Err err  -> Err err
    Ok value -> case mapResult fn xs of
      Err err -> Err err
      Ok l -> Ok (value :: l)

encodeTreeNode: GitTreeNode -> Result String String
encodeTreeNode node =
  if isHash node.hash then Ok(
    (case node.mode of
      Directory -> "40000 "
      File -> "100644 "
      Executable -> "100755 "
      Symlink -> "120000 "
      Submodule -> "160000 "
    ) ++ node.name ++ "\0" ++
    (Bodec.decodeHex node.hash)
  )
  else
    Err ("Not a valid hash: " ++ node.hash)

encodeTree: GitTree -> Result String String
encodeTree =
  List.sortWith treeSort
  >> mapResult encodeTreeNode
  >> Result.map String.concat

tim = {
  name= "Tim",
  email= "tim@creationix.com",
  date= {
    seconds= 1453874664,
    offset= -6*60}}

commit = {
  parents= [],
  tree= "9bd2ca396909383054d9e7e7e3ded6c9858d5c9c",
  author= tim,
  committer= tim,
  message= "Initial commit\n"}

tag = {
  object= "2a1d907f65f20318292200f8bc9dcb412c11a4f8",
  objectType= CommitType,
  tag= "v1.0.2",
  tagger= tim,
  message= "Test tag\n"}

tree = [
  { name= "README.md",
    mode= File,
    hash= "a9970646b5636df47a4ca888e71ba75505940cfe"
  }]


main = show (encodeTree tree)
