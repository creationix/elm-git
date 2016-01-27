import Graphics.Element exposing (show)

type GitType = CommitType | TreeType | BlobType

type GitMode = Submodule | Directory | File | Symlink | Executable

type GitObject
  = Commit {
    parents: List String,
    tree: String,
    author: {
      name: String,
      email: String,
      date: {
        seconds: Int,
        offset: Int}},
    committer: {
      name: String,
      email: String,
      date: {
        seconds: Int,
        offset: Int}},
    message: String}
  | Tag {
    object: String,
    objectType: GitType,
    tag: String,
    tagger: {
      name: String,
      email: String,
      date: {
        seconds: Int,
        offset: Int}},
    message: String}
  | Tree List {
    name: String,
    mode: GitMode,
    hash: String}
  | Blob (String)

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

main = show (Commit commit)
