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
    seconds= 100,
    offset= 0}}

commit = {
    parents= [],
    tree= "asd",
    author= tim,
    committer= tim,
    message= "test"}

main = show (Commit commit)
