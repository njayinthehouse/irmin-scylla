
type cassSession
type cassCluster

type t = cassSession
type error = string
type ip = string

val v : ip -> (t * cassCluster, error) Lwt_result.t

module File : Git.FILE 
  with type t = t 
   and type error = string
   and type 'a fd = (t * string * string) ref

module Mapper : Git.MAPPER
  with type t = t
   and type error = string
   and type fd = (t * string * string) ref

module Dir : Git.DIR 
  with type t = t 
   and type error = string
            
module FS : Git.FS
  with type t = t
   and type error = string
   and module File = File
   and module Dir = Dir
   and module Mapper = Mapper

val closeSession : cassSession -> unit
val closeCluster : cassCluster -> unit
