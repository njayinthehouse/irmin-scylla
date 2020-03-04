
open Lwt_result.Infix

type cassSession

type cassStatement

type cassCluster

type cassFuture

type cassError

type cassResult

type cassRow

type cassValue

type cassIterator

external ml_cass_session_new : unit -> cassSession = "cass_session_new"

external ml_cass_cluster_new : unit -> cassCluster = "cass_cluster_new"

external ml_cass_cluster_set_contact_points : cassCluster -> string -> unit
  = "cass_cluster_set_contact_points"

external ml_cass_session_connect : cassSession -> cassCluster -> cassFuture
  = "cass_session_connect"

external ml_cass_future_wait : cassFuture -> unit = "cass_future_wait"

external ml_cass_future_error_code : cassFuture -> cassError
  = "cass_future_error_code"

external ml_cass_cluster_free : cassCluster -> unit = "cass_cluster_free"

external ml_cass_session_free : cassSession -> unit = "cass_session_free"

external ml_cass_statement_new : string -> int -> cassStatement
  = "cass_statement_new"

external ml_cass_session_execute : cassSession -> cassStatement -> cassFuture
  = "cass_session_execute"

external ml_cass_future_free : cassFuture -> unit = "cass_future_free"

external ml_cass_statement_free : cassStatement -> unit = "cass_statement_free"

external ml_cass_statement_bind_string : cassStatement -> int -> string -> unit
  = "cass_statement_bind_string"

external ml_cass_future_get_result : cassFuture -> cassResult
  = "cass_future_get_result"

external ml_cass_result_first_row : cassResult -> cassRow
  = "cass_result_first_row"

external ml_cass_row_get_column : cassRow -> int -> cassValue
  = "cass_row_get_column"

external ml_cass_iterator_from_result : cassResult -> cassIterator
  = "cass_iterator_from_result"

external ml_cass_result_row_count : cassResult -> int = "cass_result_row_count"

external ml_cass_iterator_next : cassIterator -> bool = "cass_iterator_next"

external ml_cass_iterator_get_row : cassIterator -> cassRow
  = "cass_iterator_get_row"

external ml_cass_row_get_column_by_name : cassRow -> string -> cassValue
  = "cass_row_get_column_by_name"

external ml_cass_session_close : cassSession -> cassFuture
  = "cass_session_close"

external cstub_get_string : cassValue -> string = "get_string"

external cstub_convert : int -> int = "convert"

external cstub_match_enum : cassError -> cassFuture -> bool = "match_enum"

external cstub_convert_to_bool : bool -> bool = "convert_to_bool"

external cstub_convert_to_ml : int -> int = "convert_to_ml"


type t = cassSession
type error = string

type ip = string

let create_keyspace = 
  "CREATE KEYSPACE IF NOT EXISTS Irmin_scylla\
   WITH REPLICATION = { 'class' : 'SimpleStrategy', 'replication_factor' : 3 }"

let create_dir_table = 
  "CREATE TABLE IF NOT EXISTS Irmin_scylla.Dir\
   ( key text PRIMARY KEY )"

let create_file_table = 
  "CREATE TABLE IF NOT EXISTS Irmin_scylla.File\
   ( key text PRIMARY KEY, value text )"

let execute_query t query = 
  let q = ml_cass_statement_new query (cstub_convert 0) in
  let future = ml_cass_session_execute t q in
  ml_cass_future_wait future;
  let response = ml_cass_future_error_code future in
  let is_cass_ok = cstub_match_enum response future in
  if is_cass_ok then
    Lwt.return_ok future
  else
    Lwt.return_error "Not CASS_OK"
    
let execute_query_opt t query =
  let q = ml_cass_statement_new query (cstub_convert 0) in
  let future = ml_cass_session_execute t q in
  ml_cass_future_wait future;
  let response = ml_cass_future_error_code future in
  let is_cass_ok = cstub_match_enum response future in
  if is_cass_ok then
    Some future
  else 
    None

let v ip = 
  (* Create session, cluster, and connect them *)
  let session = ml_cass_session_new () in
  let cluster = ml_cass_cluster_new () in
  ml_cass_cluster_set_contact_points cluster ip;
  let future = ml_cass_session_connect session cluster in
  ml_cass_future_wait future;
  let response = ml_cass_future_error_code future in
  let is_cass_ok = cstub_match_enum response future in
  if is_cass_ok then
    execute_query session create_keyspace >>= fun _ ->
    execute_query session create_dir_table >>= fun _ ->
    execute_query session create_file_table >>= fun _ ->
    Lwt.return_ok session
  else
    Lwt.return_error "Not CASS_OK"

module File = struct
  type t = cassSession
  type error = string

  type 'a fd = (t * string * string) ref constraint 'a = [< `Read | `Write]

  let exists t path = 
    let key = Fpath.to_string path in
    let query_str = 
      "SELECT * FROM Irmin_scylla.File\
       WHERE key = '" ^ key ^ "'" in
    execute_query t query_str >>= fun future -> 
    let result = ml_cass_future_get_result future in
    let cass_row_count = ml_cass_result_row_count result in
    let row_count = cstub_convert_to_ml cass_row_count in
    Lwt.return_ok (row_count = 1)

  let delete t path = 
    let key = Fpath.to_string path in
    let query_str = 
      "DELETE FROM Irmin_scylla.File\
       WHERE key = '" ^ key ^ "'" in
    execute_query t query_str >>= fun _ ->
    Lwt.return_ok () 

  let move t path path' = 
    let key = Fpath.to_string path in 
    let key' = Fpath.to_string path' in
    let query_str = 
      "UPDATE Irmin_scylla.File\
       SET key = '" ^ key' ^ "'\
       WHERE key = '" ^ key ^ "'" in
    execute_query t query_str >>= fun _ ->
    Lwt.return_ok ()

  let open_f t path = 
    let key = Fpath.to_string path in
    let query_str = 
      "SELECT * FROM Irmin_scylla.File\
       WHERE key = '" ^ key ^ "'" in
    execute_query t query_str >>= fun future ->
    let result = ml_cass_future_get_result future in
    let cass_row_count = ml_cass_result_row_count result in
    let row_count = cstub_convert_to_ml cass_row_count in
    if row_count > 0 then
      let row = ml_cass_result_first_row result in
      let cass_value = ml_cass_row_get_column row (cstub_convert 1) in
      let value = cstub_get_string cass_value in
      Lwt.return_ok (ref (t, key, value))
    else
      Lwt.return_error "No rows retrieved"

  let open_w t path = (open_f t path : ([`Write] fd, error) result Lwt.t)

  let open_r t path = (open_f t path : ([`Read] fd, error) result Lwt.t)

  let write cs ?off ?len fd = 
    let value' = Cstruct.to_string cs in
    let (t, key, _) = !fd in
    let query_str = 
      "UPDATE Irmin_scylla.File\
       SET value = '" ^ value' ^ "'\
       WHERE key = '" ^ key ^ "'" in
    execute_query t query_str >>= fun _ ->
    begin 
      fd := (t, key, value');
      Lwt.return_ok 1
    end

  let read cs ?(off = 0) ?len fd = 
    let (_, _, value) = !fd in
    let l = (match len with
      | None -> 0
      | Some l' -> l') in
    let ret = Cstruct.of_string ?allocator:None ?off:(Some off) ?len:(Some l) value in
    Cstruct.blit ret off cs off l;
    Lwt.return_ok 1

  let close fd =
    let (t, _, _) = !fd in 
    Lwt.return_ok (fd := (t, "", ""))
end

module Mapper = struct
  type t = cassSession
  type error = string
  
  type fd = (t * string * string) ref

  let pp_error = Fmt.flush

  let openfile = File.open_f

  let length fd = failwith "Unimplemented"

  let map fd ?pos n = failwith "Unimplemented"

  let close fd = 
    let (t, _, _) = !fd in
    Lwt.return_ok (fd := (t, "", ""))
end

module Dir = struct
  type t = cassSession
  type error = string

  let exists t path = 
    let key = Fpath.to_string path in
    let query_str = 
      "SELECT * FROM Irmin_scylla.Dir\
       WHERE key = '" ^ key ^ "'" in
    execute_query t query_str >>= fun future -> 
    let result = ml_cass_future_get_result future in
    let cass_row_count = ml_cass_result_row_count result in
    let row_count = cstub_convert_to_ml cass_row_count in
    Lwt.return_ok (row_count = 1)

  let create t path = 
    let key = Fpath.to_string path in
    let query_str = 
      "INSERT INTO Irmin_scylla.Dir\
       VALUES ('" ^ key ^ "')" in
    execute_query t query_str >>= fun _ ->
    Lwt.return_ok true

  let delete t path = 
    let key = Fpath.to_string path in
    let query_str = 
      "DELETE FROM Irmin_scylla.Dir\
       WHERE key = '" ^ key ^ "'" in
    execute_query t query_str >>= fun _ ->
    Lwt.return_ok ()

  let contents t ?rel path = failwith "Unimplemented"

  let current t = failwith "Unimplemented"
end

module FS = struct
  type t = cassSession
  type error = string

  module File = File
  module Dir = Dir
  module Mapper = Mapper

  let pp_error = Fmt.flush

  let is_dir = Dir.exists
  
  let is_file = File.exists

  let has_global_watches = failwith "Wha?"

  let has_global_checkout = failwith "Wha?"
end
