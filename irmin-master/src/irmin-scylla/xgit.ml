module GitStore (H : Digestif.S) = struct
  module Hash = Git.Hash.Make(H)
  module Inflate = Git.Inflate
  module Deflate = Git.Deflate
  module Buffer = Cstruct_buffer
  module Value = Git.Value.Raw(Hash)(Inflate)(Deflate)
  module Reference = Git.Reference.Make(Hash)
  module PDec = Git.Unpack.Stream(Hash)(Inflate)
  module PEnc = Git.Pack.Stream(Hash)(Deflate)
  module Revidx = Map.Make(Int64)

  type kind = [`Commit | `Tree | `Blob | `Tag]
  type buffer = {ztmp: Cstruct.t; window: Inflate.window}

  let default_buffer () = failwith "Unimplemented: default_buffer"

  let buffer ?ztmp ?etmp ?dtmp ?raw ?window () = failwith "Unimplemented: buffer"

  let dotgit r = failwith "Unimplemented: dotgit"

  let root r = failwith "Unimplemented: root"

  let compression r = failwith "Unimplemented: compression"

  let contents r = failwith "Unimplemented: contents"

  let size r = failwith "Unimplemented: size"

  let read r h = failwith "Unimplemented: read"

  let read_exn r h = failwith "Unimplemented: read_exn"

  let mem r h = failwith "Unimplemented: mem"

  let list r = failwith "Unimplemented: list"

  let write r v = failwith "Unimplemented: write"

  let fold r f path acc h = failwith "Unimplemented: fold"

  let iter r kv h = failwith "Unimplemented: iter"

  (*module Pack

  module Ref*)

  let reset r = failwith "Unimplemented: reset"

  let clear_caches r = failwith "Unimplemented: clear_caches"

  let read_inflated r h = failwith "Unimplemented: read_inflated"

  let write_inflated r kind cs = failwith "Unimplemented: write_inflated"

  let has_global_watches = failwith "Uninitialized: has_global_watches"

  let has_global_checkout = failwith "Uninitialized: has_global_checkout"
end;;
