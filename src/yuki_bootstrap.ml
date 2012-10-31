open Lwt
open Riak
open Yuki_j

exception Empty

module Make(Conn:Make.Conn)(Elem:Make.Ord) =
struct
  module BootstrappedElem = struct
      type t = Elem.t * heap
      let compare (x, _) (y, _) = Elem.compare x y
      let of_string x = let (x, p) = bootstrap_of_string x in (Elem.of_string x, p)
      let to_string (x, p) = string_of_bootstrap (Elem.to_string x, p)
      let bucket = Elem.bucket
  end

  module PrimH = Yuki_heap.Make(Conn)(BootstrappedElem)

  let merge ((x, p1) as h1) ((y, p2) as h2) =
    if Elem.compare x y <= 0 then
      lwt p = PrimH.insert h2 p1 in
      return (x, p)
    else
      lwt p = PrimH.insert h1 p2 in
      return (y, p)

  let insert x h = merge (x, []) h

  let find_min (x, _) = return x

  let delete_min (x, p) = match p with
    | [] ->
        return (x, None)
    | _ ->
        lwt ((y, p1), p2) = PrimH.delete_min p in
        lwt p' = PrimH.merge p1 p2 in
        return (x, Some (y, p'))
end
