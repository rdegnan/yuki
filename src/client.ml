open Lwt
open Riak

module type S = sig
  type t

  val with_connection : (riak_connection -> 'a Lwt.t) -> 'a Lwt.t

  val get : riak_connection -> riak_key -> (t * riak_key list) Lwt.t
  val put : riak_connection -> t -> riak_key list -> riak_key Lwt.t
  val with_elem : riak_key -> (riak_connection -> t * riak_key list -> 'a Lwt.t) -> 'a Lwt.t
end

module Make(Conn:Make.Conn)(Elem:Make.Elem) = struct
  type t = Elem.t

  let keys = List.map (function { key = Some key } -> key | _ -> raise Not_found)
  let links = List.map (fun key -> { riak_link_defaults with key = Some key })

  let with_connection fn = Lwt_pool.use Conn.pool fn

  let get conn key = match_lwt riak_get conn Elem.bucket key [] with
    | Some { obj_value = Some x; obj_links = links } -> return (Elem.of_string x, keys links)
    | _ -> raise Not_found

  let put conn x ts = match_lwt riak_put_raw conn Elem.bucket None ~links:(links ts) (Elem.to_string x) [Put_return_head true] None with
    | Some { obj_key = Some key } -> return key
    | _ -> raise Not_found

  let with_elem key fn = with_connection (fun conn ->
    lwt x = get conn key in
    fn conn x
  )
end






