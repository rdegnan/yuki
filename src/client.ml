open Lwt
open Riak
open Riak_kv_piqi

module Make(Conn:Make.Conn)(Elem:Make.Elem) = struct
  type t = {
    key : riak_key;
    value : Elem.t;
    vclock : riak_vclock;
    links : riak_key list;
  }

  let keys = List.map (function { Rpb_link.key = Some key } -> key | _ -> raise Not_found)
  let links = List.map (fun key -> { Rpb_link.bucket = Some Elem.bucket; key = Some key; tag = None })

  let get key = Conn.with_connection (fun conn ->
    match_lwt riak_get conn Elem.bucket key [] with
      | Some { obj_key = Some key; obj_value = Some value; obj_vclock = Some vclock; obj_links = links } ->
          return { key = key; value = Elem.of_string value; vclock = vclock; links = keys links }
      | _ -> raise Not_found
  )

  let put ?key ?v x ts = Conn.with_connection (fun conn ->
    match_lwt riak_put_raw conn Elem.bucket key ~links:(links ts) (Elem.to_string x) [Put_return_head true; Put_if_none_match true] v with
      | Some { obj_key = Some key; obj_vclock = Some vclock } ->
          return { key = key; value = x; vclock = vclock; links = ts }
      | Some { obj_vclock = Some vclock } -> (match key with
          | Some key -> return { key = key; value = x; vclock = vclock; links = ts }
          | None -> raise Not_found)
      | _ -> raise Not_found
  )

  let read key fn =
    lwt { value = x } = get key in
    fn x

  let read_default key empty fn =
    try_lwt
      read key fn
    with Not_found ->
      fn empty

  let write key fn =
    lwt { value = x; vclock = v } = get key in
    lwt x' = fn x in
    lwt { key = key' } = put ~v x' [] in
    return key'

  let write_default key empty fn =
    try_lwt
      lwt _ = write key fn in
      return ()
    with Not_found ->
      lwt x = fn empty in
      lwt _ = put ~key x [] in
      return ()

  let write' key fn =
    lwt { value = ts; vclock = v } = get key in
    lwt (x, ts') = fn ts in
    lwt { key = key' } = put ~v ts' [] in
    return (x, key')

  let write_default' key empty fn =
    try_lwt
      lwt (x, _) = write' key fn in
      return x
  with Not_found ->
    lwt (x, ts) = fn empty in
    lwt _ = put ~key ts [] in
    return x
end
