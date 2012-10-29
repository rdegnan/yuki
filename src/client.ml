open Lwt
open Riak

module Make(Conn:Make.Conn)(Elem:Make.Elem) = struct
  type t = {
    key : riak_key;
    value : Elem.t;
    vclock : riak_vclock;
    links : riak_key list;
  }

  let keys = List.map (function { Riak.key = Some key } -> key | _ -> raise Not_found)
  let links = List.map (fun key -> { riak_link_defaults with Riak.key = Some key })

  let get key = Conn.with_connection (fun conn ->
    match_lwt riak_get conn Elem.bucket key [] with
      | Some { obj_key = Some key; obj_value = Some value; obj_vclock = Some vclock; obj_links = links } ->
          return { key = key; value = Elem.of_string value; vclock = vclock; links = keys links }
      | _ -> raise Not_found
  )

  let put ?key ?v x ts = Conn.with_connection (fun conn ->
    match_lwt riak_put_raw conn Elem.bucket key ~links:(links ts) (Elem.to_string x) [Put_return_head true] v with
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

  let write key fn =
    lwt { value = x; vclock = v } = get key in
    lwt x' = fn x in
    put ~key ~v x' [] >> return ()
end
