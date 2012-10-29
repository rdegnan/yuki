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

  let with_connection fn = Lwt_pool.use Conn.pool fn

  let get conn key = match_lwt riak_get conn Elem.bucket key [] with
    | Some { obj_key = Some key; obj_value = Some value; obj_vclock = Some vclock; obj_links = links } ->
        return { key = key; value = Elem.of_string value; vclock = vclock; links = keys links }
    | _ -> raise Not_found

  let put conn x ts = match_lwt riak_put_raw conn Elem.bucket None ~links:(links ts) (Elem.to_string x) [Put_return_head true] None with
    | Some { obj_key = Some key; obj_vclock = Some vclock } ->
        return { key = key; value = x; vclock = vclock; links = ts }
    | _ -> raise Not_found

  let with_elem key fn = with_connection (fun conn ->
    lwt x = get conn key in
    fn conn x
  )
end
