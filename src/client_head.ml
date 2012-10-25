open Lwt
open Riak

module type S = sig
  type t

  val get_head : riak_connection -> riak_key -> (t * riak_vclock option) Lwt.t
  val put_head : riak_connection -> riak_key -> t -> riak_vclock option -> unit Lwt.t
  val with_head : riak_key -> (riak_connection -> t -> 'a Lwt.t) -> 'a Lwt.t
  val modify_head : riak_key -> (riak_connection -> t -> t Lwt.t) -> unit Lwt.t
end

module Make(Conn:Make.Conn)(Head:Make.Elem)= struct
  type t = Head.t

  let keys = List.map (function { key = Some key } -> key | _ -> raise Not_found)
  let links = List.map (fun key -> { riak_link_defaults with key = Some key })

  let with_connection fn = Lwt_pool.use Conn.pool fn

  let get_head conn key = match_lwt riak_get conn Head.bucket key [] with
    | Some { obj_value = Some x; obj_vclock = v } -> return (Head.of_string x, v)
    | _ -> raise Not_found

  let put_head conn key x v =
    lwt _ = riak_put_raw conn Head.bucket (Some key) (Head.to_string x) [] v in
    return ()

  let with_head key fn = with_connection (fun conn ->
    lwt (x, _) = get_head conn key in
    fn conn x
  )

  let modify_head key fn = with_connection (fun conn ->
    lwt (x, v) = get_head conn key in
    lwt x' = fn conn x in
    put_head conn key x' v
  )
end
