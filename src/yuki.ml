open Lwt
open Riak

module Make(Conn:Make.Conn)(Obj:Make.Obj) = struct
  module Client = struct
    type t = Obj.t

    let keys = List.map (function { key = Some key } -> key | _ -> raise Not_found)
    let links = List.map (fun key -> { riak_link_defaults with key = Some key })

    let get key = Lwt_pool.use Conn.pool (fun conn ->
      match_lwt riak_get conn Conn.bucket key [] with
        | Some { obj_value = Some x; obj_links = links } -> return (Obj.of_string x, keys links)
        | _ -> raise Not_found
    )

    let put x ts = Lwt_pool.use Conn.pool (fun conn ->
      match_lwt riak_put_raw conn Conn.bucket None ~links:(links ts) (Obj.to_string x) [Put_return_head true] None with
        | Some { obj_key = Some key } -> return key
        | _ -> raise Not_found
    )
  end

  module Array = struct
    module Impl = Yuki_array.Make(Client)

    let get_head conn key = match_lwt riak_get conn Conn.bucket key [] with
      | Some { obj_value = Some x; obj_vclock = v } -> return (Yuki_types.rlist_of_string x, v)
      | _ -> return ([], None)

    let put_head conn key x v =
      lwt _ = riak_put_raw conn Conn.bucket (Some key) (Yuki_types.string_of_rlist x) [] v in
      return ()

    let read_head key fn = Lwt_pool.use Conn.pool (fun conn ->
      get_head conn key >>= fn
    )

    let write_head key fn = Lwt_pool.use Conn.pool (fun conn ->
      lwt (head, vclock) = get_head conn key in
      lwt head' = fn head in
      put_head conn key head' vclock
    )

    let cons key x = write_head key (Impl.cons x)
    let head key = read_head key (fun (x, _) -> Impl.head x)
    let tail key = write_head key Impl.tail

    let lookup key i = read_head key (fun (x, _) -> Impl.lookup i x)
    let update key i y = write_head key (Impl.update i y)

    let page key i n = read_head key (fun (x, _) -> Impl.page i n x)
  end
end
