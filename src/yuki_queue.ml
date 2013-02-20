open Lwt
open Riak
open Riak_kv_piqi
open Ag_util
open Yuki_j

exception Empty

module Make(Conn:Yuki_make.Conn)(Elem:Yuki_make.Elem) = struct
  let empty = `Shallow `Zero
  let is_empty = function `Shallow `Zero -> true | _ -> false

  let reader v lexbuf = Elem.of_string (Yojson.Safe.read_string v lexbuf)
  let writer ob x = Yojson.Safe.write_string ob (Elem.to_string x)

  let get reader key =
    Conn.with_connection (fun conn ->
      match_lwt riak_get conn Elem.bucket key [] with
        | Some { obj_key = Some key; obj_value = Some value; obj_links = links } ->
            return (Json.from_string (read_queue reader) value)
        | _ -> raise_lwt Not_found
    )

  let put writer ?key ?(ops=[Put_return_head true; Put_if_none_match true]) x =
    Conn.with_connection (fun conn ->
      match_lwt riak_put conn Elem.bucket key (Json.to_string (write_queue writer) x) ops with
        | Some { obj_key = Some key } -> return key
        | _ -> (match key with
            | Some key -> return key
            | None -> raise_lwt Not_found
        )
    )

  let rec snoc_queue : 'a .'a Json.reader -> 'a Json.writer -> 'a -> 'a queue -> 'a queue Lwt.t =
    fun reader writer y -> function
      | `Shallow `Zero -> return (`Shallow (`One y))
      | `Shallow (`One x) ->
        lwt m = put writer (`Shallow `Zero) in
        return (`Deep (`Two (x,y), m, `Zero))
      | `Deep (f, m, `Zero) -> return (`Deep (f, m, `One y))
      | `Deep (f, m, `One x) ->
        let reader' = read_pair reader and writer' = write_pair writer in
        lwt m' = get reader' m >>= snoc_queue reader' writer' (x,y) >>= put writer' in
        return (`Deep (f, m', `Zero))
      | _ -> assert false

  let snoc = snoc_queue reader writer

  let head = function
    | `Shallow `Zero -> raise_lwt Empty
    | `Shallow (`One x)
    | `Deep (`One x, _, _)
    | `Deep (`Two (x, _), _, _) -> return x
    | _ -> assert false

   let rec pop_queue : 'a. 'a Json.reader -> 'a Json.writer -> 'a queue -> ('a * 'a queue) Lwt.t =
     fun reader writer -> function
     | `Shallow `Zero -> raise_lwt Empty
     | `Shallow (`One x) -> return (x, `Shallow `Zero)
     | `Deep (`Two (x,y), m, r) -> return (x, `Deep (`One y, m, r))
     | `Deep (`One x, m, r) ->
       let reader' = read_pair reader and writer' = write_pair writer in
       (match_lwt get reader' m with
         | `Shallow `Zero -> return (x, `Shallow r)
         | m ->
           lwt ((y, z), m') = pop_queue reader' writer' m in
           lwt m' = put writer' m' in
           return (x, `Deep (`Two (y,z), m', r))
       )
     | _ -> assert false

  let pop = pop_queue reader writer
end
