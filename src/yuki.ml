open Lwt
open Riak

exception Empty
exception Subscript

module type STRINGABLE = sig
  type t

  val of_string : string -> t
  val to_string : t -> string
end

module type RANDOM_ACCESS_LIST =
  functor (S : STRINGABLE) -> sig
    type ra_list

    val empty : ra_list
    val is_empty : ra_list -> bool

    val cons : S.t -> ra_list -> ra_list Lwt.t
    val head : ra_list -> S.t Lwt.t
    val tail : ra_list -> ra_list Lwt.t
    (* head and tail raise Empty if list is empty *)

    val lookup : int -> ra_list -> S.t Lwt.t
    val update : int -> S.t -> ra_list -> ra_list Lwt.t
    (* lookup and update raise Subscript if index is out of bounds *)

    val page : int -> int -> ra_list -> S.t list Lwt.t
  end

let ip = "127.0.0.1"
let port = 8087
let bucket = "test"

module SkewBinaryRandomAccessList : RANDOM_ACCESS_LIST =
  functor (S : STRINGABLE) -> struct
    type ra_list = (int * string) list (* integer is the weight of the tree *)

    let pool = Lwt_pool.create 100 (fun () -> riak_connect ip port riak_connection_defaults)

    let empty = []
    let is_empty ts = ts = []

    let links = List.map (fun key -> { riak_link_defaults with key = Some key })
    let keys = List.map (function { key = Some key } -> key | _ -> raise Not_found)

    let put x ts = Lwt_pool.use pool (fun conn ->
      match_lwt riak_put_raw conn bucket None ~links:(links ts) (S.to_string x) [Put_return_head true] None with
        | Some { obj_key = Some key } -> return key
        | _ -> raise Not_found
    )

    let get key = Lwt_pool.use pool (fun conn ->
      match_lwt riak_get conn bucket key [] with
        | Some { obj_value = Some x; obj_links = links } -> return (S.of_string x, keys links)
        | _ -> raise Not_found
    )

    let leaf x = put x []
    let node x t1 t2 = put x [t1; t2]

    let cons x = function
      | (w1, t1) :: (w2, t2) :: ts' as ts ->
          if w1 = w2 then
            lwt t = node x t1 t2 in
            return ((1 + w1 + w2, t) :: ts')
          else
            lwt t = leaf x in
            return ((1, t) :: ts)
      | ts ->
          lwt t = leaf x in
          return ((1, t) :: ts)

    let head = function
      | [] -> raise Empty
      | (w, t) :: _ ->
          lwt (x, _) = get t in
          return x

    let tail = function
      | [] -> raise Empty
      | (w, t) :: ts ->
          match_lwt (get t) with
            | (_, []) -> return ts
            | (_, [t1; t2]) -> return ((w/2, t1) :: (w/2, t2) :: ts)
            | _ -> raise Not_found

    let rec lookup_tree w i t = match w, i, t with
      | 1, 0, (x, []) -> return x
      | 1, _, (_, []) -> raise Subscript
      | _, 0, (x, [t1; t2]) -> return x
      | _, _, (_, [t1; t2]) ->
          if i <= w/2 then get t1 >>= lookup_tree (w/2) (i - 1)
          else get t2 >>= lookup_tree (w/2) (i - 1 - w/2)
      | _ -> raise Not_found

    let rec lookup i = function
      | [] -> raise Subscript
      | (w, t) :: ts ->
          if i < w then get t >>= lookup_tree w i
          else lookup (i - w) ts

    let rec update_tree w i y t = match w, i, t with
      | 1, 0, (_, []) -> leaf y
      | 1, _, (_, []) -> raise Subscript
      | _, 0, (_, [t1; t2]) -> node y t1 t2
      | _, _, (x, [t1; t2]) ->
          if i <= w/2 then
            lwt t1' = get t1 >>= update_tree (w/2) (i - 1) y in
            node x t1' t2
          else
            lwt t2' = get t2 >>= update_tree (w/2) (i - 1 - w/2) y in
            node x t1 t2'
      | _ -> raise Not_found

    let rec update i y = function
      | [] -> raise Subscript
      | (w, t) :: ts ->
          if i < w then
            lwt t' = get t >>= update_tree w i y in
            return ((w, t') :: ts)
          else
            lwt ts' = update (i - w) y ts in
            return ((w, t) :: ts')

    let rec page_tree w i n t =
      if n = 0 then return []
      else match w, i, t with
        | 1, 0, (x, []) -> return [x]
        | 1, _, (_, []) -> return []
        | _, 0, (x, [t1; t2]) ->
            if n <= w/2 then
              lwt t1' = get t1 >>= page_tree (w/2) 0 (n - 1) in
              return (x :: t1')
            else
              lwt t1' = get t1 >>= page_tree (w/2) 0 (n - 1)
              and t2' = get t2 >>= page_tree (w/2) 0 (n - 1 - w/2) in
              return (x :: t1' @ t2')
        | _, _, (x, [t1; t2]) ->
            if i <= w/2 then
              if i + n <= w/2 then
                get t1 >>= page_tree (w/2) (i - 1) n
              else
                lwt t1' = get t1 >>= page_tree (w/2) (i - 1) n
                and t2' = get t2 >>= page_tree (w/2) 0 (i + n - 1 - w/2) in
                return (t1' @ t2')
            else
              get t2 >>= page_tree (w/2) (i - 1 - w/2) n
        | _ -> raise Not_found

    let rec page i n = function
      | [] -> return []
      | (w, t) :: ts ->
          if i < w then
            if i + n < w then
              get t >>= page_tree w i n
            else
              lwt t1 = get t >>= page_tree w i n
              and t2 = page 0 (i + n - w) ts in
              return (t1 @ t2)
          else page (i - w) n ts
  end
