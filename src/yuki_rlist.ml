open Lwt
open Riak

exception Empty
exception Subscript

module Make(Conn:Make.Conn)(Elem:Make.Elem) = struct
  module Client = Client.Make(Conn)(Elem)

  open Client

  let leaf conn x =
    lwt t = put conn x [] in return t.key

  let node conn x t1 t2 =
    lwt t = put conn x [t1; t2] in return t.key

  let cons conn x = function
    | (w1, t1) :: (w2, t2) :: ts' as ts ->
        if w1 = w2 then
          lwt t = node conn x t1 t2 in
          return ((1 + w1 + w2, t) :: ts')
        else
          lwt t = leaf conn x in
          return ((1, t) :: ts)
    | ts ->
        lwt t = leaf conn x in
        return ((1, t) :: ts)

  let head conn = function
    | [] -> raise Empty
    | (w, t) :: _ ->
        lwt { value = x } = get conn t in
        return x

  let tail conn = function
    | [] -> raise Empty
    | (w, t) :: ts ->
        match_lwt get conn t with
          | { links = [] } -> return ts
          | { links = [t1; t2] } -> return ((w/2, t1) :: (w/2, t2) :: ts)
          | _ -> raise Not_found

  let rec lookup_tree conn w i t = match w, i, t with
    | 1, 0, { value = x; links = [] } -> return x
    | 1, _, { links = [] } -> raise Subscript
    | _, 0, { value = x; links = [t1; t2] } -> return x
    | _, _, { links = [t1; t2] } ->
        if i <= w/2 then get conn t1 >>= lookup_tree conn (w/2) (i - 1)
        else get conn t2 >>= lookup_tree conn (w/2) (i - 1 - w/2)
    | _ -> raise Not_found

  let rec lookup conn i = function
    | [] -> raise Subscript
    | (w, t) :: ts ->
        if i < w then get conn t >>= lookup_tree conn w i
        else lookup conn (i - w) ts

  let rec update_tree conn w i y t = match w, i, t with
    | 1, 0, { links = [] } -> leaf conn y
    | 1, _, { links = [] } -> raise Subscript
    | _, 0, { links = [t1; t2] } -> node conn y t1 t2
    | _, _, { value = x; links = [t1; t2] } ->
        if i <= w/2 then
          lwt t1' = get conn t1 >>= update_tree conn (w/2) (i - 1) y in
          node conn x t1' t2
        else
          lwt t2' = get conn t2 >>= update_tree conn (w/2) (i - 1 - w/2) y in
          node conn x t1 t2'
    | _ -> raise Not_found

  let rec update conn i y = function
    | [] -> raise Subscript
    | (w, t) :: ts ->
        if i < w then
          lwt t' = get conn t >>= update_tree conn w i y in
          return ((w, t') :: ts)
        else
          lwt ts' = update conn (i - w) y ts in
          return ((w, t) :: ts')

  let rec page_tree conn w i n t =
    if n = 0 then return []
    else match w, i, t with
      | 1, 0, { value = x; links = [] } -> return [x]
      | 1, _, { links = [] } -> return []
      | _, 0, { value = x; links = [t1; t2] } ->
          if n <= w/2 then
            lwt t1' = get conn t1 >>= page_tree conn (w/2) 0 (n - 1) in
            return (x :: t1')
          else
            lwt t1' = get conn t1 >>= page_tree conn (w/2) 0 (n - 1)
            and t2' = with_elem t2 (fun conn -> page_tree conn (w/2) 0 (n - 1 - w/2)) in
            return (x :: t1' @ t2')
      | _, _, { value = x; links = [t1; t2] } ->
          if i <= w/2 then
            if i + n <= w/2 then
              get conn t1 >>= page_tree conn (w/2) (i - 1) n
            else
              lwt t1' = get conn t1 >>= page_tree conn (w/2) (i - 1) n
              and t2' = with_elem t2 (fun conn -> page_tree conn (w/2) 0 (i + n - 1 - w/2)) in
              return (t1' @ t2')
          else
            get conn t2 >>= page_tree conn (w/2) (i - 1 - w/2) n
      | _ -> raise Not_found

  let rec page conn i n = function
    | [] -> return []
    | (w, t) :: ts ->
        if i < w then
          if i + n < w then
            get conn t >>= page_tree conn w i n
          else
            lwt t1 = get conn t >>= page_tree conn w i n
            and t2 = with_connection (fun conn -> page conn 0 (i + n - w) ts) in
            return (t1 @ t2)
        else page conn (i - w) n ts
end
