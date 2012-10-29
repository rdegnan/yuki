open Lwt
open Riak
open Yuki_types

exception Empty

module Make(Conn:Make.Conn)(Elem:Make.Ord) = struct
  module Client = Client.Make(Conn)(struct
    type t = int * Elem.t * Elem.t list
    let of_string x = let (r, x, xs) = node_of_string x in (r, Elem.of_string x, List.map Elem.of_string xs)
    let to_string (r, x, xs) = string_of_node (r, Elem.to_string x, List.map Elem.to_string xs)
    let bucket = Elem.bucket
  end)

  open Client

  let node (r, x, xs, c) = put (r, x, xs) c

  let key { key = t } = t
  let rank { value = (r, _, _) } = r
  let root { value = (_, x, _) } = x

  let link' { key = t1; value = (r, x1, xs1); links = c1 } { key = t2; value = (_, x2, xs2); links = c2 } =
    if Elem.compare x1 x2 <= 0 then (r + 1, x1, xs1, t2 :: c1)
    else (r + 1, x2, xs2, t1 :: c2)
  let link t1 t2 = node (link' t1 t2)

  let skew_link' x t1 t2 =
    let (r, y, ys, c) = link' t1 t2 in
    if Elem.compare x y <= 0 then (r, x, y :: ys, c)
    else (r, y, x :: ys, c)
  let skew_link x t1 t2 = node (skew_link' x t1 t2)

  let rec ins_tree t = function
    | [] -> return [key t]
    | t' :: ts ->
        lwt t' = get t' in
        if rank t < rank t' then
          return (key t :: key t' :: ts)
        else
          lwt t = link t t' in
          ins_tree t ts

  let rec merge_trees ts1 ts2 = match ts1, ts2 with
    | _, [] -> return ts1
    | [], _ -> return ts2
    | (t1 :: ts1'), (t2 :: ts2') ->
        lwt t1' = get t1
        and t2' = get t2 in
        if rank t1' < rank t2' then
          lwt ts = merge_trees ts1' ts2 in
          return (t1 :: ts)
        else if rank t2' < rank t1' then
          lwt ts = merge_trees ts1 ts2' in
          return (t2 :: ts)
        else
          lwt t = link t1' t2'
          and ts = merge_trees ts1' ts2' in
          ins_tree t ts

  let normalize = function
    | [] -> return []
    | t :: ts ->
        lwt t' = get t in
        ins_tree t' ts

  let insert x = function
    | t1 :: t2 :: rest as ts ->
        lwt t1' = get t1
        and t2' = get t2 in
        if rank t1' = rank t2' then
          lwt t = skew_link x t1' t2' in
          return (key t :: rest)
        else
          lwt t = node (0, x, [], []) in
          return (key t :: ts)
    | ts ->
        lwt t = node (0, x, [], []) in
        return (key t :: ts)

  let merge ts1 ts2 =
    lwt ts1' = normalize ts1
    and ts2' = normalize ts2 in
    merge_trees ts1' ts2'

  let rec remove_min_tree = function
    | [] -> raise Empty
    | [t] ->
        lwt t = get t in
        return (t, [])
    | t :: ts ->
        lwt t = get t
        and t', ts' = remove_min_tree ts in
        if Elem.compare (root t) (root t') <= 0 then
          return (t, ts)
        else
          return (t', key t :: ts')

  let find_min ts =
    lwt t = remove_min_tree ts in
    return (root (fst t))

  let delete_min ts =
    lwt { value = (_, _, xs); links = ts1 }, ts2 = remove_min_tree ts in
    let rec insert_all ts = function
      | [] -> return ts
      | x :: xs' ->
          lwt ts' = insert x ts in
          insert_all ts' xs' in
    lwt ts = merge (List.rev ts1) ts2 in
    insert_all ts xs
end
