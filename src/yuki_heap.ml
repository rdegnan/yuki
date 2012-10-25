open Lwt
open Riak
open Yuki_types

exception Empty
exception Subscript

module Make(Conn:Make.Conn)(Elem:Make.Ord) = struct
  module Client = Client.Make(Conn)(struct
    type t = node
    let of_string x = node_of_string x
    let to_string x = string_of_node x
    let bucket = Elem.bucket
  end)

  let rank (r, _, _, _) = r
  let root (_, x, _, _) = x

  let link conn ((r, x1, xs1, c1) as t1) ((_, x2, xs2, c2) as t2) =
    if Elem.compare x1 x2 <= 0 then
      Client.put conn (r + 1, Elem.to_string x1, xs1) (t2 :: c1)
    else
      Client.put conn (r + 1, Elem.to_string x2, xs2) (t1 :: c2)

  let skew_link x t1 t2 =
    let (r, y, ys, c) = link t1 t2 in
    if Elem.compare x y <= 0 then (r, x, y :: ys, c)
    else (r, y, x :: ys, c)

  let rec ins_tree t = function
    | [] -> [t]
    | t' :: ts ->
        if rank t < rank t' then t :: t' :: ts
        else ins_tree (link t t') ts

  let rec merge_trees ts1 ts2 = match ts1, ts2 with
    | _, [] -> ts1
    | [], _ -> ts2
    | t1 :: ts1', t2 :: ts2' ->
        if rank t1 < rank t2 then t1 :: merge_trees ts1' ts2
        else if rank t2 < rank t1 then t2 :: merge_trees ts1 ts2'
        else ins_tree (link t1 t2) (merge_trees ts1' ts2')

  let normalize = function
    | [] -> []
    | t :: ts -> ins_tree t ts

  let insert x = function
    | t1 :: t2 :: rest as ts ->
        if rank t1 = rank t2 then skew_link x t1 t2 :: rest
        else (0, x, [], []) :: ts
    | ts -> (0, x, [], []) :: ts

  let merge ts1 ts2 = merge_trees (normalize ts1) (normalize ts2)

  let rec remove_min_tree = function
    | [] -> raise Empty
    | [t] -> t, []
    | t :: ts ->
        let t', ts' = remove_min_tree ts in
        if Elem.compare (root t) (root t') <= 0 then t, ts else t', t :: ts'

  let find_min ts = root (fst (remove_min_tree ts))

  let delete_min ts =
    let (_, x, xs, ts1), ts2 = remove_min_tree ts in
      let rec insert_all ts = function
        | [] -> ts
        | x :: xs' -> insert_all (insert x ts) xs' in
    insert_all (merge (List.rev ts1) ts2) xs
end
