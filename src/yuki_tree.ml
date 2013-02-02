open Lwt
open Riak
open Riak_kv_piqi
open Ag_util
open Yuki_tree_j

exception Empty

module Make(Conn:Yuki_make.Conn)(Elem:Yuki_make.Elem)(M:Yuki_make.Monoid) = struct
  let empty = `Nil
  let is_empty = function `Nil -> true | _ -> false

  let reader v lexbuf = Elem.of_string (Yojson.Safe.read_string v lexbuf)
  let writer ob x = Yojson.Safe.write_string ob (Elem.to_string x)

  let get reader key =
    Conn.with_connection (fun conn ->
      match_lwt riak_get conn Elem.bucket key [] with
        | Some { obj_value = Some value } -> return (Json.from_string (read_fg reader) value)
        | _ -> raise Not_found
    )

  let get_opt reader = function
    | None -> return `Nil
    | Some m -> get reader m

  let put writer ?key ?(ops=[Put_return_head true; Put_if_none_match true]) x =
    Conn.with_connection (fun conn ->
      match_lwt riak_put conn Elem.bucket key (Json.to_string (write_fg writer) x) ops with
        | Some { obj_key = Some key } -> return key
        | _ -> (match key with
            | Some key -> return key
            | None -> raise Not_found
        )
    )

  let put_opt writer = function
    | `Nil -> return None
    | m ->
      lwt m' = put writer m in
      return (Some m')

  (*---------------------------------*)
  (*              fold               *)
  (*---------------------------------*)
  let fold_right_node : 'acc 'a. ('acc -> 'a -> 'acc Lwt.t) -> 'acc -> 'a node -> 'acc Lwt.t = fun f acc -> function
    | `Node2 (_, a, b) ->
      lwt acc = f acc b in
      f acc a
    | `Node3 (_, a, b, c) ->
      lwt acc = f acc c in
      lwt acc = f acc b in
      f acc a
  let fold_left_node : 'acc 'a. ('acc -> 'a -> 'acc Lwt.t) -> 'acc -> 'a node -> 'acc Lwt.t = fun f acc -> function
    | `Node2 (_, a, b) ->
      lwt acc = f acc a in
      f acc b
    | `Node3 (_, a, b, c) ->
      lwt acc = f acc a in
      lwt acc = f acc b in
      f acc c

  let fold_right_digit : 'acc 'a. ('acc -> 'a -> 'acc Lwt.t) -> 'acc -> 'a digit -> 'acc Lwt.t = fun f acc -> function
    | `One (_, a) -> f acc a
    | `Two (_, a, b) ->
      lwt acc = f acc b in
      f acc a
    | `Three (_, a, b, c) ->
      lwt acc = f acc c in
      lwt acc = f acc b in
      f acc a
    | `Four (_, a, b, c, d) ->
      lwt acc = f acc d in
      lwt acc = f acc c in
      lwt acc = f acc b in
      f acc a
  let fold_left_digit : 'acc 'a. ('acc -> 'a -> 'acc Lwt.t) -> 'acc -> 'a digit -> 'acc Lwt.t = fun f acc -> function
    | `One (_, a) -> f acc a
    | `Two (_, a, b) ->
      lwt acc = f acc a in
      f acc b
    | `Three (_, a, b, c) ->
      lwt acc = f acc a in
      lwt acc = f acc b in
      f acc c
    | `Four (_, a, b, c, d) ->
      lwt acc = f acc a in
      lwt acc = f acc b in
      lwt acc = f acc c in
      f acc d

  let rec fold_right : 'acc 'a. 'a Json.reader -> ('acc -> 'a -> 'acc Lwt.t) -> 'acc -> 'a fg -> 'acc Lwt.t = fun reader f acc -> function
    | `Nil -> return acc
    | `Single x -> f acc x
    | `Deep (_, pr, m, sf) ->
      let reader' = read_node reader in
      lwt acc = fold_right_digit f acc sf
      and m' = get_opt reader' m in
      lwt acc = fold_right reader' (fun acc elt -> fold_right_node f acc elt) acc m' in
      fold_right_digit f acc pr
  let rec fold_left : 'acc 'a. 'a Json.reader -> ('acc -> 'a -> 'acc Lwt.t) -> 'acc -> 'a fg -> 'acc Lwt.t = fun reader f acc -> function
    | `Nil -> return acc
    | `Single x -> f acc x
    | `Deep (_, pr, m, sf) ->
      let reader' = read_node reader in
      lwt acc = fold_left_digit f acc pr
      and m' = get_opt reader' m in
      lwt acc = fold_left reader' (fun acc elt -> fold_left_node f acc elt) acc m' in
      fold_left_digit f acc sf

  (*---------------------------------*)
  (*     measurement functions       *)
  (*---------------------------------*)
  let measure_node : 'a. 'a node -> M.t = function
    | `Node2 (v, _, _)
    | `Node3 (v, _, _, _) -> M.of_string v

  let measure_digit : 'a. 'a digit -> M.t = function
    | `One (v, _)
    | `Two (v, _, _)
    | `Three (v, _, _, _)
    | `Four (v, _, _, _, _) -> M.of_string v

  let measure_t_node : 'a. 'a node fg -> M.t = function
    | `Nil -> M.zero
    | `Single x -> measure_node x
    | `Deep (v, _, _, _) -> M.of_string v
  let measure_t : 'a. measure:('a -> M.t) -> 'a fg -> M.t = fun ~measure -> function
    | `Nil -> M.zero
    | `Single x -> measure x
    | `Deep (v, _, _, _) -> M.of_string v

  (*---------------------------------*)
  (*  a bunch of smart constructors  *)
  (*---------------------------------*)
  let node2_node : 'a. 'a node -> 'a node -> 'a node node = fun a b ->
    `Node2 (M.to_string (M.combine (measure_node a) (measure_node b)), a, b)
  let node2 : 'a. measure:('a -> M.t) -> 'a -> 'a -> 'a node = fun ~measure a b ->
    `Node2 (M.to_string (M.combine (measure a) (measure b)), a, b)

  let node3_node : 'a. 'a node -> 'a node -> 'a node -> 'a node node = fun a b c ->
    `Node3 (M.to_string (M.combine (measure_node a) (M.combine (measure_node b) (measure_node c))), a, b, c)
  let node3 : 'a. measure:('a -> M.t) -> 'a -> 'a -> 'a -> 'a node = fun ~measure a b c ->
    `Node3 (M.to_string (M.combine (measure a) (M.combine (measure b) (measure c))), a, b, c)

  let deep : 'a. 'a node Json.writer -> 'a digit -> 'a node fg -> 'a digit -> 'a fg Lwt.t = fun writer pr m sf ->
    lwt m' = put_opt writer m in
    return (`Deep (M.to_string (M.combine (M.combine (measure_digit pr) (measure_t_node m)) (measure_digit sf)), pr, m', sf))

  let one_node : 'a. 'a node -> 'a node digit = fun a ->
    `One (M.to_string (measure_node a), a)
  let one : 'a. measure:('a -> M.t) -> 'a -> 'a digit = fun ~measure a ->
    `One (M.to_string (measure a), a)

  let two_node : 'a. 'a node -> 'a node -> 'a node digit = fun a b ->
    `Two (M.to_string (M.combine (measure_node a) (measure_node b)), a, b)
  let two : 'a. measure:('a -> M.t) -> 'a -> 'a -> 'a digit = fun ~measure a b ->
    `Two (M.to_string (M.combine (measure a) (measure b)), a, b)

  let three_node : 'a. 'a node -> 'a node -> 'a node -> 'a node digit = fun a b c ->
    `Three (M.to_string (M.combine (M.combine (measure_node a) (measure_node b)) (measure_node c)), a, b, c)
  let three : 'a. measure:('a -> M.t) -> 'a -> 'a -> 'a -> 'a digit = fun ~measure a b c ->
    `Three (M.to_string (M.combine (M.combine (measure a) (measure b)) (measure c)), a, b, c)

  let four_node : 'a. 'a node -> 'a node -> 'a node -> 'a node -> 'a node digit = fun a b c d ->
    `Four (M.to_string (M.combine (M.combine (measure_node a) (measure_node b)) (M.combine (measure_node c) (measure_node d))), a, b, c, d)
  let four : 'a. measure:('a -> M.t) -> 'a -> 'a -> 'a -> 'a -> 'a digit = fun ~measure a b c d ->
    `Four (M.to_string (M.combine (M.combine (measure a) (measure b)) (M.combine (measure c) (measure d))), a, b, c, d)

  (*---------------------------------*)
  (*          cons / snoc            *)
  (*---------------------------------*)
  let cons_digit_node : 'a. 'a node digit -> 'a node -> 'a node digit = fun d x ->
    match d with
    | `One (v, a) -> `Two (M.to_string (M.combine (measure_node x) (M.of_string v)), x, a)
    | `Two (v, a, b) -> `Three (M.to_string (M.combine (measure_node x) (M.of_string v)), x, a, b)
    | `Three (v, a, b, c) -> `Four (M.to_string (M.combine (measure_node x) (M.of_string v)), x, a, b, c)
    | `Four _ -> assert false
  let cons_digit : 'a. measure:('a -> M.t) -> 'a digit -> 'a -> 'a digit = fun ~measure d x ->
    match d with
    | `One (v, a) -> `Two (M.to_string (M.combine (measure x) (M.of_string v)), x, a)
    | `Two (v, a, b) -> `Three (M.to_string (M.combine (measure x) (M.of_string v)), x, a, b)
    | `Three (v, a, b, c) -> `Four (M.to_string (M.combine (measure x) (M.of_string v)), x, a, b, c)
    | `Four _ -> assert false

  let snoc_digit_node : 'a. 'a node digit -> 'a node -> 'a node digit = fun d x ->
    match d with
    | `One (v, a) -> `Two (M.to_string (M.combine (M.of_string v) (measure_node x)), a, x)
    | `Two (v, a, b) -> `Three (M.to_string (M.combine (M.of_string v) (measure_node x)), a, b, x)
    | `Three (v, a, b, c) -> `Four (M.to_string (M.combine (M.of_string v) (measure_node x)), a, b, c, x)
    | `Four _ -> assert false
  let snoc_digit : 'a. measure:('a -> M.t) -> 'a digit -> 'a -> 'a digit = fun ~measure d x ->
    match d with
    | `One (v, a) -> `Two (M.to_string (M.combine (M.of_string v) (measure x)), a, x)
    | `Two (v, a, b) -> `Three (M.to_string (M.combine (M.of_string v) (measure x)), a, b, x)
    | `Three (v, a, b, c) -> `Four (M.to_string (M.combine (M.of_string v) (measure x)), a, b, c, x)
    | `Four _ -> assert false

  let rec cons_aux : 'a. 'a node Json.reader -> 'a node Json.writer -> 'a node fg -> 'a node -> 'a node fg Lwt.t = fun reader writer t a ->
    match t with
    | `Nil ->
      return (`Single a)
    | `Single b ->
      deep (write_node writer) (one_node a) `Nil (one_node b)
    | `Deep (_, `Four (_, b, c, d, e), m, sf) ->
      let reader' = read_node reader and writer' = write_node writer in
      lwt m' = get_opt reader' m in
      lwt m'' = cons_aux reader' writer' m' (node3_node c d e) in
      deep writer' (two_node a b) m'' sf
    | `Deep (v, pr, m, sf) ->
      return (`Deep (M.to_string (M.combine (measure_node a) (M.of_string v)), cons_digit_node pr a, m, sf))
  let cons : 'a. measure:('a -> M.t) -> 'a Json.reader -> 'a Json.writer -> 'a fg -> 'a -> 'a fg Lwt.t = fun ~measure reader writer t a ->
    match t with
    | `Nil ->
      return (`Single a)
    | `Single b ->
      deep (write_node writer) (one ~measure a) `Nil (one ~measure b)
    | `Deep (_, `Four (_, b, c, d, e), m, sf) ->
      let reader' = read_node reader and writer' = write_node writer in
      lwt m' = get_opt reader' m in
      lwt m'' = cons_aux reader' writer' m' (node3 ~measure c d e) in
      deep writer' (two ~measure a b) m'' sf
    | `Deep (v, pr, m, sf) ->
      return (`Deep (M.to_string (M.combine (measure a) (M.of_string v)), cons_digit ~measure pr a, m, sf))

  let rec snoc_aux : 'a. 'a node Json.reader -> 'a node Json.writer -> 'a node fg -> 'a node -> 'a node fg Lwt.t = fun reader writer t a ->
    match t with
    | `Nil ->
      return (`Single a)
    | `Single b ->
      deep (write_node writer) (one_node b) `Nil (one_node a)
    | `Deep (_, pr, m, `Four (_, b, c, d, e)) ->
      let reader' = read_node reader and writer' = write_node writer in
      lwt m' = get_opt reader' m in
      lwt m'' = snoc_aux reader' writer' m' (node3_node b c d) in
      deep writer' pr m'' (two_node e a)
    | `Deep (v, pr, m, sf) ->
      return (`Deep (M.to_string (M.combine (M.of_string v) (measure_node a)), pr, m, snoc_digit_node sf a))
  let snoc : 'a. measure:('a -> M.t) -> 'a Json.reader -> 'a Json.writer -> 'a fg -> 'a -> 'a fg Lwt.t = fun ~measure reader writer t a ->
    match t with
    | `Nil ->
      return (`Single a)
    | `Single b ->
      deep (write_node writer) (one ~measure b) `Nil (one ~measure a)
    | `Deep (_, pr, m, `Four (_, b, c, d, e)) ->
      let reader' = read_node reader and writer' = write_node writer in
      lwt m' = get_opt reader' m in
      lwt m'' = snoc_aux reader' writer' m' (node3 ~measure b c d) in
      deep writer' pr m'' (two ~measure e a)
    | `Deep (v, pr, m, sf) ->
      return (`Deep (M.to_string (M.combine (M.of_string v) (measure a)), pr, m, snoc_digit ~measure sf a))

  (*---------------------------------*)
  (*     various conversions         *)
  (*---------------------------------*)
  let to_tree_digit_node : 'a. 'a node digit -> 'a node fg = fun d ->
    match d with
    | `One (_, a) -> `Single a
    | `Two (v, a, b) -> `Deep (v, one_node a, None, one_node b)
    | `Three (v, a, b, c) -> `Deep (v, two_node a b, None, one_node c)
    | `Four (v, a, b, c, d) -> `Deep (v, three_node a b c, None, one_node d)
  let to_tree_digit : 'a. measure:('a -> M.t) -> 'a digit -> 'a fg = fun ~measure d ->
    match d with
    | `One (_, a) -> `Single a
    | `Two (v, a, b) -> `Deep (v, one ~measure a, None, one ~measure b)
    | `Three (v, a, b, c) -> `Deep (v, two ~measure a b, None, one ~measure c)
    | `Four (v, a, b, c, d) -> `Deep (v, three ~measure a b c, None, one ~measure d)
  let to_tree_list ~measure = function
    | [] -> `Nil
    | [a] -> `Single a
    | [a; b] ->
      let m_pr = measure a and m_sf = measure b in
      `Deep (M.to_string (M.combine m_pr m_sf), `One (M.to_string m_pr, a), None, `One (M.to_string m_sf, b))
    | [a; b; c] ->
      let m_pr = M.combine (measure a) (measure b) and m_sf = measure c in
      `Deep (M.to_string (M.combine m_pr m_sf), `Two (M.to_string m_pr, a, b), None, `One (M.to_string m_sf, c))
    | [a; b; c; d] ->
      let m_pr = M.combine (M.combine (measure a) (measure b)) (measure c) and m_sf = measure d in
      `Deep (M.to_string (M.combine m_pr m_sf), `Three (M.to_string m_pr, a, b, c), None, `One (M.to_string m_sf, d))
    | _ -> assert false

  let to_digit_node : 'a. 'a node -> 'a digit = function
    | `Node2 (v, a, b) -> `Two (v, a, b)
    | `Node3 (v, a, b, c) -> `Three (v, a, b, c)
  let to_digit_list : 'a. measure:('a -> M.t) -> 'a list -> 'a digit = fun ~measure -> function
    | [a] -> one ~measure a
    | [a; b] -> two ~measure a b
    | [a; b; c] -> three ~measure a b c
    | [a; b; c; d] -> four ~measure a b c d
    | _ -> assert false
  let to_digit_list_node : 'a. 'a node list -> 'a node digit = function
    | [a] -> one_node a
    | [a; b] -> two_node a b
    | [a; b; c] -> three_node a b c
    | [a; b; c; d] -> four_node a b c d
    | _ -> assert false

  (*---------------------------------*)
  (*     front / rear / etc.         *)
  (*---------------------------------*)
  let head_digit : 'a. 'a digit -> 'a = function
    | `One (_, a)
    | `Two (_, a, _)
    | `Three (_, a, _, _)
    | `Four (_, a, _, _, _) -> a
  let last_digit : 'a. 'a digit -> 'a = function
    | `One (_, a)
    | `Two (_, _, a)
    | `Three (_, _, _, a)
    | `Four (_, _, _, _, a) -> a
  let tail_digit_node : 'a. 'a node digit -> 'a node digit = function
    | `One _ -> assert false
    | `Two (_, _, a) -> one_node a
    | `Three (_, _, a, b) -> two_node a b
    | `Four (_, _, a, b, c) -> three_node a b c
  let tail_digit : 'a. measure:('a -> M.t) -> 'a digit -> 'a digit = fun ~measure -> function
    | `One _ -> assert false
    | `Two (_, _, a) -> one ~measure a
    | `Three (_, _, a, b) -> two ~measure a b
    | `Four (_, _, a, b, c) -> three ~measure a b c
  let init_digit_node : 'a. 'a node digit -> 'a node digit = function
    | `One _ -> assert false
    | `Two (_, a, _) -> one_node a
    | `Three (_, a, b, _) -> two_node a b
    | `Four (_, a, b, c, _) -> three_node a b c
  let init_digit : 'a. measure:('a -> M.t) -> 'a digit -> 'a digit = fun ~measure -> function
    | `One _ -> assert false
    | `Two (_, a, _) -> one ~measure a
    | `Three (_, a, b, _) -> two ~measure a b
    | `Four (_, a, b, c, _) -> three ~measure a b c

  type 'a view =
    | Vnil
    | Vcons of 'a * 'a fg

  let rec view_left_aux : 'a. 'a node Json.reader -> 'a node Json.writer -> 'a node fg -> 'a node view Lwt.t = fun reader writer -> function
    | `Nil -> return Vnil
    | `Single x -> return (Vcons (x, `Nil))
    | `Deep (_, `One (_, a), m, sf) ->
      let reader' = read_node reader and writer' = write_node writer in
      lwt m' = get_opt reader' m in
      lwt m'' = view_left_aux reader' writer' m' in
      lwt vcons =
        match m'' with
        | Vnil -> return (to_tree_digit_node sf)
        | Vcons (a, m') -> deep writer' (to_digit_node a) m' sf in
      return (Vcons (a, vcons))
    | `Deep (_, pr, m, sf) ->
      let reader' = read_node reader and writer' = write_node writer in
      lwt m' = get_opt reader' m in
      lwt vcons = deep writer' (tail_digit_node pr) m' sf in
      return (Vcons (head_digit pr, vcons))
  let view_left : 'a. measure:('a -> M.t) -> 'a Json.reader -> 'a Json.writer -> 'a fg -> 'a view Lwt.t = fun ~measure reader writer -> function
    | `Nil -> return Vnil
    | `Single x -> return (Vcons (x, `Nil))
    | `Deep (_, `One (_, a), m, sf) ->
      let reader' = read_node reader and writer' = write_node writer in
      lwt m' = get_opt reader' m in
      lwt m'' = view_left_aux reader' writer' m' in
      lwt vcons =
        match m'' with
        | Vnil -> return (to_tree_digit ~measure sf)
        | Vcons (a, m') -> deep writer' (to_digit_node a) m' sf in
      return (Vcons (a, vcons))
    | `Deep (_, pr, m, sf) ->
      let reader' = read_node reader and writer' = write_node writer in
      lwt m' = get_opt reader' m in
      lwt vcons = deep writer' (tail_digit ~measure pr) m' sf in
      return (Vcons (head_digit pr, vcons))

  let rec view_right_aux : 'a. 'a node Json.reader -> 'a node Json.writer -> 'a node fg -> 'a node view Lwt.t = fun reader writer -> function
    | `Nil -> return Vnil
    | `Single x -> return (Vcons (x, `Nil))
    | `Deep (_, pr, m, `One (_, a)) ->
      let reader' = read_node reader and writer' = write_node writer in
      lwt m' = get_opt reader' m in
      lwt m'' = view_right_aux reader' writer' m' in
      lwt vcons =
        match m'' with
        | Vnil -> return (to_tree_digit_node pr)
        | Vcons (a, m') -> deep writer' pr m' (to_digit_node a) in
      return (Vcons (a, vcons))
    | `Deep (_, pr, m, sf) ->
      let reader' = read_node reader and writer' = write_node writer in
      lwt m' = get_opt reader' m in
      lwt vcons = deep writer' pr m' (init_digit_node sf) in
      return (Vcons (last_digit sf, vcons))
  let view_right : 'a. measure:('a -> M.t) -> 'a Json.reader -> 'a Json.writer -> 'a fg -> 'a view Lwt.t = fun ~measure reader writer -> function
    | `Nil -> return Vnil
    | `Single x -> return (Vcons (x, `Nil))
    | `Deep (_, pr, m, `One (_, a)) ->
      let reader' = read_node reader and writer' = write_node writer in
      lwt m' = get_opt reader' m in
      lwt m'' = view_right_aux reader' writer' m' in
      lwt vcons =
        match m'' with
        | Vnil -> return (to_tree_digit ~measure pr)
        | Vcons (a, m') -> deep writer' pr m' (to_digit_node a) in
      return (Vcons (a, vcons))
    | `Deep (_, pr, m, sf) ->
      let reader' = read_node reader and writer' = write_node writer in
      lwt m' = get_opt reader' m in
      lwt vcons = deep writer' pr m' (init_digit ~measure sf) in
      return (Vcons (last_digit sf, vcons))

  let head = function
    | `Nil -> raise Empty
    | `Single a -> a
    | `Deep (_, pr, _, _) -> head_digit pr

  let last = function
    | `Nil -> raise Empty
    | `Single a -> a
    | `Deep (_, _, _, sf) -> last_digit sf

  let tail ~measure t =
    match_lwt view_left ~measure reader writer t with
    | Vnil -> raise Empty
    | Vcons (_, tl) -> return tl

  let front ~measure t =
    match_lwt view_left ~measure reader writer t with
    | Vnil -> raise Empty
    | Vcons (hd, tl) -> return (tl, hd)

  let init ~measure t =
    match_lwt view_right ~measure reader writer t with
    | Vnil -> raise Empty
    | Vcons (_, tl) -> return tl

  let rear ~measure t =
    match_lwt view_right ~measure reader writer t with
    | Vnil -> raise Empty
    | Vcons (hd, tl) -> return (tl, hd)

  (*---------------------------------*)
  (*            append               *)
  (*---------------------------------*)
  let nodes : 'a. measure:('a -> M.t) -> 'a digit -> 'a list -> 'a digit -> 'a node list =
    let add_digit_to : 'a. 'a digit -> 'a list -> 'a list = fun digit l ->
      match digit with
      | `One (_, a) -> a :: l
      | `Two (_, a, b) -> a :: b :: l
      | `Three (_, a, b, c) -> a :: b :: c :: l
      | `Four (_, a, b, c, d) -> a :: b :: c :: d :: l in

    let rec nodes_aux : 'a. measure:('a -> M.t) -> 'a list -> 'a digit -> 'a node list = fun ~measure ts sf2 ->
      match ts, sf2 with
      | [], `One _ -> assert false
      | [], `Two (_, a, b)
      | [a], `One (_, b) -> [node2 ~measure a b]
      | [], `Three (_, a, b, c)
      | [a], `Two (_, b, c)
      | [a; b], `One (_, c) -> [node3 ~measure a b c]
      | [], `Four (_, a, b, c, d)
      | [a], `Three (_, b, c, d)
      | [a; b], `Two (_, c, d)
      | [a; b; c], `One (_, d) -> [node2 ~measure a b; node2 ~measure c d]
      | a :: b :: c :: ts, _ -> node3 ~measure a b c :: nodes_aux ~measure ts sf2
      | [a], `Four (_, b, c, d, e)
      | [a; b], `Three (_, c, d, e) -> [node3 ~measure a b c; node2 ~measure d e]
      | [a; b], `Four (_, c, d, e, f) -> [node3 ~measure a b c; node3 ~measure d e f] in

    fun ~measure sf1 ts sf2 ->
      let ts = add_digit_to sf1 ts in
      nodes_aux ~measure ts sf2

  let rec app3 : 'a. measure:('a -> M.t) -> 'a Json.reader -> 'a Json.writer -> 'a fg -> 'a list -> 'a fg -> 'a fg Lwt.t = fun ~measure reader writer t1 elts t2 ->
    match t1, t2 with
    | `Nil, _ ->
      Lwt_list.fold_right_s (fun elt acc -> cons ~measure reader writer acc elt) elts t2
    | _, `Nil ->
      Lwt_list.fold_left_s (fun acc elt -> snoc ~measure reader writer acc elt) t1 elts
    | `Single x1, _ ->
      lwt t = Lwt_list.fold_right_s (fun elt acc -> cons ~measure reader writer acc elt) elts t2 in
      cons ~measure reader writer t x1
    | _, `Single x2 ->
      lwt t = Lwt_list.fold_left_s (fun acc elt -> snoc ~measure reader writer acc elt) t1 elts in
      snoc ~measure reader writer t x2
    | `Deep (_, pr1, m1, sf1), `Deep (_, pr2, m2, sf2) ->
      let reader' = read_node reader and writer' = write_node writer in
      lwt m1' = get_opt reader' m1 and m2' = get_opt reader' m2 in
      lwt m = app3 ~measure:measure_node reader' writer' m1' (nodes ~measure sf1 elts pr2) m2' in
      deep writer' pr1 m sf2

  let append ~measure t1 t2 = app3 ~measure reader writer t1 [] t2

  (*---------------------------------*)
  (*            reverse              *)
  (*---------------------------------*)
  (* unfortunately, when reversing, we need to rebuild every annotation
   * because the monoid does not have to be commutative *)

  let reverse_digit_node : 'a. ('a node -> 'a node) -> 'a node digit -> 'a node digit = fun rev_a -> function
    | `One (_, a) -> one_node (rev_a a)
    | `Two (_, a, b) -> two_node (rev_a b) (rev_a a)
    | `Three (_, a, b, c) -> three_node (rev_a c) (rev_a b) (rev_a a)
    | `Four (_, a, b, c, d) -> four_node (rev_a d) (rev_a c) (rev_a b) (rev_a a)
  let reverse_digit : 'a. measure:('a -> M.t) -> 'a digit -> 'a digit = fun ~measure -> function
    | `One _ as d -> d
    | `Two (_, a, b) -> two ~measure b a
    | `Three (_, a, b, c) -> three ~measure c b a
    | `Four (_, a, b, c, d) -> four ~measure d c b a
  let reverse_node_node : 'a. ('a node -> 'a node) -> 'a node node -> 'a node node = fun rev_a -> function
    | `Node2 (_, a, b) -> node2_node (rev_a b) (rev_a a)
    | `Node3 (_, a, b, c) -> node3_node (rev_a c) (rev_a b) (rev_a a)
  let reverse_node : 'a. measure:('a -> M.t) -> 'a node -> 'a node = fun ~measure -> function
    | `Node2 (_, a, b) -> node2 ~measure b a
    | `Node3 (_, a, b, c) -> node3 ~measure c b a

  let rec reverse_aux : 'a. 'a node Json.reader -> 'a node Json.writer -> ('a node -> 'a node) -> 'a node fg -> 'a node fg Lwt.t = fun reader writer reverse_a -> function
    | `Nil -> return `Nil
    | `Single a -> return (`Single (reverse_a a))
    | `Deep (_, pr, m, sf) ->
      let rev_pr = reverse_digit_node reverse_a pr in
      let rev_sf = reverse_digit_node reverse_a sf in
      let reader' = read_node reader and writer' = write_node writer in
      lwt m' = get_opt reader' m in
      lwt rev_m = reverse_aux reader' writer' (reverse_node_node (reverse_a)) m' in
      deep writer' rev_sf rev_m rev_pr
  let reverse ~measure = function
    | `Nil
    | `Single _ as t -> return t
    | `Deep (_, pr, m, sf) ->
      let rev_pr = reverse_digit ~measure pr in
      let rev_sf = reverse_digit ~measure sf in
      let reader' = read_node reader and writer' = write_node writer in
      lwt m' = get_opt reader' m in
      lwt rev_m = reverse_aux reader' writer' (reverse_node ~measure) m' in
      deep writer' rev_sf rev_m rev_pr

  (*---------------------------------*)
  (*             split               *)
  (*---------------------------------*)
  let split_digit : 'a. measure:('a -> M.t) -> (M.t -> bool) -> M.t -> 'a digit -> 'a list * 'a * 'a list = fun ~measure p i -> function
    | `One (_, a) -> ([], a, [])
    | `Two (_, a, b) ->
      let i' = M.combine i (measure a) in
      if p i' then ([], a, [b]) else
        ([a], b, [])
    | `Three (_, a, b, c) ->
      let i' = M.combine i (measure a) in
      if p i' then ([], a, [b; c]) else
        let i'' = M.combine i' (measure b) in
        if p i'' then ([a], b, [c]) else
          ([a; b], c, [])
    | `Four (_, a, b, c, d) ->
      let i' = M.combine i (measure a) in
      if p i' then ([], a, [b; c; d]) else
        let i'' = M.combine i' (measure b) in
        if p i'' then ([a], b, [c; d]) else
          let i''' = M.combine i'' (measure c) in
          if p i''' then ([a; b], c, [d]) else
            ([a; b; c], d, [])

  let deep_left ~measure reader writer pr m sf =
    match pr with
    | [] -> (
      match_lwt view_left ~measure:measure_node reader writer m with
      | Vnil -> return (to_tree_digit ~measure sf)
      | Vcons (a, m') ->
        deep writer (to_digit_node a) m' sf
    )
    | _ ->
      deep writer (to_digit_list ~measure pr) m sf
  let deep_right ~measure reader writer pr m sf =
    match sf with
    | [] -> (
      match_lwt view_right ~measure:measure_node reader writer m with
      | Vnil -> return (to_tree_digit ~measure pr)
      | Vcons (a, m') -> deep writer pr m' (to_digit_node a)
    )
    | _ ->
      deep writer pr m (to_digit_list ~measure sf)

  let rec split_tree : 'a. measure:('a -> M.t) -> 'a Json.reader -> 'a Json.writer -> (M.t -> bool) -> M.t -> 'a fg -> ('a fg * 'a * 'a fg) Lwt.t = fun ~measure reader writer p i -> function
    | `Nil -> raise Empty
    | `Single x -> return (`Nil, x, `Nil)
    | `Deep (_, pr, m, sf) ->
      let vpr = M.combine i (measure_digit pr) in
      let reader' = read_node reader and writer' = write_node writer in
      lwt m' = get_opt reader' m in
      if p vpr then
        let (l, x, r) = split_digit ~measure p i pr in
        lwt r' = deep_left ~measure reader' writer' r m' sf in
        return (to_tree_list ~measure l, x, r')
      else
        let vm = M.combine vpr (measure_t_node m') in
        if p vm then
          lwt (ml, xs, mr) = split_tree ~measure:measure_node reader' writer' p vpr m' in
          let (l, x, r) = split_digit ~measure p (M.combine vpr (measure_t_node ml)) (to_digit_node xs) in
          lwt l' = deep_right ~measure reader' writer' pr ml l and r' = deep_left ~measure reader' writer' r mr sf in
          return (l', x, r')
        else
          let (l, x, r) = split_digit ~measure p vm sf in
          lwt l' = deep_right ~measure reader' writer' pr m' l in
          return (l', x, to_tree_list ~measure r)

  let split ~measure f t =
    match t with
    | `Nil -> return (`Nil, `Nil)
    | _ ->
      if f (measure_t ~measure t) then
        lwt (l, x, r) = split_tree ~measure reader writer f M.zero t in
        lwt r' = cons ~measure reader writer r x in
        return (l, r')
      else
        return (t, `Nil)

  (*---------------------------------*)
  (*            lookup               *)
  (*---------------------------------*)
  let lookup_digit : 'a. measure:('a -> M.t) -> (M.t -> bool) -> M.t -> 'a digit -> M.t * 'a = fun ~measure p i -> function
    | `One (_, a) -> M.zero, a
    | `Two (_, a, b) ->
      let m_a = measure a in
      let i' = M.combine i m_a in
      if p i' then M.zero, a else m_a, b
    | `Three (_, a, b, c) ->
      let m_a = measure a in
      let i' = M.combine i m_a in
      if p i' then M.zero, a else
        let m_b = measure b in
        let i'' = M.combine i' m_b in
        if p i'' then m_a, b else M.combine m_a m_b, c
    | `Four (_, a, b, c, d) ->
      let m_a = measure a in
      let i' = M.combine i m_a in
      if p i' then M.zero, a else
        let m_b = measure b in
        let i'' = M.combine i' m_b in
        if p i'' then m_a, b else
          let m_c = measure c in
          let i''' = M.combine i'' m_c in
          if p i''' then M.combine m_a m_b, c else M.combine (M.combine m_a m_b) m_c, d

  let lookup_node : 'a. measure:('a -> M.t) -> (M.t -> bool) -> M.t -> 'a node -> M.t * 'a = fun ~measure p i -> function
    | `Node2 (_, a, b) ->
      let m_a = measure a in
      let i' = M.combine i m_a in
      if p i' then M.zero, a else m_a, b
    | `Node3 (_, a, b, c) ->
      let m_a = measure a in
      let i' = M.combine i m_a in
      if p i' then M.zero, a else
        let m_b = measure b in
        let i'' = M.combine i' m_b in
        if p i'' then m_a, b else M.combine m_a m_b, c

  let rec lookup_tree : 'a. measure:('a -> M.t) -> 'a Json.reader -> (M.t -> bool) -> M.t -> 'a fg -> (M.t * 'a) Lwt.t = fun ~measure reader p i -> function
    | `Nil -> raise Empty
    | `Single x -> return (M.zero, x)
    | `Deep (_, pr, m, sf) ->
      let m_pr = measure_digit pr in
      let vpr = M.combine i m_pr in
      if p vpr then
        return (lookup_digit ~measure p i pr)
      else
        let reader' = read_node reader in
        lwt m' = get_opt reader' m in
        let m_m = measure_t_node m' in
        let vm = M.combine vpr m_m in
        if p vm then
          lwt v_left, node = lookup_tree ~measure:measure_node reader' p vpr m' in
          let v, x = lookup_node ~measure p (M.combine vpr v_left) node in
          return (M.combine (M.combine m_pr v_left) v, x)
        else
          let v, x = lookup_digit ~measure p vm sf in
          return (M.combine (M.combine m_pr m_m) v, x)

  let lookup ~measure p t =
    lookup_tree ~measure reader p M.zero t >|= snd

  (*---------------------------------*)
  (*        classic traversals       *)
  (*---------------------------------*)
  let iter f t =
    fold_left reader (fun () elt -> f elt) () t
  let iter_right f t =
    fold_right reader (fun () elt -> f elt) () t
  let map ~measure f t = (* suboptimal when the measure does not depend on 'a *)
    fold_left reader (fun acc elt -> snoc reader writer ~measure acc (f elt)) empty t
  let map_right ~measure f t =
    fold_right reader (fun acc elt -> cons reader writer ~measure acc (f elt)) empty t
end
