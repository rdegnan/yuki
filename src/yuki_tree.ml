open Lwt
open Riak
open Riak_kv_piqi
open Ag_util
open Yuki_tree_j

exception Empty

module Make(Conn:Yuki_make.Conn)(Elem:Yuki_make.Elem)(Monoid:Yuki_make.Monoid) = struct
  let empty = `Nil
  let is_empty = function `Nil -> true | `Single _ | `Deep _ -> false

  let reader v lexbuf = Elem.of_string (Yojson.Safe.read_string v lexbuf)
  let writer ob x = Yojson.Safe.write_string ob (Elem.to_string x)

  let get reader key =
    Conn.with_connection (fun conn ->
      match_lwt riak_get conn Elem.bucket key [] with
        | Some { obj_value = Some value } -> return (Json.from_string (read_fg reader) value)
        | _ -> raise Not_found
    )

  let put writer ?key ?(ops=[Put_return_head true; Put_if_none_match true]) x =
    Conn.with_connection (fun conn ->
      match_lwt riak_put conn Elem.bucket key (Json.to_string (write_fg writer) x) ops with
        | Some { obj_key = Some key } -> return key
        | _ -> (match key with
            | Some key -> return key
            | None -> raise Not_found
        )
    )

  (*---------------------------------*)
  (*              fold               *)
  (*---------------------------------*)
  (*let fold_right_node f acc = function
    | `Node2 (_, a, b) -> f (f acc b) a
    | `Node3 (_, a, b, c) -> f (f (f acc c) b) a
  let fold_left_node f acc = function
    | `Node2 (_, a, b) -> f (f acc a) b
    | `Node3 (_, a, b, c) -> f (f (f acc a) b) c

  let fold_right_digit f acc = function
    | `One (_, a) -> f acc a
    | `Two (_, a, b) -> f (f acc b) a
    | `Three (_, a, b, c) -> f (f (f acc c) b) a
    | `Four (_, a, b, c, d) -> f (f (f (f acc d) c) b) a
  let fold_left_digit f acc = function
    | `One (_, a) -> f acc a
    | `Two (_, a, b) -> f (f acc a) b
    | `Three (_, a, b, c) -> f (f (f acc a) b) c
    | `Four (_, a, b, c, d) -> f (f (f (f acc a) b) c) d

  let rec fold_right : 'acc 'a. ('acc -> 'a -> 'acc) -> 'acc -> 'a fg -> 'acc = fun f acc -> function
    | `Nil -> acc
    | `Single x -> f acc x
    | `Deep (_, pr, m, sf) ->
      let acc = fold_right_digit f acc sf in
      let acc = fold_right (fun acc elt -> fold_right_node f acc elt) acc m in
      let acc = fold_right_digit f acc pr in
      acc
  let rec fold_left : 'acc 'a. ('acc -> 'a -> 'acc) -> 'acc -> 'a fg -> 'acc = fun f acc -> function
    | `Nil -> acc
    | `Single x -> f acc x
    | `Deep (_, pr, m, sf) ->
      let acc = fold_left_digit f acc pr in
      let acc = fold_left (fun acc elt -> fold_left_node f acc elt) acc m in
      let acc = fold_left_digit f acc sf in
      acc*)

  (*---------------------------------*)
  (*     measurement functions       *)
  (*---------------------------------*)
  let measure_node : 'a. 'a node -> Monoid.t = function
    | `Node2 (v, _, _)
    | `Node3 (v, _, _, _) -> Monoid.of_string v

  let measure_digit : 'a. 'a digit -> Monoid.t = function
    | `One (v, _)
    | `Two (v, _, _)
    | `Three (v, _, _, _)
    | `Four (v, _, _, _, _) -> Monoid.of_string v

  let measure_t_node : 'a. 'a node fg -> Monoid.t = function
    | `Nil -> Monoid.zero
    | `Single x -> measure_node x
    | `Deep (v, _, _, _) -> Monoid.of_string v
  let measure_t : 'a. measure:('a -> Monoid.t) -> 'a fg -> Monoid.t = fun ~measure -> function
    | `Nil -> Monoid.zero
    | `Single x -> measure x
    | `Deep (v, _, _, _) -> Monoid.of_string v

  (*---------------------------------*)
  (*  a bunch of smart constructors  *)
  (*---------------------------------*)
  let node2_node : 'a. 'a node -> 'a node -> 'a node node = fun a b ->
    `Node2 (Monoid.to_string (Monoid.combine (measure_node a) (measure_node b)), a, b)
  let node2 : 'a. measure:('a -> Monoid.t) -> 'a -> 'a -> 'a node = fun ~measure a b ->
    `Node2 (Monoid.to_string (Monoid.combine (measure a) (measure b)), a, b)

  let node3_node : 'a. 'a node -> 'a node -> 'a node -> 'a node node = fun a b c ->
    `Node3 (Monoid.to_string (Monoid.combine (measure_node a) (Monoid.combine (measure_node b) (measure_node c))), a, b, c)
  let node3 : 'a. measure:('a -> Monoid.t) -> 'a -> 'a -> 'a -> 'a node = fun ~measure a b c ->
    `Node3 (Monoid.to_string (Monoid.combine (measure a) (Monoid.combine (measure b) (measure c))), a, b, c)

  let deep : 'a. 'a Json.writer -> 'a digit -> 'a node fg -> 'a digit -> 'a fg Lwt.t = fun writer pr m sf ->
    let v = measure_digit pr in
    let v = Monoid.combine v (measure_t_node m) in
    let v = Monoid.combine v (measure_digit sf) in
    lwt m' = put (write_node writer) m in
    return (`Deep (Monoid.to_string v, pr, m', sf))

  let one_node : 'a. 'a node -> 'a node digit = fun a ->
    `One (Monoid.to_string (measure_node a), a)
  let one : 'a. measure:('a -> Monoid.t) -> 'a -> 'a digit = fun ~measure a ->
    `One (Monoid.to_string (measure a), a)

  let two_node : 'a. 'a node -> 'a node -> 'a node digit = fun a b ->
    `Two (Monoid.to_string (Monoid.combine (measure_node a) (measure_node b)), a, b)
  let two : 'a. measure:('a -> Monoid.t) -> 'a -> 'a -> 'a digit = fun ~measure a b ->
    `Two (Monoid.to_string (Monoid.combine (measure a) (measure b)), a, b)

  let three_node : 'a. 'a node -> 'a node -> 'a node -> 'a node digit = fun a b c ->
    `Three (Monoid.to_string (Monoid.combine (Monoid.combine (measure_node a) (measure_node b)) (measure_node c)), a, b, c)
  let three : 'a. measure:('a -> Monoid.t) -> 'a -> 'a -> 'a -> 'a digit = fun ~measure a b c ->
    `Three (Monoid.to_string (Monoid.combine (Monoid.combine (measure a) (measure b)) (measure c)), a, b, c)

  let four_node : 'a. 'a node -> 'a node -> 'a node -> 'a node -> 'a node digit = fun a b c d ->
    `Four (Monoid.to_string (Monoid.combine (Monoid.combine (measure_node a) (measure_node b)) (Monoid.combine (measure_node c) (measure_node d))), a, b, c, d)
  let four : 'a. measure:('a -> Monoid.t) -> 'a -> 'a -> 'a -> 'a -> 'a digit = fun ~measure a b c d ->
    `Four (Monoid.to_string (Monoid.combine (Monoid.combine (measure a) (measure b)) (Monoid.combine (measure c) (measure d))), a, b, c, d)

  (*---------------------------------*)
  (*          cons / snoc            *)
  (*---------------------------------*)
  let cons_digit_node : 'a. 'a node digit -> 'a node -> 'a node digit = fun d x ->
    match d with
    | `One (v, a) -> `Two (Monoid.to_string (Monoid.combine (measure_node x) (Monoid.of_string v)), x, a)
    | `Two (v, a, b) -> `Three (Monoid.to_string (Monoid.combine (measure_node x) (Monoid.of_string v)), x, a, b)
    | `Three (v, a, b, c) -> `Four (Monoid.to_string (Monoid.combine (measure_node x) (Monoid.of_string v)), x, a, b, c)
    | `Four _ -> assert false
  let cons_digit : 'a. measure:('a -> Monoid.t) -> 'a digit -> 'a -> 'a digit = fun ~measure d x ->
    match d with
    | `One (v, a) -> `Two (Monoid.to_string (Monoid.combine (measure x) (Monoid.of_string v)), x, a)
    | `Two (v, a, b) -> `Three (Monoid.to_string (Monoid.combine (measure x) (Monoid.of_string v)), x, a, b)
    | `Three (v, a, b, c) -> `Four (Monoid.to_string (Monoid.combine (measure x) (Monoid.of_string v)), x, a, b, c)
    | `Four _ -> assert false

  let snoc_digit_node : 'a. 'a node digit -> 'a node -> 'a node digit = fun d x ->
    match d with
    | `One (v, a) -> `Two (Monoid.to_string (Monoid.combine (Monoid.of_string v) (measure_node x)), a, x)
    | `Two (v, a, b) -> `Three (Monoid.to_string (Monoid.combine (Monoid.of_string v) (measure_node x)), a, b, x)
    | `Three (v, a, b, c) -> `Four (Monoid.to_string (Monoid.combine (Monoid.of_string v) (measure_node x)), a, b, c, x)
    | `Four _ -> assert false
  let snoc_digit : 'a. measure:('a -> Monoid.t) -> 'a digit -> 'a -> 'a digit = fun ~measure d x ->
    match d with
    | `One (v, a) -> `Two (Monoid.to_string (Monoid.combine (Monoid.of_string v) (measure x)), a, x)
    | `Two (v, a, b) -> `Three (Monoid.to_string (Monoid.combine (Monoid.of_string v) (measure x)), a, b, x)
    | `Three (v, a, b, c) -> `Four (Monoid.to_string (Monoid.combine (Monoid.of_string v) (measure x)), a, b, c, x)
    | `Four _ -> assert false

  let rec cons_aux : 'a. 'a node Json.reader -> 'a node Json.writer -> 'a node fg -> 'a node -> 'a node fg Lwt.t = fun reader writer t a ->
    match t with
    | `Nil ->
      return (`Single a)
    | `Single b ->
      deep writer (one_node a) `Nil (one_node b)
    | `Deep (_, `Four (_, b, c, d, e), m, sf) ->
      let reader' = read_node reader and writer' = write_node writer in
      lwt m' = get reader' m in
      lwt m'' = cons_aux reader' writer' m' (node3_node c d e) in
      deep writer (two_node a b) m'' sf
    | `Deep (v, pr, m, sf) ->
      return (`Deep (Monoid.to_string (Monoid.combine (measure_node a) (Monoid.of_string v)), cons_digit_node pr a, m, sf))
  let cons ~measure t a =
    match t with
    | `Nil ->
      return (`Single a)
    | `Single b ->
      deep writer (one ~measure a) `Nil (one ~measure b)
    | `Deep (_, `Four (_, b, c, d, e), m, sf) ->
      let reader' = read_node reader and writer' = write_node writer in
      lwt m' = get reader' m in
      lwt m'' = cons_aux reader' writer' m' (node3 ~measure c d e) in
      deep writer (two ~measure a b) m'' sf
    | `Deep (v, pr, m, sf) ->
      return (`Deep (Monoid.to_string (Monoid.combine (measure a) (Monoid.of_string v)), cons_digit ~measure pr a, m, sf))

  let rec snoc_aux : 'a. 'a node Json.reader -> 'a node Json.writer -> 'a node fg -> 'a node -> 'a node fg Lwt.t = fun reader writer t a ->
    match t with
    | `Nil ->
      return (`Single a)
    | `Single b ->
      deep writer (one_node b) `Nil (one_node a)
    | `Deep (_, pr, m, `Four (_, b, c, d, e)) ->
      let reader' = read_node reader and writer' = write_node writer in
      lwt m' = get reader' m in
      lwt m'' = snoc_aux reader' writer' m' (node3_node b c d) in
      deep writer pr m'' (two_node e a)
    | `Deep (v, pr, m, sf) ->
      return (`Deep (Monoid.to_string (Monoid.combine (Monoid.of_string v) (measure_node a)), pr, m, snoc_digit_node sf a))
  let snoc ~measure t a =
    match t with
    | `Nil ->
      return (`Single a)
    | `Single b ->
      deep writer (one ~measure b) `Nil (one ~measure a)
    | `Deep (_, pr, m, `Four (_, b, c, d, e)) ->
      let reader' = read_node reader and writer' = write_node writer in
      lwt m' = get reader' m in
      lwt m'' = snoc_aux reader' writer' m' (node3 ~measure b c d) in
      deep writer pr m'' (two ~measure e a)
    | `Deep (v, pr, m, sf) ->
      return (`Deep (Monoid.to_string (Monoid.combine (Monoid.of_string v) (measure a)), pr, m, snoc_digit ~measure sf a))

  (*---------------------------------*)
  (*     various conversions         *)
  (*---------------------------------*)
  (*let to_tree_digit_node : 'a. 'a digit -> 'a fg = fun d ->
    match d with
    | `One (_, a) -> `Single a
    | `Two (v, a, b) -> `Deep (v, one_node a, `Nil, one_node b)
    | `Three (v, a, b, c) -> `Deep (v, two_node a b, `Nil, one_node c)
    | `Four (v, a, b, c, d) -> `Deep (v, three_node a b c, `Nil, one_node d)
  let to_tree_digit ~measure d =
    match d with
    | `One (_, a) -> `Single a
    | `Two (v, a, b) -> `Deep (v, one ~measure a, `Nil, one ~measure b)
    | `Three (v, a, b, c) -> `Deep (v, two ~measure a b, `Nil, one ~measure c)
    | `Four (v, a, b, c, d) -> `Deep (v, three ~measure a b c, `Nil, one ~measure d)
  (*let to_tree_list ~measure = function
    | [] -> `Nil
    | [a] -> `Single a
    | [a; b] -> deep (one ~measure a) `Nil (one ~measure b)
    | [a; b; c] -> deep (two ~measure a b) `Nil (one ~measure c)
    | [a; b; c; d] -> deep (three ~measure a b c) `Nil (one ~measure d)
    | _ -> assert false*)

  let to_digit_node = function
    | `Node2 (v, a, b) -> `Two (v, a, b)
    | `Node3 (v, a, b, c) -> `Three (v, a, b, c)
  let to_digit_list ~measure = function
    | [a] -> one ~measure a
    | [a; b] -> two ~measure a b
    | [a; b; c] -> three ~measure a b c
    | [a; b; c; d] -> four ~measure a b c d
    | _ -> assert false
  let to_digit_list_node = function
    | [a] -> one_node a
    | [a; b] -> two_node a b
    | [a; b; c] -> three_node a b c
    | [a; b; c; d] -> four_node a b c d
    | _ -> assert false

  (*---------------------------------*)
  (*     front / rear / etc.         *)
  (*---------------------------------*)
  let head_digit = function
    | `One (_, a)
    | `Two (_, a, _)
    | `Three (_, a, _, _)
    | `Four (_, a, _, _, _) -> a
  let last_digit = function
    | `One (_, a)
    | `Two (_, _, a)
    | `Three (_, _, _, a)
    | `Four (_, _, _, _, a) -> a
  let tail_digit_node = function
    | `One _ -> assert false
    | `Two (_, _, a) -> one_node a
    | `Three (_, _, a, b) -> two_node a b
    | `Four (_, _, a, b, c) -> three_node a b c
  let tail_digit ~measure = function
    | `One _ -> assert false
    | `Two (_, _, a) -> one ~measure a
    | `Three (_, _, a, b) -> two ~measure a b
    | `Four (_, _, a, b, c) -> three ~measure a b c
  let init_digit_node = function
    | `One _ -> assert false
    | `Two (_, a, _) -> one_node a
    | `Three (_, a, b, _) -> two_node a b
    | `Four (_, a, b, c, _) -> three_node a b c
  let init_digit ~measure = function
    | `One _ -> assert false
    | `Two (_, a, _) -> one ~measure a
    | `Three (_, a, b, _) -> two ~measure a b
    | `Four (_, a, b, c, _) -> three ~measure a b c

  type ('a, 'rest) view =
    | Vnil
    | Vcons of 'a * 'rest

  let rec view_left_aux : 'a. 'a node Json.reader -> 'a node Json.writer -> 'a node fg -> ('a node, 'a node fg) view Lwt.t = fun reader writer -> function
    | `Nil -> return Vnil
    | `Single x -> return (Vcons (x, `Nil))
    | `Deep (_, `One (_, a), m, sf) ->
      let reader' = read_node reader and writer' = write_node writer in
      lwt m' = get reader' m in
      lwt m'' = view_left_aux reader' writer' m' in
      lwt vcons =
        match m'' with
        | Vnil -> return (to_tree_digit_node sf)
        | Vcons (a, m') -> deep writer (to_digit_node a) m' sf in
      Vcons (a, vcons)
    | `Deep (_, pr, m, sf) ->
      let vcons = deep (tail_digit_node pr) m sf in
      Vcons (head_digit pr, vcons)
  let view_left ~measure = function
    | `Nil -> Vnil
    | `Single x -> Vcons (x, `Nil)
    | `Deep (_, `One (_, a), m, sf) ->
      let vcons =
        match view_left_aux m with
        | Vnil -> to_tree_digit ~measure sf
        | Vcons (a, m') -> deep (to_digit_node a) m' sf in
      Vcons (a, vcons)
    | `Deep (_, pr, m, sf) ->
      let vcons = deep (tail_digit ~measure pr) m sf in
      Vcons (head_digit pr, vcons)

  let rec view_right_aux : 'a. 'a node fg -> ('a node, 'a node fg) view =
    function
    | `Nil -> Vnil
    | `Single x -> Vcons (x, `Nil)
    | `Deep (_, pr, m, `One (_, a)) ->
      let vcons =
        match view_right_aux m with
        | Vnil -> to_tree_digit_node pr
        | Vcons (a, m') -> deep pr m' (to_digit_node a) in
      Vcons (a, vcons)
    | `Deep (_, pr, m, sf) ->
      let vcons = deep pr m (init_digit_node sf) in
      Vcons (last_digit sf, vcons)
  let view_right ~measure = function
    | `Nil -> Vnil
    | `Single x -> Vcons (x, `Nil)
    | `Deep (_, pr, m, `One (_, a)) ->
      let vcons =
        match view_right_aux m with
        | Vnil -> to_tree_digit ~measure pr
        | Vcons (a, m') -> deep pr m' (to_digit_node a) in
      Vcons (a, vcons)
    | `Deep (_, pr, m, sf) ->
      let vcons = deep pr m (init_digit ~measure sf) in
      Vcons (last_digit sf, vcons)

  let head_exn = function
    | `Nil -> raise Empty
    | `Single a -> a
    | `Deep (_, pr, _, _) -> head_digit pr
  let head = function
    | `Nil -> None
    | `Single a -> Some a
    | `Deep (_, pr, _, _) -> Some (head_digit pr)

  let last_exn = function
    | `Nil -> raise Empty
    | `Single a -> a
    | `Deep (_, _, _, sf) -> last_digit sf
  let last = function
    | `Nil -> None
    | `Single a -> Some a
    | `Deep (_, _, _, sf) -> Some (last_digit sf)

  let tail ~measure t =
    match view_left ~measure t with
    | Vnil -> None
    | Vcons (_, tl) -> Some tl
  let tail_exn ~measure t =
    match view_left ~measure t with
    | Vnil -> raise Empty
    | Vcons (_, tl) -> tl

  let front ~measure t =
    match view_left ~measure t with
    | Vnil -> None
    | Vcons (hd, tl) -> Some (tl, hd)
  let front_exn ~measure t =
    match view_left ~measure t with
    | Vnil -> raise Empty
    | Vcons (hd, tl) -> (tl, hd)

  let init ~measure t =
    match view_right ~measure t with
    | Vnil -> None
    | Vcons (_, tl) -> Some tl
  let init_exn ~measure t =
    match view_right ~measure t with
    | Vnil -> raise Empty
    | Vcons (_, tl) -> tl

  let rear ~measure t =
    match view_right ~measure t with
    | Vnil -> None
    | Vcons (hd, tl) -> Some (tl, hd)
  let rear_exn ~measure t =
    match view_right ~measure t with
    | Vnil -> raise Empty
    | Vcons (hd, tl) -> (tl, hd)*)

  (*---------------------------------*)
  (*            append               *)
  (*---------------------------------*)
  (*let nodes =
    let add_digit_to digit l =
      match digit with
      | `One (_, a) -> a :: l
      | `Two (_, a, b) -> a :: b :: l
      | `Three (_, a, b, c) -> a :: b :: c :: l
      | `Four (_, a, b, c, d) -> a :: b :: c :: d :: l in

    let rec nodes_aux ~measure ts sf2 = (* no idea if this should be tail rec *)
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

  let rec app3 : 'a. measure:('a -> Monoid.t) -> 'a fg -> 'a list -> 'a fg -> 'a fg =
    fun ~measure t1 elts t2 ->
    match t1, t2 with
    | `Nil, _ ->
      List.fold_right (fun elt acc -> cons ~measure acc elt) elts t2
    | _, `Nil ->
      List.fold_left (fun acc elt -> snoc ~measure acc elt) t1 elts
    | `Single x1, _ ->
      cons ~measure (List.fold_right (fun elt acc -> cons ~measure acc elt) elts t2) x1
    | _, `Single x2 ->
      snoc ~measure (List.fold_left (fun acc elt -> snoc ~measure acc elt) t1 elts) x2
    | `Deep (_, pr1, m1, sf1), `Deep (_, pr2, m2, sf2) ->
      deep pr1 (app3 ~measure:measure_node m1 (nodes ~measure sf1 elts pr2) m2) sf2

  let append ~measure t1 t2 = app3 ~measure t1 [] t2

  (*---------------------------------*)
  (*            reverse              *)
  (*---------------------------------*)
  (* unfortunately, when reversing, we need to rebuild every annotation
   * because the monoid does not have to be commutative *)

  let reverse_digit_node rev_a = function
    | `One (_, a) -> one_node (rev_a a)
    | `Two (_, a, b) -> two_node (rev_a b) (rev_a a)
    | `Three (_, a, b, c) -> three_node (rev_a c) (rev_a b) (rev_a a)
    | `Four (_, a, b, c, d) -> four_node (rev_a d) (rev_a c) (rev_a b) (rev_a a)
  let reverse_digit ~measure = function
    | `One _ as d -> d
    | `Two (_, a, b) -> two ~measure b a
    | `Three (_, a, b, c) -> three ~measure c b a
    | `Four (_, a, b, c, d) -> four ~measure d c b a
  let reverse_node_node rev_a = function
    | Node2 (_, a, b) -> node2_node (rev_a b) (rev_a a)
    | Node3 (_, a, b, c) -> node3_node (rev_a c) (rev_a b) (rev_a a)
  let reverse_node ~measure = function
    | Node2 (_, a, b) -> node2 ~measure b a
    | Node3 (_, a, b, c) -> node3 ~measure c b a

  let rec reverse_aux : 'a. ('a node -> 'a node) -> 'a node fg -> 'a node fg =
    fun reverse_a -> function
    | `Nil -> `Nil
    | `Single a -> `Single (reverse_a a)
    | `Deep (_, pr, m, sf) ->
      let rev_pr = reverse_digit_node reverse_a pr in
      let rev_sf = reverse_digit_node reverse_a sf in
      let rev_m = reverse_aux (reverse_node_node (reverse_a)) m in
      deep rev_sf rev_m rev_pr
  let reverse ~measure = function
    | `Nil
    | `Single _ as t -> t
    | `Deep (_, pr, m, sf) ->
      let rev_pr = reverse_digit ~measure pr in
      let rev_sf = reverse_digit ~measure sf in
      let rev_m = reverse_aux (reverse_node ~measure) m in
      deep rev_sf rev_m rev_pr

  (*---------------------------------*)
  (*             split               *)
  (*---------------------------------*)
  type ('a, 'rest) split = Split of 'rest * 'a * 'rest

  let split_digit ~measure p i = function
    | `One (_, a) -> Split ([], a, [])
    | `Two (_, a, b) ->
      let i' = Monoid.combine i (measure a) in
      if p i' then Split ([], a, [b]) else
        Split ([a], b, [])
    | `Three (_, a, b, c) ->
      let i' = Monoid.combine i (measure a) in
      if p i' then Split ([], a, [b; c]) else
        let i'' = Monoid.combine i' (measure b) in
        if p i'' then Split ([a], b, [c]) else
          Split ([a; b], c, [])
    | `Four (_, a, b, c, d) ->
      let i' = Monoid.combine i (measure a) in
      if p i' then Split ([], a, [b; c; d]) else
        let i'' = Monoid.combine i' (measure b) in
        if p i'' then Split ([a], b, [c; d]) else
          let i''' = Monoid.combine i'' (measure c) in
          if p i''' then Split ([a; b], c, [d]) else
            Split ([a; b; c], d, [])

  let deep_left ~measure pr m sf =
    match pr with
    | [] -> (
      match view_left ~measure:measure_node m with
      | Vnil -> to_tree_digit ~measure sf
      | Vcons (a, m') -> deep (to_digit_node a) m' sf
    )
    | _ ->
      deep (to_digit_list ~measure pr) m sf
  let deep_right ~measure pr m sf =
    match sf with
    | [] -> (
      match view_right ~measure:measure_node m with
      | Vnil -> to_tree_digit ~measure pr
      | Vcons (a, m') -> deep pr m' (to_digit_node a)
    )
    | _ ->
      deep pr m (to_digit_list ~measure sf)

  let rec split_tree : 'a. measure:('a -> Monoid.t) -> (Monoid.t -> bool) -> Monoid.t -> 'a fg -> ('a, 'a fg) split =
    fun ~measure p i -> function
    | `Nil -> raise Empty
    | `Single x -> Split (`Nil, x, `Nil)
    | `Deep (_, pr, m, sf) ->
      let vpr = Monoid.combine i (measure_digit pr) in
      if p vpr then
        let Split (l, x, r) = split_digit ~measure p i pr in
        Split (to_tree_list ~measure l, x, deep_left ~measure r m sf)
      else
        let vm = Monoid.combine vpr (measure_t_node m) in
        if p vm then
          let Split (ml, xs, mr) = split_tree ~measure:measure_node p vpr m in
          let Split (l, x, r) = split_digit ~measure p (Monoid.combine vpr (measure_t_node ml)) (to_digit_node xs) in
          Split (deep_right ~measure pr ml l, x, deep_left ~measure r mr sf)
        else
          let Split (l, x, r) = split_digit ~measure p vm sf in
          Split (deep_right ~measure pr m l, x, to_tree_list ~measure r)

  let split ~measure f t =
    match t with
    | `Nil -> (`Nil, `Nil)
    | _ ->
      if f (measure_t ~measure t) then
        let Split (l, x, r) = split_tree ~measure f Monoid.zero t in
        (l, cons ~measure r x)
      else
        (t, `Nil)

  (*---------------------------------*)
  (*            lookup               *)
  (*---------------------------------*)
  let lookup_digit ~measure p i = function
    | `One (_, a) -> Monoid.zero, a
    | `Two (_, a, b) ->
      let m_a = measure a in
      let i' = Monoid.combine i m_a in
      if p i' then Monoid.zero, a else m_a, b
    | `Three (_, a, b, c) ->
      let m_a = measure a in
      let i' = Monoid.combine i m_a in
      if p i' then Monoid.zero, a else
        let m_b = measure b in
        let i'' = Monoid.combine i' m_b in
        if p i'' then m_a, b else Monoid.combine m_a m_b, c
    | `Four (_, a, b, c, d) ->
      let m_a = measure a in
      let i' = Monoid.combine i m_a in
      if p i' then Monoid.zero, a else
        let m_b = measure b in
        let i'' = Monoid.combine i' m_b in
        if p i'' then m_a, b else
          let m_c = measure c in
          let i''' = Monoid.combine i'' m_c in
          if p i''' then Monoid.combine m_a m_b, c else Monoid.combine (Monoid.combine m_a m_b) m_c, d

  let lookup_node ~measure p i = function
    | Node2 (_, a, b) ->
      let m_a = measure a in
      let i' = Monoid.combine i m_a in
      if p i' then Monoid.zero, a else m_a, b
    | Node3 (_, a, b, c) ->
      let m_a = measure a in
      let i' = Monoid.combine i m_a in
      if p i' then Monoid.zero, a else
        let m_b = measure b in
        let i'' = Monoid.combine i' m_b in
        if p i'' then m_a, b else Monoid.combine m_a m_b, c

  let rec lookup_tree : 'a. measure:('a -> Monoid.t) -> (Monoid.t -> bool) -> Monoid.t -> 'a fg -> Monoid.t * 'a =
    fun ~measure p i -> function
    | `Nil -> raise Empty
    | `Single x -> Monoid.zero, x
    | `Deep (_, pr, m, sf) ->
      let m_pr = measure_digit pr in
      let vpr = Monoid.combine i m_pr in
      if p vpr then lookup_digit ~measure p i pr else
        let m_m = measure_t_node m in
        let vm = Monoid.combine vpr m_m in
        if p vm then
          let v_left, node = lookup_tree ~measure:measure_node p vpr m in
          let v, x = lookup_node ~measure p (Monoid.combine vpr v_left) node in
          Monoid.combine (Monoid.combine m_pr v_left) v, x
        else
          let v, x = lookup_digit ~measure p vm sf in
          Monoid.combine (Monoid.combine m_pr m_m) v, x

  let lookup ~measure p t =
    snd (lookup_tree ~measure p Monoid.zero t)

  (*---------------------------------*)
  (*        classic traversals       *)
  (*---------------------------------*)
  let iter f t =
    fold_left (fun () elt -> f elt) () t
  let iter_right f t =
    fold_right (fun () elt -> f elt) () t
  let map ~measure f t = (* suboptimal when the measure does not depend on 'a *)
    fold_left (fun acc elt -> snoc ~measure acc (f elt)) empty t
  let map_right ~measure f t =
    fold_right (fun acc elt -> cons ~measure acc (f elt)) empty t*)
end
