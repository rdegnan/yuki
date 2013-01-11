type 'a monoid = {
  zero : 'a;
  combine : 'a -> 'a -> 'a ;
}

exception Empty

module Generic =
struct
  type ('a, 'm) node =
    | Node2 of 'm * 'a * 'a
    | Node3 of 'm * 'a * 'a * 'a
  type ('a, 'm) digit =
    | One of 'm * 'a
    | Two of 'm * 'a * 'a
    | Three of 'm * 'a * 'a * 'a
    | Four of 'm * 'a * 'a * 'a * 'a
  type ('a, 'm) fg =
    | Nil (* not called Empty as in the paper to avoid a name
           * clash with the exception Empty *)
    | Single of 'a
    | Deep of 'm * ('a, 'm) digit * (('a, 'm) node, 'm) fg * ('a, 'm) digit

  let empty = Nil
  let singleton a = Single a

  let is_empty = function
    | Nil -> true
    | Single _ | Deep _ -> false

  (*---------------------------------*)
  (*              fold               *)
  (*---------------------------------*)
  let fold_right_node f acc = function
    | Node2 (_, a, b) -> f (f acc b) a
    | Node3 (_, a, b, c) -> f (f (f acc c) b) a
  let fold_left_node f acc = function
    | Node2 (_, a, b) -> f (f acc a) b
    | Node3 (_, a, b, c) -> f (f (f acc a) b) c

  let fold_right_digit f acc = function
    | One (_, a) -> f acc a
    | Two (_, a, b) -> f (f acc b) a
    | Three (_, a, b, c) -> f (f (f acc c) b) a
    | Four (_, a, b, c, d) -> f (f (f (f acc d) c) b) a
  let fold_left_digit f acc = function
    | One (_, a) -> f acc a
    | Two (_, a, b) -> f (f acc a) b
    | Three (_, a, b, c) -> f (f (f acc a) b) c
    | Four (_, a, b, c, d) -> f (f (f (f acc a) b) c) d

  let rec fold_right : 'acc 'a 'm. ('acc -> 'a -> 'acc) -> 'acc -> ('a, 'm) fg -> 'acc = fun f acc -> function
    | Nil -> acc
    | Single x -> f acc x
    | Deep (_, pr, m, sf) ->
      let acc = fold_right_digit f acc sf in
      let acc = fold_right (fun acc elt -> fold_right_node f acc elt) acc m in
      let acc = fold_right_digit f acc pr in
      acc
  let rec fold_left : 'acc 'a 'm. ('acc -> 'a -> 'acc) -> 'acc -> ('a, 'm) fg -> 'acc = fun f acc -> function
    | Nil -> acc
    | Single x -> f acc x
    | Deep (_, pr, m, sf) ->
      let acc = fold_left_digit f acc pr in
      let acc = fold_left (fun acc elt -> fold_left_node f acc elt) acc m in
      let acc = fold_left_digit f acc sf in
      acc

  (*---------------------------------*)
  (*     measurement functions       *)
  (*---------------------------------*)
  type ('wrapped_type, 'a, 'm) wrap = monoid:'m monoid -> measure:('a -> 'm) -> 'wrapped_type
  let measure_node = function
    | Node2 (v, _, _)
    | Node3 (v, _, _, _) -> v

  let measure_digit = function
    | One (v, _)
    | Two (v, _, _)
    | Three (v, _, _, _)
    | Four (v, _, _, _, _) -> v

  let measure_t_node ~monoid = function
    | Nil -> monoid.zero
    | Single x -> measure_node x
    | Deep (v, _, _, _) -> v
  let measure_t ~monoid ~measure = function
    | Nil -> monoid.zero
    | Single x -> measure x
    | Deep (v, _, _, _) -> v

  let check_measures_digit ~monoid ~measure ~eq check = function
    | One (v, a) ->
      check a &&
      eq (measure a) v
    | Two (v, a, b) ->
      check a &&
      check b &&
      eq (monoid.combine (measure a) (measure b)) v
    | Three (v, a, b, c) ->
      check a &&
      check b &&
      check c &&
      eq (monoid.combine
            (monoid.combine (measure a) (measure b))
            (measure c)) v
    | Four (v, a, b, c, d) ->
      check a &&
      check b &&
      check c &&
      check d &&
      eq (monoid.combine
            (monoid.combine (measure a) (measure b))
            (monoid.combine (measure c) (measure d))) v
  let check_measures_node ~monoid ~measure ~eq check = function
    | Node2 (v, a, b) ->
      check a &&
      check b &&
      eq (monoid.combine (measure a) (measure b)) v
    | Node3 (v, a, b, c) ->
      check a &&
      check b &&
      check c &&
      eq (monoid.combine
            (monoid.combine (measure a) (measure b))
            (measure c)) v

  let rec check_measures : 'a 'm. monoid:'m monoid -> measure:('a -> 'm) -> eq:('m -> 'm -> bool) -> ('a -> bool) -> ('a, 'm) fg -> bool =
    fun ~monoid ~measure ~eq check -> function
    | Nil -> true
    | Single a -> check a
    | Deep (v, pr, m, sf) ->
      check_measures_digit ~monoid ~measure ~eq check pr &&
      check_measures_digit ~monoid ~measure ~eq check sf &&
      check_measures ~monoid ~measure:measure_node ~eq (fun a ->
        check_measures_node ~monoid ~measure ~eq check a
      ) m &&
      eq (monoid.combine (measure_digit pr) (monoid.combine (measure_t_node ~monoid m) (measure_digit sf))) v

  let check_measures ~monoid ~measure ~eq t =
    check_measures ~monoid ~measure ~eq (fun _ -> true) t

  (*---------------------------------*)
  (*  a bunch of smart constructors  *)
  (*---------------------------------*)
  let node2 ~monoid ~measure a b =
    Node2 (monoid.combine (measure a) (measure b), a, b)
  let node2_node ~monoid a b =
    Node2 (monoid.combine (measure_node a) (measure_node b), a, b)

  let node3 ~monoid ~measure a b c =
    Node3 (monoid.combine (measure a) (monoid.combine (measure b) (measure c)), a, b, c)
  let node3_node ~monoid a b c =
    Node3 (monoid.combine (measure_node a) (monoid.combine (measure_node b) (measure_node c)), a, b, c)

  let deep ~monoid pr m sf =
    let v = measure_digit pr in
    let v = monoid.combine v (measure_t_node ~monoid m) in
    let v = monoid.combine v (measure_digit sf) in
    Deep (v, pr, m, sf)

  let one_node a =
    One (measure_node a, a)
  let one ~measure a =
    One (measure a, a)

  let two_node ~monoid a b =
    Two (monoid.combine (measure_node a) (measure_node b), a, b)
  let two ~monoid ~measure a b =
    Two (monoid.combine (measure a) (measure b), a, b)

  let three_node ~monoid a b c =
    Three (monoid.combine (monoid.combine (measure_node a) (measure_node b)) (measure_node c), a, b, c)
  let three ~monoid ~measure a b c =
    Three (monoid.combine (monoid.combine (measure a) (measure b)) (measure c), a, b, c)

  let four_node ~monoid a b c d =
    Four (monoid.combine (monoid.combine (measure_node a) (measure_node b)) (monoid.combine (measure_node c) (measure_node d)), a, b, c, d)
  let four ~monoid ~measure a b c d =
    Four (monoid.combine (monoid.combine (measure a) (measure b)) (monoid.combine (measure c) (measure d)), a, b, c, d)

  (*---------------------------------*)
  (*          cons / snoc            *)
  (*---------------------------------*)
  let cons_digit_node ~monoid d x =
    match d with
    | One (v, a) -> Two (monoid.combine (measure_node x) v, x, a)
    | Two (v, a, b) -> Three (monoid.combine (measure_node x) v, x, a, b)
    | Three (v, a, b, c) -> Four (monoid.combine (measure_node x) v, x, a, b, c)
    | Four _ -> assert false (*BISECT-VISIT*)
  let cons_digit ~monoid ~measure d x =
    match d with
    | One (v, a) -> Two (monoid.combine (measure x) v, x, a)
    | Two (v, a, b) -> Three (monoid.combine (measure x) v, x, a, b)
    | Three (v, a, b, c) -> Four (monoid.combine (measure x) v, x, a, b, c)
    | Four _ -> assert false (*BISECT-VISIT*)

  let snoc_digit_node ~monoid d x =
    match d with
    | One (v, a) -> Two (monoid.combine v (measure_node x), a, x)
    | Two (v, a, b) -> Three (monoid.combine v (measure_node x), a, b, x)
    | Three (v, a, b, c) -> Four (monoid.combine v (measure_node x), a, b, c, x)
    | Four _ -> assert false (*BISECT-VISIT*)
  let snoc_digit ~monoid ~measure d x =
    match d with
    | One (v, a) -> Two (monoid.combine v (measure x), a, x)
    | Two (v, a, b) -> Three (monoid.combine v (measure x), a, b, x)
    | Three (v, a, b, c) -> Four (monoid.combine v (measure x), a, b, c, x)
    | Four _ -> assert false (*BISECT-VISIT*)

  let rec cons_aux : 'a 'm.
      monoid:'m monoid -> (('a, 'm) node, 'm) fg -> ('a, 'm) node -> (('a, 'm) node, 'm) fg =
    fun ~monoid t a ->
    match t with
    | Nil ->
      Single a
    | Single b ->
      deep ~monoid (one_node a) Nil (one_node b)
    | Deep (_, Four (_, b, c, d, e), m, sf) ->
      deep ~monoid (two_node ~monoid a b) (cons_aux ~monoid m (node3_node ~monoid c d e)) sf
    | Deep (v, pr, m, sf) ->
      Deep (monoid.combine (measure_node a) v, cons_digit_node ~monoid pr a, m, sf)
  let cons ~monoid ~measure t a =
    match t with
    | Nil ->
      Single a
    | Single b ->
      deep ~monoid (one ~measure a) Nil (one ~measure b)
    | Deep (_, Four (_, b, c, d, e), m, sf) ->
      deep ~monoid (two ~monoid ~measure a b) (cons_aux ~monoid m (node3 ~monoid ~measure c d e)) sf
    | Deep (v, pr, m, sf) ->
      Deep (monoid.combine (measure a) v, cons_digit ~monoid ~measure pr a, m, sf)

  let rec snoc_aux : 'a 'm.
      monoid:'m monoid -> (('a, 'm) node, 'm) fg -> ('a, 'm) node -> (('a, 'm) node, 'm) fg =
    fun ~monoid t a ->
    match t with
    | Nil ->
      Single a
    | Single b ->
      deep ~monoid (one_node b) Nil (one_node a)
    | Deep (_, pr, m, Four (_, b, c, d, e)) ->
      deep ~monoid pr (snoc_aux ~monoid m (node3_node ~monoid b c d)) (two_node ~monoid e a)
    | Deep (v, pr, m, sf) ->
      Deep (monoid.combine v (measure_node a), pr, m, snoc_digit_node ~monoid sf a)
  let snoc ~monoid ~measure t a =
    match t with
    | Nil ->
      Single a
    | Single b ->
      deep ~monoid (one ~measure b) Nil (one ~measure a)
    | Deep (_, pr, m, Four (_, b, c, d, e)) ->
      deep ~monoid pr (snoc_aux ~monoid m (node3 ~monoid ~measure b c d)) (two ~measure ~monoid e a)
    | Deep (v, pr, m, sf) ->
      Deep (monoid.combine v (measure a), pr, m, snoc_digit ~monoid ~measure sf a)

  (*---------------------------------*)
  (*     various conversions         *)
  (*---------------------------------*)
  let to_tree_digit_node ~monoid d =
    match d with
    | One (_, a) -> Single a
    | Two (v, a, b) -> Deep (v, one_node a, Nil, one_node b)
    | Three (v, a, b, c) -> Deep (v, two_node ~monoid a b, Nil, one_node c)
    | Four (v, a, b, c, d) -> Deep (v, three_node ~monoid a b c, Nil, one_node d)
  let to_tree_digit ~monoid ~measure d =
    match d with
    | One (_, a) -> Single a
    | Two (v, a, b) -> Deep (v, one ~measure a, Nil, one ~measure b)
    | Three (v, a, b, c) -> Deep (v, two ~monoid ~measure a b, Nil, one ~measure c)
    | Four (v, a, b, c, d) -> Deep (v, three ~monoid ~measure a b c, Nil, one ~measure d)
  let to_tree_list ~monoid ~measure = function
    | [] -> Nil
    | [a] -> Single a
    | [a; b] -> deep ~monoid (one ~measure a) Nil (one ~measure b)
    | [a; b; c] -> deep ~monoid (two ~monoid ~measure a b) Nil (one ~measure c)
    | [a; b; c; d] -> deep ~monoid (three ~monoid ~measure a b c) Nil (one ~measure d)
    | _ -> assert false (*BISECT-VISIT*)

  let to_digit_node = function
    | Node2 (v, a, b) -> Two (v, a, b)
    | Node3 (v, a, b, c) -> Three (v, a, b, c)
  let to_digit_list ~monoid ~measure = function
    | [a] -> one ~measure a
    | [a; b] -> two ~monoid ~measure a b
    | [a; b; c] -> three ~monoid ~measure a b c
    | [a; b; c; d] -> four ~monoid ~measure a b c d
    | _ -> assert false (*BISECT-VISIT*)
  let to_digit_list_node ~monoid = function
    | [a] -> one_node a
    | [a; b] -> two_node ~monoid a b
    | [a; b; c] -> three_node ~monoid a b c
    | [a; b; c; d] -> four_node ~monoid a b c d
    | _ -> assert false (*BISECT-VISIT*)

  (*---------------------------------*)
  (*     front / rear / etc.         *)
  (*---------------------------------*)
  let head_digit = function
    | One (_, a)
    | Two (_, a, _)
    | Three (_, a, _, _)
    | Four (_, a, _, _, _) -> a
  let last_digit = function
    | One (_, a)
    | Two (_, _, a)
    | Three (_, _, _, a)
    | Four (_, _, _, _, a) -> a
  let tail_digit_node ~monoid = function
    | One _ -> assert false (*BISECT-VISIT*)
    | Two (_, _, a) -> one_node a
    | Three (_, _, a, b) -> two_node ~monoid a b
    | Four (_, _, a, b, c) -> three_node ~monoid a b c
  let tail_digit ~monoid ~measure = function
    | One _ -> assert false (*BISECT-VISIT*)
    | Two (_, _, a) -> one ~measure a
    | Three (_, _, a, b) -> two ~monoid ~measure a b
    | Four (_, _, a, b, c) -> three ~monoid ~measure a b c
  let init_digit_node ~monoid = function
    | One _ -> assert false (*BISECT-VISIT*)
    | Two (_, a, _) -> one_node a
    | Three (_, a, b, _) -> two_node ~monoid a b
    | Four (_, a, b, c, _) -> three_node ~monoid a b c
  let init_digit ~monoid ~measure = function
    | One _ -> assert false (*BISECT-VISIT*)
    | Two (_, a, _) -> one ~measure a
    | Three (_, a, b, _) -> two ~monoid ~measure a b
    | Four (_, a, b, c, _) -> three ~monoid ~measure a b c

  type ('a, 'rest) view =
    | Vnil
    | Vcons of 'a * 'rest

  let rec view_left_aux : 'a 'm.
      monoid:'m monoid -> (('a, 'm) node, 'm) fg -> (('a, 'm) node, (('a, 'm) node, 'm) fg) view =
    fun ~monoid -> function
    | Nil -> Vnil
    | Single x -> Vcons (x, Nil)
    | Deep (_, One (_, a), m, sf) ->
      let vcons =
        match view_left_aux ~monoid m with
        | Vnil -> to_tree_digit_node ~monoid sf
        | Vcons (a, m') -> deep ~monoid (to_digit_node a) m' sf in
      Vcons (a, vcons)
    | Deep (_, pr, m, sf) ->
      let vcons = deep ~monoid (tail_digit_node ~monoid pr) m sf in
      Vcons (head_digit pr, vcons)
  let view_left ~monoid ~measure = function
    | Nil -> Vnil
    | Single x -> Vcons (x, Nil)
    | Deep (_, One (_, a), m, sf) ->
      let vcons =
        match view_left_aux ~monoid m with
        | Vnil -> to_tree_digit ~monoid ~measure sf
        | Vcons (a, m') -> deep ~monoid (to_digit_node a) m' sf in
      Vcons (a, vcons)
    | Deep (_, pr, m, sf) ->
      let vcons = deep ~monoid (tail_digit ~monoid ~measure pr) m sf in
      Vcons (head_digit pr, vcons)

  let rec view_right_aux : 'a 'm.
      monoid:'m monoid -> (('a, 'm) node, 'm) fg -> (('a, 'm) node, (('a, 'm) node, 'm) fg) view =
    fun ~monoid -> function
    | Nil -> Vnil
    | Single x -> Vcons (x, Nil)
    | Deep (_, pr, m, One (_, a)) ->
      let vcons =
        match view_right_aux ~monoid m with
        | Vnil -> to_tree_digit_node ~monoid pr
        | Vcons (a, m') -> deep ~monoid pr m' (to_digit_node a) in
      Vcons (a, vcons)
    | Deep (_, pr, m, sf) ->
      let vcons = deep ~monoid pr m (init_digit_node ~monoid sf) in
      Vcons (last_digit sf, vcons)
  let view_right ~monoid ~measure = function
    | Nil -> Vnil
    | Single x -> Vcons (x, Nil)
    | Deep (_, pr, m, One (_, a)) ->
      let vcons =
        match view_right_aux ~monoid m with
        | Vnil -> to_tree_digit ~monoid ~measure pr
        | Vcons (a, m') -> deep ~monoid pr m' (to_digit_node a) in
      Vcons (a, vcons)
    | Deep (_, pr, m, sf) ->
      let vcons = deep ~monoid pr m (init_digit ~monoid ~measure sf) in
      Vcons (last_digit sf, vcons)

  let head_exn = function
    | Nil -> raise Empty
    | Single a -> a
    | Deep (_, pr, _, _) -> head_digit pr
  let head = function
    | Nil -> None
    | Single a -> Some a
    | Deep (_, pr, _, _) -> Some (head_digit pr)

  let last_exn = function
    | Nil -> raise Empty
    | Single a -> a
    | Deep (_, _, _, sf) -> last_digit sf
  let last = function
    | Nil -> None
    | Single a -> Some a
    | Deep (_, _, _, sf) -> Some (last_digit sf)

  let tail ~monoid ~measure t =
    match view_left ~monoid ~measure t with
    | Vnil -> None
    | Vcons (_, tl) -> Some tl
  let tail_exn ~monoid ~measure t =
    match view_left ~monoid ~measure t with
    | Vnil -> raise Empty
    | Vcons (_, tl) -> tl

  let front ~monoid ~measure t =
    match view_left ~monoid ~measure t with
    | Vnil -> None
    | Vcons (hd, tl) -> Some (tl, hd)
  let front_exn ~monoid ~measure t =
    match view_left ~monoid ~measure t with
    | Vnil -> raise Empty
    | Vcons (hd, tl) -> (tl, hd)

  let init ~monoid ~measure t =
    match view_right ~monoid ~measure t with
    | Vnil -> None
    | Vcons (_, tl) -> Some tl
  let init_exn ~monoid ~measure t =
    match view_right ~monoid ~measure t with
    | Vnil -> raise Empty
    | Vcons (_, tl) -> tl

  let rear ~monoid ~measure t =
    match view_right ~monoid ~measure t with
    | Vnil -> None
    | Vcons (hd, tl) -> Some (tl, hd)
  let rear_exn ~monoid ~measure t =
    match view_right ~monoid ~measure t with
    | Vnil -> raise Empty
    | Vcons (hd, tl) -> (tl, hd)

  (*---------------------------------*)
  (*            append               *)
  (*---------------------------------*)
  let nodes =
    let add_digit_to digit l =
      match digit with
      | One (_, a) -> a :: l
      | Two (_, a, b) -> a :: b :: l
      | Three (_, a, b, c) -> a :: b :: c :: l
      | Four (_, a, b, c, d) -> a :: b :: c :: d :: l in

    let rec nodes_aux ~monoid ~measure ts sf2 = (* no idea if this should be tail rec *)
      match ts, sf2 with
      | [], One _ -> assert false (*BISECT-VISIT*)
      | [], Two (_, a, b)
      | [a], One (_, b) -> [node2 ~monoid ~measure a b]
      | [], Three (_, a, b, c)
      | [a], Two (_, b, c)
      | [a; b], One (_, c) -> [node3 ~monoid ~measure a b c]
      | [], Four (_, a, b, c, d)
      | [a], Three (_, b, c, d)
      | [a; b], Two (_, c, d)
      | [a; b; c], One (_, d) -> [node2 ~monoid ~measure a b; node2 ~monoid ~measure c d]
      | a :: b :: c :: ts, _ -> node3 ~monoid ~measure a b c :: nodes_aux ~monoid ~measure ts sf2
      | [a], Four (_, b, c, d, e)
      | [a; b], Three (_, c, d, e) -> [node3 ~monoid ~measure a b c; node2 ~monoid ~measure d e]
      | [a; b], Four (_, c, d, e, f) -> [node3 ~monoid ~measure a b c; node3 ~monoid ~measure d e f] in

    fun ~monoid ~measure sf1 ts sf2 ->
      let ts = add_digit_to sf1 ts in
      nodes_aux ~monoid ~measure ts sf2

  let rec app3 : 'a 'm.
      monoid:'m monoid -> measure:('a -> 'm) -> ('a, 'm) fg -> 'a list -> ('a, 'm) fg -> ('a, 'm) fg =
    fun ~monoid ~measure t1 elts t2 ->
    match t1, t2 with
    | Nil, _ ->
      List.fold_right (fun elt acc -> cons ~monoid ~measure acc elt) elts t2
    | _, Nil ->
      List.fold_left (fun acc elt -> snoc ~monoid ~measure acc elt) t1 elts
    | Single x1, _ ->
      cons ~monoid ~measure (List.fold_right (fun elt acc -> cons ~monoid ~measure acc elt) elts t2) x1
    | _, Single x2 ->
      snoc ~monoid ~measure (List.fold_left (fun acc elt -> snoc ~monoid ~measure acc elt) t1 elts) x2
    | Deep (_, pr1, m1, sf1), Deep (_, pr2, m2, sf2) ->
      deep ~monoid pr1 (app3 ~monoid ~measure:measure_node m1 (nodes ~monoid ~measure sf1 elts pr2) m2) sf2

  let append ~monoid ~measure t1 t2 = app3 ~monoid ~measure t1 [] t2

  (*---------------------------------*)
  (*            reverse              *)
  (*---------------------------------*)
  (* unfortunately, when reversing, we need to rebuild every annotation
   * because the monoid does not have to be commutative *)

  let reverse_digit_node ~monoid rev_a = function
    | One (_, a) -> one_node (rev_a a)
    | Two (_, a, b) -> two_node ~monoid (rev_a b) (rev_a a)
    | Three (_, a, b, c) -> three_node ~monoid (rev_a c) (rev_a b) (rev_a a)
    | Four (_, a, b, c, d) -> four_node ~monoid (rev_a d) (rev_a c) (rev_a b) (rev_a a)
  let reverse_digit ~monoid ~measure = function
    | One _ as d -> d
    | Two (_, a, b) -> two ~monoid ~measure b a
    | Three (_, a, b, c) -> three ~monoid ~measure c b a
    | Four (_, a, b, c, d) -> four ~monoid ~measure d c b a
  let reverse_node_node ~monoid rev_a = function
    | Node2 (_, a, b) -> node2_node ~monoid (rev_a b) (rev_a a)
    | Node3 (_, a, b, c) -> node3_node ~monoid (rev_a c) (rev_a b) (rev_a a)
  let reverse_node ~monoid ~measure = function
    | Node2 (_, a, b) -> node2 ~monoid ~measure b a
    | Node3 (_, a, b, c) -> node3 ~monoid ~measure c b a

  let rec reverse_aux : 'a 'm.
      monoid:'m monoid -> (('a, 'm) node -> ('a, 'm) node) -> (('a, 'm) node, 'm) fg -> (('a, 'm) node, 'm) fg =
    fun ~monoid reverse_a -> function
    | Nil -> Nil
    | Single a -> Single (reverse_a a)
    | Deep (_, pr, m, sf) ->
      let rev_pr = reverse_digit_node ~monoid reverse_a pr in
      let rev_sf = reverse_digit_node ~monoid reverse_a sf in
      let rev_m = reverse_aux ~monoid (reverse_node_node ~monoid (reverse_a)) m in
      deep ~monoid rev_sf rev_m rev_pr
  let reverse ~monoid ~measure = function
    | Nil
    | Single _ as t -> t
    | Deep (_, pr, m, sf) ->
      let rev_pr = reverse_digit ~monoid ~measure pr in
      let rev_sf = reverse_digit ~monoid ~measure sf in
      let rev_m = reverse_aux ~monoid (reverse_node ~monoid ~measure) m in
      deep ~monoid rev_sf rev_m rev_pr

  (*---------------------------------*)
  (*             split               *)
  (*---------------------------------*)
  type ('a, 'rest) split = Split of 'rest * 'a * 'rest
  let split_digit ~monoid ~measure p i = function
    | One (_, a) -> Split ([], a, [])
    | Two (_, a, b) ->
      let i' = monoid.combine i (measure a) in
      if p i' then Split ([], a, [b]) else
        Split ([a], b, [])
    | Three (_, a, b, c) ->
      let i' = monoid.combine i (measure a) in
      if p i' then Split ([], a, [b; c]) else
        let i'' = monoid.combine i' (measure b) in
        if p i'' then Split ([a], b, [c]) else
          Split ([a; b], c, [])
    | Four (_, a, b, c, d) ->
      let i' = monoid.combine i (measure a) in
      if p i' then Split ([], a, [b; c; d]) else
        let i'' = monoid.combine i' (measure b) in
        if p i'' then Split ([a], b, [c; d]) else
          let i''' = monoid.combine i'' (measure c) in
          if p i''' then Split ([a; b], c, [d]) else
            Split ([a; b; c], d, [])

  let deep_left ~monoid ~measure pr m sf =
    match pr with
    | [] -> (
      match view_left ~monoid ~measure:measure_node m with
      | Vnil -> to_tree_digit ~monoid ~measure sf
      | Vcons (a, m') -> deep ~monoid (to_digit_node a) m' sf
    )
    | _ ->
      deep ~monoid (to_digit_list ~monoid ~measure pr) m sf
  let deep_right ~monoid ~measure pr m sf =
    match sf with
    | [] -> (
      match view_right ~monoid ~measure:measure_node m with
      | Vnil -> to_tree_digit ~monoid ~measure pr
      | Vcons (a, m') -> deep ~monoid pr m' (to_digit_node a)
    )
    | _ ->
      deep ~monoid pr m (to_digit_list ~monoid ~measure sf)

  let rec split_tree : 'a 'm.
      monoid:'m monoid -> measure:('a -> 'm) -> ('m -> bool) -> 'm -> ('a, 'm) fg -> ('a, ('a, 'm) fg) split =
    fun ~monoid ~measure p i -> function
    | Nil -> raise Empty
    | Single x -> Split (Nil, x, Nil)
    | Deep (_, pr, m, sf) ->
      let vpr = monoid.combine i (measure_digit pr) in
      if p vpr then
        let Split (l, x, r) = split_digit ~monoid ~measure p i pr in
        Split (to_tree_list ~monoid ~measure l, x, deep_left ~monoid ~measure r m sf)
      else
        let vm = monoid.combine vpr (measure_t_node ~monoid m) in
        if p vm then
          let Split (ml, xs, mr) = split_tree ~monoid ~measure:measure_node p vpr m in
          let Split (l, x, r) = split_digit ~monoid ~measure p (monoid.combine vpr (measure_t_node ~monoid ml)) (to_digit_node xs) in
          Split (deep_right ~monoid ~measure pr ml l, x, deep_left ~monoid ~measure r mr sf)
        else
          let Split (l, x, r) = split_digit ~monoid ~measure p vm sf in
          Split (deep_right ~monoid ~measure pr m l, x, to_tree_list ~monoid ~measure r)

  let split ~monoid ~measure f t =
    match t with
    | Nil -> (Nil, Nil)
    | _ ->
      if f (measure_t ~monoid ~measure t) then
        let Split (l, x, r) = split_tree ~monoid ~measure f monoid.zero t in
        (l, cons ~monoid ~measure r x)
      else
        (t, Nil)

  (*---------------------------------*)
  (*            lookup               *)
  (*---------------------------------*)
  let lookup_digit ~monoid ~measure p i = function
    | One (_, a) -> monoid.zero, a
    | Two (_, a, b) ->
      let m_a = measure a in
      let i' = monoid.combine i m_a in
      if p i' then monoid.zero, a else m_a, b
    | Three (_, a, b, c) ->
      let m_a = measure a in
      let i' = monoid.combine i m_a in
      if p i' then monoid.zero, a else
        let m_b = measure b in
        let i'' = monoid.combine i' m_b in
        if p i'' then m_a, b else monoid.combine m_a m_b, c
    | Four (_, a, b, c, d) ->
      let m_a = measure a in
      let i' = monoid.combine i m_a in
      if p i' then monoid.zero, a else
        let m_b = measure b in
        let i'' = monoid.combine i' m_b in
        if p i'' then m_a, b else
          let m_c = measure c in
          let i''' = monoid.combine i'' m_c in
          if p i''' then monoid.combine m_a m_b, c else monoid.combine (monoid.combine m_a m_b) m_c, d

  let lookup_node ~monoid ~measure p i = function
    | Node2 (_, a, b) ->
      let m_a = measure a in
      let i' = monoid.combine i m_a in
      if p i' then monoid.zero, a else m_a, b
    | Node3 (_, a, b, c) ->
      let m_a = measure a in
      let i' = monoid.combine i m_a in
      if p i' then monoid.zero, a else
        let m_b = measure b in
        let i'' = monoid.combine i' m_b in
        if p i'' then m_a, b else monoid.combine m_a m_b, c

  let rec lookup_tree : 'a 'm. monoid:'m monoid -> measure:('a -> 'm) -> ('m -> bool) -> 'm -> ('a, 'm) fg -> 'm * 'a =
    fun ~monoid ~measure p i -> function
    | Nil -> raise Empty
    | Single x -> monoid.zero, x
    | Deep (_, pr, m, sf) ->
      let m_pr = measure_digit pr in
      let vpr = monoid.combine i m_pr in
      if p vpr then lookup_digit ~monoid ~measure p i pr else
        let m_m = measure_t_node ~monoid m in
        let vm = monoid.combine vpr m_m in
        if p vm then
          let v_left, node = lookup_tree ~monoid ~measure:measure_node p vpr m in
          let v, x = lookup_node ~monoid ~measure p (monoid.combine vpr v_left) node in
          monoid.combine (monoid.combine m_pr v_left) v, x
        else
          let v, x = lookup_digit ~monoid ~measure p vm sf in
          monoid.combine (monoid.combine m_pr m_m) v, x

  let lookup ~monoid ~measure p t =
    snd (lookup_tree ~monoid ~measure p monoid.zero t)

  (*---------------------------------*)
  (*        classic traversals       *)
  (*---------------------------------*)
  let iter f t =
    fold_left (fun () elt -> f elt) () t
  let iter_right f t =
    fold_right (fun () elt -> f elt) () t
  let map ~monoid ~measure f t = (* suboptimal when the measure does not depend on 'a *)
    fold_left (fun acc elt -> snoc ~monoid ~measure acc (f elt)) empty t
  let map_right ~monoid ~measure f t =
    fold_right (fun acc elt -> cons ~monoid ~measure acc (f elt)) empty t
end
