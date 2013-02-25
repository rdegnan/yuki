open Lwt
open Riak
open Riak_kv_piqi
open Ag_util
open Yuki_tree_j
open Yojson.Safe

exception Empty

module Make(Conn:Yuki_make.Conn)(Elem:Yuki_make.Elem)(Measure:Yuki_make.Measure with type t = Elem.t) = struct
  module Client = Client.Make(Conn)(Elem)
  open Client
  open Measure

  let empty = `Nil
  let is_empty = function `Nil -> true | _ -> false

  let reader = read_node read_string
  let writer = write_node write_string

  let get_fg_aux reader key =
    Conn.with_connection (fun conn ->
      match_lwt riak_get conn Elem.bucket key [] with
        | Some { obj_value = Some value } -> return (Json.from_string (read_fg reader) value)
        | _ -> raise_lwt Not_found
    )
  let get_fg reader = function
    | None -> return `Nil
    | Some m -> get_fg_aux reader m

  let put_fg_aux writer ?key ?(ops=[Put_return_head true; Put_if_none_match true]) x =
    Conn.with_connection (fun conn ->
      match_lwt riak_put conn Elem.bucket key (Json.to_string (write_fg writer) x) ops with
        | Some { obj_key = Some key } -> return key
        | _ -> (match key with
            | Some key -> return key
            | None -> raise_lwt Not_found
        )
    )
  let put_fg ?key writer = function
    | `Nil -> return None
    | m ->
      lwt m' = put_fg_aux writer ?key m in
      return (Some m')

  let get_elem key =
    lwt { value } = get key in
    return value

  let put_elem ?key x =
    lwt key = put ?key x [] in
    return { key; value = x; links = [] }

  (*---------------------------------*)
  (*              fold               *)
  (*---------------------------------*)
  let fold_left_node : 'acc 'a. ('acc -> 'a -> 'acc Lwt.t) -> 'acc -> 'a node -> 'acc Lwt.t = fun f acc -> function
    | `Node2 (_, a, b) ->
      lwt acc = f acc a in
      f acc b
    | `Node3 (_, a, b, c) ->
      lwt acc = f acc a in
      lwt acc = f acc b in
      f acc c
  let fold_right_node : 'acc 'a. ('a -> 'acc -> 'acc Lwt.t) -> 'acc -> 'a node -> 'acc Lwt.t = fun f acc -> function
    | `Node2 (_, a, b) ->
      lwt acc = f b acc in
      f a acc
    | `Node3 (_, a, b, c) ->
      lwt acc = f c acc in
      lwt acc = f b acc in
      f a acc

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
  let fold_right_digit : 'acc 'a. ('a -> 'acc -> 'acc Lwt.t) -> 'acc -> 'a digit -> 'acc Lwt.t = fun f acc -> function
    | `One (_, a) -> f a acc
    | `Two (_, a, b) ->
      lwt acc = f b acc in
      f a acc
    | `Three (_, a, b, c) ->
      lwt acc = f c acc in
      lwt acc = f b acc in
      f a acc
    | `Four (_, a, b, c, d) ->
      lwt acc = f d acc in
      lwt acc = f c acc in
      lwt acc = f b acc in
      f a acc

  let rec fold_left_aux : 'acc 'a. 'a Json.reader -> ('acc -> 'a -> 'acc Lwt.t) -> 'acc -> 'a fg -> 'acc Lwt.t = fun reader f acc -> function
    | `Nil -> return acc
    | `Single x -> f acc x
    | `Deep (_, pr, m, sf) ->
      let reader' = read_node reader in
      lwt acc = fold_left_digit f acc pr
      and m' = get_fg reader' m in
      lwt acc = fold_left_aux reader' (fun acc elt -> fold_left_node f acc elt) acc m' in
      fold_left_digit f acc sf
  let fold_left f =
    fold_left_aux read_string (fun acc elt ->
      lwt elt' = get_elem elt in
      f acc elt'
    )

  let rec fold_right_aux : 'acc 'a. 'a Json.reader -> ('a -> 'acc -> 'acc Lwt.t) -> 'acc -> 'a fg -> 'acc Lwt.t = fun reader f acc -> function
    | `Nil -> return acc
    | `Single x -> f x acc
    | `Deep (_, pr, m, sf) ->
      let reader' = read_node reader in
      lwt acc = fold_right_digit f acc sf
      and m' = get_fg reader' m in
      lwt acc = fold_right_aux reader' (fun elt acc -> fold_right_node f acc elt) acc m' in
      fold_right_digit f acc pr
  let fold_right f =
    fold_right_aux read_string (fun elt acc ->
      lwt elt' = get_elem elt in
      f elt' acc
    )

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
  let measure_t = function
    | `Nil -> return Monoid.zero
    | `Single x ->
      lwt x' = get_elem x in
      return (measure x')
    | `Deep (v, _, _, _) -> return (Monoid.of_string v)

  (*---------------------------------*)
  (*  a bunch of smart constructors  *)
  (*---------------------------------*)
  let singleton a = `Single a.key

  let node2_node : 'a. 'a node -> 'a node -> 'a node node = fun a b ->
    `Node2 (Monoid.to_string (Monoid.combine (measure_node a) (measure_node b)), a, b)
  let node2 a b =
    `Node2 (Monoid.to_string (Monoid.combine (measure a.value) (measure b.value)), a.key, b.key)

  let node3_node : 'a. 'a node -> 'a node -> 'a node -> 'a node node = fun a b c ->
    `Node3 (Monoid.to_string (Monoid.combine (measure_node a) (Monoid.combine (measure_node b) (measure_node c))), a, b, c)
  let node3 a b c =
    `Node3 (Monoid.to_string (Monoid.combine (measure a.value) (Monoid.combine (measure b.value) (measure c.value))), a.key, b.key, c.key)

  let deep : 'a. 'a node Json.writer -> 'a digit -> 'a node fg -> 'a digit -> 'a fg Lwt.t = fun writer pr m sf ->
    lwt m' = put_fg writer m in
    return (`Deep (Monoid.to_string (Monoid.combine (Monoid.combine (measure_digit pr) (measure_t_node m)) (measure_digit sf)), pr, m', sf))

  let one_node : 'a. 'a node -> 'a node digit = fun a ->
    `One (Monoid.to_string (measure_node a), a)
  let one a =
    `One (Monoid.to_string (measure a.value), a.key)

  let two_node : 'a. 'a node -> 'a node -> 'a node digit = fun a b ->
    `Two (Monoid.to_string (Monoid.combine (measure_node a) (measure_node b)), a, b)
  let two a b =
    `Two (Monoid.to_string (Monoid.combine (measure a.value) (measure b.value)), a.key, b.key)

  let three_node : 'a. 'a node -> 'a node -> 'a node -> 'a node digit = fun a b c ->
    `Three (Monoid.to_string (Monoid.combine (Monoid.combine (measure_node a) (measure_node b)) (measure_node c)), a, b, c)
  let three a b c =
    `Three (Monoid.to_string (Monoid.combine (Monoid.combine (measure a.value) (measure b.value)) (measure c.value)), a.key, b.key, c.key)

  let four_node : 'a. 'a node -> 'a node -> 'a node -> 'a node -> 'a node digit = fun a b c d ->
    `Four (Monoid.to_string (Monoid.combine (Monoid.combine (measure_node a) (measure_node b)) (Monoid.combine (measure_node c) (measure_node d))), a, b, c, d)
  let four a b c d =
    `Four (Monoid.to_string (Monoid.combine (Monoid.combine (measure a.value) (measure b.value)) (Monoid.combine (measure c.value) (measure d.value))), a.key, b.key, c.key, d.key)

  (*---------------------------------*)
  (*          cons / snoc            *)
  (*---------------------------------*)
  let cons_digit_node : 'a. 'a node -> 'a node digit -> 'a node digit = fun x -> function
    | `One (v, a) -> `Two (Monoid.to_string (Monoid.combine (measure_node x) (Monoid.of_string v)), x, a)
    | `Two (v, a, b) -> `Three (Monoid.to_string (Monoid.combine (measure_node x) (Monoid.of_string v)), x, a, b)
    | `Three (v, a, b, c) -> `Four (Monoid.to_string (Monoid.combine (measure_node x) (Monoid.of_string v)), x, a, b, c)
    | `Four _ -> assert false
  let cons_digit x = function
    | `One (v, a) -> `Two (Monoid.to_string (Monoid.combine (measure x.value) (Monoid.of_string v)), x.key, a)
    | `Two (v, a, b) -> `Three (Monoid.to_string (Monoid.combine (measure x.value) (Monoid.of_string v)), x.key, a, b)
    | `Three (v, a, b, c) -> `Four (Monoid.to_string (Monoid.combine (measure x.value) (Monoid.of_string v)), x.key, a, b, c)
    | `Four _ -> assert false

  let snoc_digit_node : 'a. 'a node -> 'a node digit -> 'a node digit = fun x -> function
    | `One (v, a) -> `Two (Monoid.to_string (Monoid.combine (Monoid.of_string v) (measure_node x)), a, x)
    | `Two (v, a, b) -> `Three (Monoid.to_string (Monoid.combine (Monoid.of_string v) (measure_node x)), a, b, x)
    | `Three (v, a, b, c) -> `Four (Monoid.to_string (Monoid.combine (Monoid.of_string v) (measure_node x)), a, b, c, x)
    | `Four _ -> assert false
  let snoc_digit x = function
    | `One (v, a) -> `Two (Monoid.to_string (Monoid.combine (Monoid.of_string v) (measure x.value)), a, x.key)
    | `Two (v, a, b) -> `Three (Monoid.to_string (Monoid.combine (Monoid.of_string v) (measure x.value)), a, b, x.key)
    | `Three (v, a, b, c) -> `Four (Monoid.to_string (Monoid.combine (Monoid.of_string v) (measure x.value)), a, b, c, x.key)
    | `Four _ -> assert false

  let rec cons_aux : 'a. 'a node Json.reader -> 'a node Json.writer -> 'a node -> 'a node fg -> 'a node fg Lwt.t = fun reader writer a -> function
    | `Nil ->
      return (`Single a)
    | `Single b ->
      deep (write_node writer) (one_node a) `Nil (one_node b)
    | `Deep (_, `Four (_, b, c, d, e), m, sf) ->
      let reader' = read_node reader and writer' = write_node writer in
      lwt m' = get_fg reader' m in
      lwt m'' = cons_aux reader' writer' (node3_node c d e) m' in
      deep writer' (two_node a b) m'' sf
    | `Deep (v, pr, m, sf) ->
      return (`Deep (Monoid.to_string (Monoid.combine (measure_node a) (Monoid.of_string v)), cons_digit_node a pr, m, sf))
  let cons a = function
    | `Nil ->
      return (singleton a)
    | `Single b ->
      lwt b' = get b in
      deep writer (one a) `Nil (one b')
    | `Deep (_, `Four (_, b, c, d, e), m, sf) ->
      lwt b' = get b and c' = get c and d' = get d and e' = get e and m' = get_fg reader m in
      lwt m'' = cons_aux reader writer (node3 c' d' e') m' in
      deep writer (two a b') m'' sf
    | `Deep (v, pr, m, sf) ->
      return (`Deep (Monoid.to_string (Monoid.combine (measure a.value) (Monoid.of_string v)), cons_digit a pr, m, sf))

  let rec snoc_aux : 'a. 'a node Json.reader -> 'a node Json.writer -> 'a node -> 'a node fg -> 'a node fg Lwt.t = fun reader writer a -> function
    | `Nil ->
      return (`Single a)
    | `Single b ->
      deep (write_node writer) (one_node b) `Nil (one_node a)
    | `Deep (_, pr, m, `Four (_, b, c, d, e)) ->
      let reader' = read_node reader and writer' = write_node writer in
      lwt m' = get_fg reader' m in
      lwt m'' = snoc_aux reader' writer' (node3_node b c d) m' in
      deep writer' pr m'' (two_node e a)
    | `Deep (v, pr, m, sf) ->
      return (`Deep (Monoid.to_string (Monoid.combine (Monoid.of_string v) (measure_node a)), pr, m, snoc_digit_node a sf))
  let snoc a = function
    | `Nil ->
      return (singleton a)
    | `Single b ->
      lwt b' = get b in
      deep writer (one b') `Nil (one a)
    | `Deep (_, pr, m, `Four (_, b, c, d, e)) ->
      lwt b' = get b and c' = get c and d' = get d and e' = get e and m' = get_fg reader m in
      lwt m'' = snoc_aux reader writer (node3 b' c' d') m' in
      deep writer pr m'' (two e' a)
    | `Deep (v, pr, m, sf) ->
      return (`Deep (Monoid.to_string (Monoid.combine (Monoid.of_string v) (measure a.value)), pr, m, snoc_digit a sf))

  (*---------------------------------*)
  (*     various conversions         *)
  (*---------------------------------*)
  let to_tree_digit_node : 'a. 'a node digit -> 'a node fg = function
    | `One (_, a) -> `Single a
    | `Two (v, a, b) -> `Deep (v, one_node a, None, one_node b)
    | `Three (v, a, b, c) -> `Deep (v, two_node a b, None, one_node c)
    | `Four (v, a, b, c, d) -> `Deep (v, three_node a b c, None, one_node d)
  let to_tree_digit = function
    | `One (_, a) ->
      return (`Single a)
    | `Two (v, a, b) ->
      lwt a' = get a and b' = get b in
      return (`Deep (v, one a', None, one b'))
    | `Three (v, a, b, c) ->
      lwt a' = get a and b' = get b and c' = get c in
      return (`Deep (v, two a' b', None, one c'))
    | `Four (v, a, b, c, d) ->
      lwt a' = get a and b' = get b and c' = get c and d' = get d in
      return (`Deep (v, three a' b' c', None, one d'))

  let to_tree_list_node : 'a. 'a node list -> 'a node fg = function
    | [] -> `Nil
    | [a] -> `Single a
    | [a; b] ->
      let m_pr = measure_node a and m_sf = measure_node b in
      `Deep (Monoid.to_string (Monoid.combine m_pr m_sf), `One (Monoid.to_string m_pr, a), None, `One (Monoid.to_string m_sf, b))
    | [a; b; c] ->
      let m_pr = Monoid.combine (measure_node a) (measure_node b) and m_sf = measure_node c in
      `Deep (Monoid.to_string (Monoid.combine m_pr m_sf), `Two (Monoid.to_string m_pr, a, b), None, `One (Monoid.to_string m_sf, c))
    | [a; b; c; d] ->
      let m_pr = Monoid.combine (Monoid.combine (measure_node a) (measure_node b)) (measure_node c) and m_sf = measure_node d in
      `Deep (Monoid.to_string (Monoid.combine m_pr m_sf), `Three (Monoid.to_string m_pr, a, b, c), None, `One (Monoid.to_string m_sf, d))
    | _ -> assert false
  let to_tree_list = function
    | [] -> return `Nil
    | [a] -> return (`Single a)
    | [a; b] ->
      lwt a' = get_elem a and b' = get_elem b in
      let m_pr = measure a' and m_sf = measure b' in
      return (`Deep (Monoid.to_string (Monoid.combine m_pr m_sf), `One (Monoid.to_string m_pr, a), None, `One (Monoid.to_string m_sf, b)))
    | [a; b; c] ->
      lwt a' = get_elem a and b' = get_elem b and c' = get_elem c in
      let m_pr = Monoid.combine (measure a') (measure b') and m_sf = measure c' in
      return (`Deep (Monoid.to_string (Monoid.combine m_pr m_sf), `Two (Monoid.to_string m_pr, a, b), None, `One (Monoid.to_string m_sf, c)))
    | [a; b; c; d] ->
      lwt a' = get_elem a and b' = get_elem b and c' = get_elem c and d' = get_elem d in
      let m_pr = Monoid.combine (Monoid.combine (measure a') (measure b')) (measure c') and m_sf = measure d' in
      return (`Deep (Monoid.to_string (Monoid.combine m_pr m_sf), `Three (Monoid.to_string m_pr, a, b, c), None, `One (Monoid.to_string m_sf, d)))
    | _ -> assert false

  let to_digit_node : 'a. 'a node -> 'a digit = function
    | `Node2 (v, a, b) -> `Two (v, a, b)
    | `Node3 (v, a, b, c) -> `Three (v, a, b, c)
  let to_digit_list = function
    | [a] ->
      lwt a' = get a in
      return (one a')
    | [a; b] ->
      lwt a' = get a and b' = get b in
      return (two a' b')
    | [a; b; c] ->
      lwt a' = get a and b' = get b and c' = get c in
      return (three a' b' c')
    | [a; b; c; d] ->
      lwt a' = get a and b' = get b and c' = get c and d' = get d in
      return (four a' b' c' d')
    | _ -> assert false
  let to_digit_list_node : 'a. 'a node list -> 'a node digit = function
    | [a] -> one_node a
    | [a; b] -> two_node a b
    | [a; b; c] -> three_node a b c
    | [a; b; c; d] -> four_node a b c d
    | _ -> assert false

  let to_list_digit : 'a. 'a digit -> 'a list = function
    | `One (_, a) -> [a]
    | `Two (_, a, b) -> [a; b]
    | `Three (_, a, b, c) -> [a; b; c]
    | `Four (_, a, b, c, d) -> [a; b; c; d]

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
  let tail_digit = function
    | `One _ -> assert false
    | `Two (_, _, a) ->
      lwt a' = get a in
      return (one a')
    | `Three (_, _, a, b) ->
      lwt a' = get a and b' = get b in
      return (two a' b')
    | `Four (_, _, a, b, c) ->
      lwt a' = get a and b' = get b and c' = get c in
      return (three a' b' c')
  let init_digit_node : 'a. 'a node digit -> 'a node digit = function
    | `One _ -> assert false
    | `Two (_, a, _) -> one_node a
    | `Three (_, a, b, _) -> two_node a b
    | `Four (_, a, b, c, _) -> three_node a b c
  let init_digit = function
    | `One _ -> assert false
    | `Two (_, a, _) ->
      lwt a' = get a in
      return (one a')
    | `Three (_, a, b, _) ->
      lwt a' = get a and b' = get b in
      return (two a' b')
    | `Four (_, a, b, c, _) ->
      lwt a' = get a and b' = get b and c' = get c in
      return (three a' b' c')

  type 'a view =
    | Vnil
    | Vcons of 'a * 'a fg

  let rec view_left_aux : 'a. 'a node Json.reader -> 'a node Json.writer -> 'a node fg -> 'a node view Lwt.t = fun reader writer -> function
    | `Nil -> return Vnil
    | `Single x -> return (Vcons (x, `Nil))
    | `Deep (_, `One (_, a), m, sf) ->
      let reader' = read_node reader and writer' = write_node writer in
      lwt m' = get_fg reader' m in
      lwt m'' = view_left_aux reader' writer' m' in
      lwt vcons =
        match m'' with
        | Vnil -> return (to_tree_digit_node sf)
        | Vcons (a, m') -> deep writer' (to_digit_node a) m' sf in
      return (Vcons (a, vcons))
    | `Deep (_, pr, m, sf) ->
      let reader' = read_node reader and writer' = write_node writer in
      lwt m' = get_fg reader' m in
      lwt vcons = deep writer' (tail_digit_node pr) m' sf in
      return (Vcons (head_digit pr, vcons))
  let view_left = function
    | `Nil -> return Vnil
    | `Single x -> return (Vcons (x, `Nil))
    | `Deep (_, `One (_, a), m, sf) ->
      lwt m' = get_fg reader m in
      lwt m'' = view_left_aux reader writer m' in
      lwt vcons =
        match m'' with
        | Vnil -> to_tree_digit sf
        | Vcons (a, m') -> deep writer (to_digit_node a) m' sf in
      return (Vcons (a, vcons))
    | `Deep (_, pr, m, sf) ->
      lwt m' = get_fg reader m and pr' = tail_digit pr in
      lwt vcons = deep writer pr' m' sf in
      return (Vcons (head_digit pr, vcons))

  let rec view_right_aux : 'a. 'a node Json.reader -> 'a node Json.writer -> 'a node fg -> 'a node view Lwt.t = fun reader writer -> function
    | `Nil -> return Vnil
    | `Single x -> return (Vcons (x, `Nil))
    | `Deep (_, pr, m, `One (_, a)) ->
      let reader' = read_node reader and writer' = write_node writer in
      lwt m' = get_fg reader' m in
      lwt m'' = view_right_aux reader' writer' m' in
      lwt vcons =
        match m'' with
        | Vnil -> return (to_tree_digit_node pr)
        | Vcons (a, m') -> deep writer' pr m' (to_digit_node a) in
      return (Vcons (a, vcons))
    | `Deep (_, pr, m, sf) ->
      let reader' = read_node reader and writer' = write_node writer in
      lwt m' = get_fg reader' m in
      lwt vcons = deep writer' pr m' (init_digit_node sf) in
      return (Vcons (last_digit sf, vcons))
  let view_right = function
    | `Nil -> return Vnil
    | `Single x -> return (Vcons (x, `Nil))
    | `Deep (_, pr, m, `One (_, a)) ->
      lwt m' = get_fg reader m in
      lwt m'' = view_right_aux reader writer m' in
      lwt vcons =
        match m'' with
        | Vnil -> to_tree_digit pr
        | Vcons (a, m') -> deep writer pr m' (to_digit_node a) in
      return (Vcons (a, vcons))
    | `Deep (_, pr, m, sf) ->
      lwt m' = get_fg reader m and sf' = init_digit sf in
      lwt vcons = deep writer pr m' sf' in
      return (Vcons (last_digit sf, vcons))

  let head = function
    | `Nil -> raise_lwt Empty
    | `Single a -> get_elem a
    | `Deep (_, pr, _, _) -> get_elem (head_digit pr)

  let last = function
    | `Nil -> raise_lwt Empty
    | `Single a -> get_elem a
    | `Deep (_, _, _, sf) -> get_elem (last_digit sf)

  let front t =
    match_lwt view_left t with
    | Vnil -> raise_lwt Empty
    | Vcons (hd, tl) ->
      lwt hd' = get_elem hd in
      return (hd', tl)

  let rear t =
    match_lwt view_right t with
    | Vnil -> raise_lwt Empty
    | Vcons (hd, tl) ->
      lwt hd' = get_elem hd in
      return (hd', tl)

  (*---------------------------------*)
  (*            append               *)
  (*---------------------------------*)
  let rec nodes_node : 'a. 'a node list -> 'a node digit -> 'a node node list = fun ts sf2 ->
    match ts, sf2 with
    | [], `One _ -> assert false
    | [], `Two (_, a, b)
    | [a], `One (_, b) -> [node2_node a b]
    | [], `Three (_, a, b, c)
    | [a], `Two (_, b, c)
    | [a; b], `One (_, c) -> [node3_node a b c]
    | [], `Four (_, a, b, c, d)
    | [a], `Three (_, b, c, d)
    | [a; b], `Two (_, c, d)
    | [a; b; c], `One (_, d) -> [node2_node a b; node2_node c d]
    | a :: b :: c :: ts, _ -> node3_node a b c :: nodes_node ts sf2
    | [a], `Four (_, b, c, d, e)
    | [a; b], `Three (_, c, d, e) -> [node3_node a b c; node2_node d e]
    | [a; b], `Four (_, c, d, e, f) -> [node3_node a b c; node3_node d e f]

  let rec nodes ts sf2 =
    match ts, sf2 with
    | [], `One _ -> assert false
    | [], `Two (_, a, b)
    | [a], `One (_, b) ->
      lwt a' = get a and b' = get b in
      return [node2 a' b']
    | [], `Three (_, a, b, c)
    | [a], `Two (_, b, c)
    | [a; b], `One (_, c) ->
      lwt a' = get a and b' = get b and c' = get c in
      return [node3 a' b' c']
    | [], `Four (_, a, b, c, d)
    | [a], `Three (_, b, c, d)
    | [a; b], `Two (_, c, d)
    | [a; b; c], `One (_, d) ->
      lwt a' = get a and b' = get b and c' = get c and d' = get d in
      return [node2 a' b'; node2 c' d']
    | a :: b :: c :: ts, _ ->
      lwt a' = get a and b' = get b and c' = get c and ts' = nodes ts sf2 in
      return (node3 a' b' c' :: ts')
    | [a], `Four (_, b, c, d, e)
    | [a; b], `Three (_, c, d, e) ->
      lwt a' = get a and b' = get b and c' = get c and d' = get d and e' = get e in
      return [node3 a' b' c'; node2 d' e']
    | [a; b], `Four (_, c, d, e, f) ->
      lwt a' = get a and b' = get b and c' = get c and d' = get d and e' = get e and f' = get f in
      return [node3 a' b' c'; node3 d' e' f']

  let rec append_aux : 'a. 'a node Json.reader -> 'a node Json.writer -> 'a node fg -> 'a node list -> 'a node fg -> 'a node fg Lwt.t = fun reader writer t1 elts t2 ->
    match t1, t2 with
    | `Nil, _ ->
      Lwt_list.fold_right_s (fun elt acc -> cons_aux reader writer elt acc) elts t2
    | _, `Nil ->
      Lwt_list.fold_left_s (fun acc elt -> snoc_aux reader writer elt acc) t1 elts
    | `Single x1, _ ->
      lwt t = Lwt_list.fold_right_s (fun elt acc -> cons_aux reader writer elt acc) elts t2 in
      cons_aux reader writer x1 t
    | _, `Single x2 ->
      lwt t = Lwt_list.fold_left_s (fun acc elt -> snoc_aux reader writer elt acc) t1 elts in
      snoc_aux reader writer x2 t
    | `Deep (_, pr1, m1, sf1), `Deep (_, pr2, m2, sf2) ->
      let reader' = read_node reader and writer' = write_node writer in
      lwt m1' = get_fg reader' m1 and m2' = get_fg reader' m2 in
      let ts = nodes_node (to_list_digit sf1 @ elts) pr2 in
      lwt m = append_aux reader' writer' m1' ts m2' in
      deep writer' pr1 m sf2
  let append t1 t2 =
    match t1, t2 with
    | `Nil, _ -> return t2
    | _, `Nil -> return t1
    | `Single x1, _ ->
      lwt x1' = get x1 in
      cons x1' t2
    | _, `Single x2 ->
      lwt x2' = get x2 in
      snoc x2' t1
    | `Deep (_, pr1, m1, sf1), `Deep (_, pr2, m2, sf2) ->
      lwt m1' = get_fg reader m1 and m2' = get_fg reader m2
      and ts = nodes (to_list_digit sf1) pr2 in
      lwt m = append_aux reader writer m1' ts m2' in
      deep writer pr1 m sf2

  (*---------------------------------*)
  (*            reverse              *)
  (*---------------------------------*)
  let reverse_digit_node : 'a. ('a node -> 'a node Lwt.t) -> 'a node digit -> 'a node digit Lwt.t = fun rev_a -> function
    | `One (_, a) ->
      lwt a' = rev_a a in
      return (one_node a')
    | `Two (_, a, b) ->
      lwt a' = rev_a a and b' = rev_a b in
      return (two_node b' a')
    | `Three (_, a, b, c) ->
      lwt a' = rev_a a and b' = rev_a b and c' = rev_a c in
      return (three_node c' b' a')
    | `Four (_, a, b, c, d) ->
      lwt a' = rev_a a and b' = rev_a b and c' = rev_a c and d' = rev_a d in
      return (four_node d' c' b' a')
  let reverse_digit = function
    | `One _ as d -> return d
    | `Two (_, a, b) ->
      lwt a' = get a and b' = get b in
      return (two b' a')
    | `Three (_, a, b, c) ->
      lwt a' = get a and b' = get b and c' = get c in
      return (three c' b' a')
    | `Four (_, a, b, c, d) ->
      lwt a' = get a and b' = get b and c' = get c and d' = get d in
      return (four d' c' b' a')
  let reverse_node_node : 'a. ('a node -> 'a node Lwt.t) -> 'a node node -> 'a node node Lwt.t = fun rev_a -> function
    | `Node2 (_, a, b) ->
      lwt a' = rev_a a and b' = rev_a b in
      return (node2_node b' a')
    | `Node3 (_, a, b, c) ->
      lwt a' = rev_a a and b' = rev_a b and c' = rev_a c in
      return (node3_node c' b' a')
  let reverse_node = function
    | `Node2 (_, a, b) ->
      lwt a' = get a and b' = get b in
      return (node2 b' a')
    | `Node3 (_, a, b, c) ->
      lwt a' = get a and b' = get b and c' = get c in
      return (node3 c' b' a')

  let rec reverse_aux : 'a. 'a node Json.reader -> 'a node Json.writer -> ('a node -> 'a node Lwt.t) -> 'a node fg -> 'a node fg Lwt.t = fun reader writer reverse_a -> function
    | `Nil -> return `Nil
    | `Single a ->
      lwt a' = reverse_a a in
      return (`Single a')
    | `Deep (_, pr, m, sf) ->
      let reader' = read_node reader and writer' = write_node writer in
      lwt rev_pr = reverse_digit_node reverse_a pr and rev_sf = reverse_digit_node reverse_a sf in
      lwt m' = get_fg reader' m in
      lwt rev_m = reverse_aux reader' writer' (reverse_node_node (reverse_a)) m' in
      deep writer' rev_sf rev_m rev_pr
  let reverse = function
    | `Nil
    | `Single _ as t -> return t
    | `Deep (_, pr, m, sf) ->
      lwt rev_pr = reverse_digit pr and rev_sf = reverse_digit sf in
      lwt m' = get_fg reader m in
      lwt rev_m = reverse_aux reader writer reverse_node m' in
      deep writer rev_sf rev_m rev_pr

  (*---------------------------------*)
  (*             split               *)
  (*---------------------------------*)
  let split_digit_node : 'a. (Monoid.t -> int) -> Monoid.t -> 'a node digit -> 'a node list * 'a node * 'a node list = fun f i -> function
    | `One (_, a) -> ([], a, [])
    | `Two (_, a, b) ->
      let i' = Monoid.combine i (measure_node a) in
      if f i' <= 0 then ([], a, [b]) else
        ([a], b, [])
    | `Three (_, a, b, c) ->
      let i' = Monoid.combine i (measure_node a) in
      if f i' <= 0 then ([], a, [b; c]) else
        let i'' = Monoid.combine i' (measure_node b) in
        if f i'' <= 0 then ([a], b, [c]) else
          ([a; b], c, [])
    | `Four (_, a, b, c, d) ->
      let i' = Monoid.combine i (measure_node a) in
      if f i' <= 0 then ([], a, [b; c; d]) else
        let i'' = Monoid.combine i' (measure_node b) in
        if f i'' <= 0 then ([a], b, [c; d]) else
          let i''' = Monoid.combine i'' (measure_node c) in
          if f i''' <= 0 then ([a; b], c, [d]) else
            ([a; b; c], d, [])
  let split_digit f i = function
    | `One (_, a) -> return ([], a, [])
    | `Two (_, a, b) ->
      lwt a' = get_elem a in
      let i' = Monoid.combine i (measure a') in
      if f i' <= 0 then return ([], a, [b]) else
        return ([a], b, [])
    | `Three (_, a, b, c) ->
      lwt a' = get_elem a in
      let i' = Monoid.combine i (measure a') in
      if f i' <= 0 then return ([], a, [b; c]) else
        lwt b' = get_elem b in
        let i'' = Monoid.combine i' (measure b') in
        if f i'' <= 0 then return ([a], b, [c]) else
          return ([a; b], c, [])
    | `Four (_, a, b, c, d) ->
      lwt a' = get_elem a in
      let i' = Monoid.combine i (measure a') in
      if f i' <= 0 then return ([], a, [b; c; d]) else
        lwt b' = get_elem b in
        let i'' = Monoid.combine i' (measure b') in
        if f i'' <= 0 then return ([a], b, [c; d]) else
          lwt c' = get_elem c in
          let i''' = Monoid.combine i'' (measure c') in
          if f i''' <= 0 then return ([a; b], c, [d]) else
            return ([a; b; c], d, [])

  let deep_left_node : 'a. 'a node node Json.reader -> 'a node node Json.writer -> 'a node list -> 'a node node fg -> 'a node digit -> 'a node fg Lwt.t = fun reader writer pr m sf ->
    match pr with
    | [] -> (
      match_lwt view_left_aux reader writer m with
      | Vnil -> return (to_tree_digit_node sf)
      | Vcons (a, m') -> deep writer (to_digit_node a) m' sf
    )
    | _ ->
      deep writer (to_digit_list_node pr) m sf
  let deep_left pr m sf =
    match pr with
    | [] -> (
      match_lwt view_left_aux reader writer m with
      | Vnil -> to_tree_digit sf
      | Vcons (a, m') -> deep writer (to_digit_node a) m' sf
    )
    | _ ->
      lwt pr' = to_digit_list pr in
      deep writer pr' m sf

  let deep_right_node : 'a. 'a node node Json.reader -> 'a node node Json.writer -> 'a node digit -> 'a node node fg -> 'a node list -> 'a node fg Lwt.t = fun reader writer pr m sf ->
    match sf with
    | [] -> (
      match_lwt view_right_aux reader writer m with
      | Vnil -> return (to_tree_digit_node pr)
      | Vcons (a, m') -> deep writer pr m' (to_digit_node a)
    )
    | _ ->
      deep writer pr m (to_digit_list_node sf)
  let deep_right pr m sf =
    match sf with
    | [] -> (
      match_lwt view_right_aux reader writer m with
      | Vnil -> to_tree_digit pr
      | Vcons (a, m') -> deep writer pr m' (to_digit_node a)
    )
    | _ ->
      lwt sf' = to_digit_list sf in
      deep writer pr m sf'

  let rec split_tree_aux : 'a. 'a node Json.reader -> 'a node Json.writer -> (Monoid.t -> int) -> Monoid.t -> 'a node fg -> ('a node fg * 'a node * 'a node fg) Lwt.t = fun reader writer f i -> function
    | `Nil -> raise_lwt Empty
    | `Single x -> return (`Nil, x, `Nil)
    | `Deep (_, pr, m, sf) ->
      let vpr = Monoid.combine i (measure_digit pr) in
      let reader' = read_node reader and writer' = write_node writer in
      lwt m' = get_fg reader' m in
      if f vpr <= 0 then
        let (l, x, r) = split_digit_node f i pr in
        lwt r' = deep_left_node reader' writer' r m' sf in
        return (to_tree_list_node l, x, r')
      else
        let vm = Monoid.combine vpr (measure_t_node m') in
        if f vm <= 0 then
          lwt (ml, xs, mr) = split_tree_aux reader' writer' f vpr m' in
          let (l, x, r) = split_digit_node f (Monoid.combine vpr (measure_t_node ml)) (to_digit_node xs) in
          lwt l' = deep_right_node reader' writer' pr ml l and r' = deep_left_node reader' writer' r mr sf in
          return (l', x, r')
        else
          let (l, x, r) = split_digit_node f vm sf in
          lwt l' = deep_right_node reader' writer' pr m' l in
          return (l', x, to_tree_list_node r)
  let split_tree f = function
    | `Nil -> raise_lwt Empty
    | `Single x -> return (`Nil, x, `Nil)
    | `Deep (_, pr, m, sf) ->
      let vpr = measure_digit pr in
      lwt m' = get_fg reader m in
      if f vpr <= 0 then
        lwt (l, x, r) = split_digit f Monoid.zero pr in
        lwt l' = to_tree_list l and r' = deep_left r m' sf in
        return (l', x, r')
      else
        let vm = Monoid.combine vpr (measure_t_node m') in
        if f vm <= 0 then
          lwt (ml, xs, mr) = split_tree_aux reader writer f vpr m' in
          lwt (l, x, r) = split_digit f (Monoid.combine vpr (measure_t_node ml)) (to_digit_node xs) in
          lwt l' = deep_right pr ml l and r' = deep_left r mr sf in
          return (l', x, r')
        else
          lwt (l, x, r) = split_digit f vm sf in
          lwt l' = deep_right pr m' l and r' = to_tree_list r in
          return (l', x, r')

  let split f = function
    | `Nil -> return (`Nil, `Nil)
    | t ->
      lwt m_t = measure_t t in
      if f m_t <= 0 then
        lwt (l, x, r) = split_tree f t in
        lwt x' = get x in
        lwt r' = cons x' r in
        return (l, r')
      else
        return (t, `Nil)

  let insert x f t =
    lwt (l, r) = split f t in
    lwt r' = cons x r in
    append l r'

  (*---------------------------------*)
  (*             find                *)
  (*---------------------------------*)
  let find_digit f i = function
    | `One (_, a) ->
      lwt a' = get_elem a in
      let i' = Monoid.combine i (measure a') in
      if f i' = 0 then return ([], a, []) else
        raise_lwt Not_found
    | `Two (_, a, b) ->
      lwt a' = get_elem a in
      let i' = Monoid.combine i (measure a') in
      if f i' = 0 then return ([], a, [b]) else
        lwt b' = get_elem b in
        let i'' = Monoid.combine i' (measure b') in
        if f i'' = 0 then return ([a], b, []) else
          raise_lwt Not_found
    | `Three (_, a, b, c) ->
      lwt a' = get_elem a in
      let i' = Monoid.combine i (measure a') in
      if f i' = 0 then return ([], a, [b; c]) else
        lwt b' = get_elem b in
        let i'' = Monoid.combine i' (measure b') in
        if f i'' = 0 then return ([a], b, [c]) else
          lwt c' = get_elem c in
          let i''' = Monoid.combine i'' (measure c') in
          if f i''' = 0 then return ([a; b], c, []) else
            raise_lwt Not_found
    | `Four (_, a, b, c, d) ->
      lwt a' = get_elem a in
      let i' = Monoid.combine i (measure a') in
      if f i' = 0 then return ([], a, [b; c; d]) else
        lwt b' = get_elem b in
        let i'' = Monoid.combine i' (measure b') in
        if f i'' = 0 then return ([a], b, [c; d]) else
          lwt c' = get_elem c in
          let i''' = Monoid.combine i'' (measure c') in
          if f i''' = 0 then return ([a; b], c, [d]) else
            lwt d' = get_elem d in
            let i'''' = Monoid.combine i''' (measure d') in
            if f i'''' = 0 then return ([a; b; c], d, []) else
              raise_lwt Not_found

  let find f = function
    | `Nil -> raise_lwt Empty
    | `Single x ->
      lwt x' = get_elem x in
      if f (measure x') = 0 then return (`Nil, x, `Nil) else
        raise_lwt Not_found
    | `Deep (_, pr, m, sf) ->
      let vpr = measure_digit pr in
      lwt m' = get_fg reader m in
      if f vpr <= 0 then
        lwt (l, x, r) = find_digit f Monoid.zero pr in
        lwt l' = to_tree_list l and r' = deep_left r m' sf in
        return (l', x, r')
      else
        let vm = Monoid.combine vpr (measure_t_node m') in
        if f vm <= 0 then
          lwt (ml, xs, mr) = split_tree_aux reader writer f vpr m' in
          lwt (l, x, r) = find_digit f (Monoid.combine vpr (measure_t_node ml)) (to_digit_node xs) in
          lwt l' = deep_right pr ml l and r' = deep_left r mr sf in
          return (l', x, r')
        else
          lwt (l, x, r) = find_digit f vm sf in
          lwt l' = deep_right pr m' l and r' = to_tree_list r in
          return (l', x, r')

  let delete f = function
    | `Nil -> return `Nil
    | t ->
      lwt m_t = measure_t t in
      if f m_t <= 0 then
        lwt (l, _, r) = find f t in
        append l r
      else
        raise_lwt Not_found

  (*---------------------------------*)
  (*            lookup               *)
  (*---------------------------------*)
  let lookup_digit_node : 'a. (Monoid.t -> int) -> Monoid.t -> 'a node digit -> Monoid.t * 'a node = fun f i -> function
    | `One (_, a) -> Monoid.zero, a
    | `Two (_, a, b) ->
      let m_a = measure_node a in
      let i' = Monoid.combine i m_a in
      if f i' <= 0 then Monoid.zero, a else m_a, b
    | `Three (_, a, b, c) ->
      let m_a = measure_node a in
      let i' = Monoid.combine i m_a in
      if f i' <= 0 then Monoid.zero, a else
        let m_b = measure_node b in
        let i'' = Monoid.combine i' m_b in
        if f i'' <= 0 then m_a, b else Monoid.combine m_a m_b, c
    | `Four (_, a, b, c, d) ->
      let m_a = measure_node a in
      let i' = Monoid.combine i m_a in
      if f i' <= 0 then Monoid.zero, a else
        let m_b = measure_node b in
        let i'' = Monoid.combine i' m_b in
        if f i'' <= 0 then m_a, b else
          let m_c = measure_node c in
          let i''' = Monoid.combine i'' m_c in
          if f i''' <= 0 then Monoid.combine m_a m_b, c else Monoid.combine (Monoid.combine m_a m_b) m_c, d

  let lookup_digit f i = function
    | `One (_, a) ->
      lwt a' = get_elem a in
      let i' = Monoid.combine i (measure a') in
      if f i' = 0 then return a' else
        raise_lwt Not_found
    | `Two (_, a, b) ->
      lwt a' = get_elem a in
      let i' = Monoid.combine i (measure a') in
      if f i' = 0 then return a' else
        lwt b' = get_elem b in
        let i'' = Monoid.combine i' (measure b') in
        if f i'' = 0 then return b' else
          raise_lwt Not_found
    | `Three (_, a, b, c) ->
      lwt a' = get_elem a in
      let i' = Monoid.combine i (measure a') in
      if f i' = 0 then return a' else
        lwt b' = get_elem b in
        let i'' = Monoid.combine i' (measure b') in
        if f i'' = 0 then return b' else
          lwt c' = get_elem c in
          let i''' = Monoid.combine i'' (measure c') in
          if f i''' = 0 then return c' else
            raise_lwt Not_found
    | `Four (_, a, b, c, d) ->
      lwt a' = get_elem a in
      let i' = Monoid.combine i (measure a') in
      if f i' = 0 then return a' else
        lwt b' = get_elem b in
        let i'' = Monoid.combine i' (measure b') in
        if f i'' = 0 then return b' else
          lwt c' = get_elem c in
          let i''' = Monoid.combine i'' (measure c') in
          if f i''' = 0 then return c' else
            lwt d' = get_elem d in
            let i'''' = Monoid.combine i''' (measure d') in
            if f i'''' = 0 then return d' else
              raise_lwt Not_found

  let lookup_node_node f i = function
    | `Node2 (_, a, b) ->
      let m_a = measure_node a in
      let i' = Monoid.combine i m_a in
      if f i' <= 0 then Monoid.zero, a else m_a, b
    | `Node3 (_, a, b, c) ->
      let m_a = measure_node a in
      let i' = Monoid.combine i m_a in
      if f i' <= 0 then Monoid.zero, a else
        let m_b = measure_node b in
        let i'' = Monoid.combine i' m_b in
        if f i'' <= 0 then m_a, b else Monoid.combine m_a m_b, c

  let lookup_node f i = function
    | `Node2 (_, a, b) ->
      lwt a' = get_elem a in
      let i' = Monoid.combine i (measure a') in
      if f i' = 0 then return a' else
        lwt b' = get_elem b in
        let i'' = Monoid.combine i' (measure b') in
        if f i'' = 0 then return b' else
          raise_lwt Not_found
    | `Node3 (_, a, b, c) ->
      lwt a' = get_elem a in
      let i' = Monoid.combine i (measure a') in
      if f i' = 0 then return a' else
        lwt b' = get_elem b in
        let i'' = Monoid.combine i' (measure b') in
        if f i'' = 0 then return b' else
          lwt c' = get_elem c in
          let i''' = Monoid.combine i'' (measure c') in
          if f i''' = 0 then return c' else
            raise_lwt Not_found

  let rec lookup_aux : 'a. 'a node Json.reader -> (Monoid.t -> int) -> Monoid.t -> 'a node fg -> (Monoid.t * 'a node) Lwt.t = fun reader f i -> function
    | `Nil -> raise_lwt Empty
    | `Single x -> return (Monoid.zero, x)
    | `Deep (_, pr, m, sf) ->
      let m_pr = measure_digit pr in
      let i' = Monoid.combine i m_pr in
      if f i' <= 0 then return (lookup_digit_node f i pr) else
        let reader' = read_node reader in
        lwt m' = get_fg reader' m in
        let m_m = measure_t_node m' in
        let i'' = Monoid.combine i' m_m in
        if f i'' <= 0 then
          lwt v_left, node = lookup_aux reader' f i' m' in
          let v, x = lookup_node_node f (Monoid.combine i' v_left) node in
          return (Monoid.combine (Monoid.combine m_pr v_left) v, x)
        else
          let v, x = lookup_digit_node f i'' sf in
          return (Monoid.combine (Monoid.combine m_pr m_m) v, x)

  let lookup f = function
    | `Nil -> raise_lwt Empty
    | `Single x ->
      lwt x' = get_elem x in
      if f (measure x') = 0 then return x' else
        raise_lwt Not_found
    | `Deep (_, pr, m, sf) ->
      let i' = measure_digit pr in
      if f i' <= 0 then lookup_digit f Monoid.zero pr else
        lwt m' = get_fg reader m in
        let i'' = Monoid.combine i' (measure_t_node m') in
        if f i'' <= 0 then
          lwt v_left, node = lookup_aux reader f i' m' in
          lookup_node f (Monoid.combine i' v_left) node
        else
          lookup_digit f i'' sf
end
