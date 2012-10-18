open Lwt
open Riak

exception Empty
exception Subscript

module type STRINGABLE = sig
  type t

  val of_string : string -> t
  val to_string : t -> string
end

module type STACK =
  functor (S : STRINGABLE) -> sig
    type stack

    val empty : stack
    val is_empty : stack -> bool
    val cons : S.t -> stack -> stack Lwt.t
    val head : stack -> S.t Lwt.t    (* raises Empty if stack is empty *)
    val tail : stack -> stack Lwt.t  (* raises Empty if stack is empty *)
  end

let ip = "127.0.0.1"
let port = 8087
let bucket = "test"

module RiakStack : STACK =
  functor (S : STRINGABLE) -> struct
    type stack = Nil | Cons of string * stack

    let empty = Nil
    let is_empty s = s = Nil

    let links = function
      | Nil -> []
      | Cons (key, _) -> [{bucket = Some bucket; key = Some key; tag = None}]

    let cons x s = riak_exec ip port (fun conn ->
      match_lwt riak_put_raw conn bucket None ~links:(links s) (S.to_string x) [Put_return_head true] None with
        | Some { obj_key = Some key } -> return (Cons (key, s))
        | _ -> raise Not_found
    )

    let head = function
      | Nil -> raise Empty
      | Cons (key, _) -> riak_exec ip port (fun conn ->
          match_lwt riak_get conn bucket key [] with
            | Some { obj_value = Some x } -> return (S.of_string x)
            | _ -> raise Not_found
        )

    let tail = function
      | Nil -> raise Empty
      | Cons (_, s) -> return s
  end
