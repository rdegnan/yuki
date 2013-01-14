open Lwt
open Riak
open Yuki_j

exception Empty

module type Queue = functor (Conn:Make.Conn) -> functor (Elem:Make.Elem) -> sig
  module ShallowElem : sig
    type digit = Zero | One of Elem.t | Two of Elem.t * Elem.t
    type t = Shallow of digit | Deep of digit * string * digit
    include Make.Elem with type t := t
  end
  module Client : module type of Client.Make(Conn)(ShallowElem)

  val empty : ShallowElem.t

  val snoc : Elem.t -> ShallowElem.t -> string Lwt.t
  val head : ShallowElem.t -> Elem.t Lwt.t
  (*val pop : ShallowElem.t -> (Elem.t * string) Lwt.t*)
end

module rec MakeQ : Queue = functor (Conn:Make.Conn) -> functor (Elem:Make.Elem) -> struct
  (*module DeepElem = struct
    type t = Elem.t * Elem.t
    let of_string x = let (x1, x2) = pair_of_string x in (Elem.of_string x1, Elem.of_string x2)
    let to_string (x1, x2) = string_of_pair (Elem.to_string x1, Elem.to_string x2)
    let bucket = Elem.bucket
  end

  module Deep = MakeQ(Conn)*)

  module ShallowElem = struct
    type digit = Zero | One of Elem.t | Two of Elem.t * Elem.t
    type t = Shallow of digit | Deep of digit * string * digit
    let digit_of_string = function
      | `Zero -> Zero
      | `One x -> One (Elem.of_string x)
      | `Two (x1, x2) -> Two (Elem.of_string x1, Elem.of_string x2)
    let string_of_digit = function
      | Zero -> `Zero
      | One x -> `One (Elem.to_string x)
      | Two (x1, x2) -> `Two (Elem.to_string x1, Elem.to_string x2)
    let of_string x = match queue_of_string x with
      | `Shallow x -> Shallow (digit_of_string x)
      | `Deep (x1, xs, x2) -> Deep (digit_of_string x1, xs, digit_of_string x2)
    let to_string x = string_of_queue (match x with
      | Shallow x -> `Shallow (string_of_digit x)
      | Deep (x1, xs, x2) -> `Deep (string_of_digit x1, xs, string_of_digit x2))
    let bucket = Elem.bucket
  end

  module Client = Client.Make(Conn)(ShallowElem)
  open ShallowElem (* expose Shallow and Deep constructors *)

  let empty = Shallow Zero
  let is_empty = function Shallow Zero -> true | _ -> false

  let shallow x = Client.put (Shallow x) []
  let deep (x1, xs, x2) = Client.put (Deep (x1, xs, x2)) []

  let rec snoc y = function
    | Shallow Zero -> shallow (One y)
    | Shallow (One x) ->
        assert false
        (*lwt key = Deep.Client.put Deep.empty [] in
        deep (Two (x,y), key, Zero)*)
    (*| Deep (f, m, Zero) -> deep (f, m, One y)
    | Deep (f, m, One x) ->
        lwt m' = Deep.Client.get m >>= Deep.snoc (x,y) in
        deep (f, m', Zero)*)
    | _ -> assert false

  let head = function
    | Shallow Zero -> raise Empty
    | Shallow (One x)
    | Deep (One x, _, _)
    | Deep (Two (x, _), _, _) -> return x
    | _ -> assert false
end
