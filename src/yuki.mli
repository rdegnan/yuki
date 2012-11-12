module Array(Conn:Make.Conn)(Elem:Make.Elem) : sig
  val empty : unit -> string Lwt.t

  val cons : string -> ?key:string -> Elem.t -> string Lwt.t
  val head : string -> Elem.t Lwt.t
  val tail : string -> string Lwt.t
  (* head and tail raise Empty if list is empty *)

  val lookup : string -> int -> Elem.t Lwt.t
  val update : string -> int -> Elem.t -> string Lwt.t
  (* lookup and update raise Subscript if index is out of bounds *)

  val page : string -> int -> int -> Elem.t list Lwt.t
end

module Heap(Conn:Make.Conn)(Elem:Make.Ord) : sig
  val empty : unit -> string Lwt.t

  val insert : string -> Elem.t -> string Lwt.t

  val find_min : string -> Elem.t Lwt.t
  val delete_min : string -> (Elem.t * string) Lwt.t
  (* find_min and delete_min raise Empty if heap is empty *)
end
