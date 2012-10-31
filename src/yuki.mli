module Make(Conn:Make.Conn) : sig
  module Array(Elem:Make.Elem) : sig
    val cons : string -> Elem.t -> unit Lwt.t
    val head : string -> Elem.t Lwt.t
    val tail : string -> unit Lwt.t
    (* head and tail raise Empty if list is empty *)

    val lookup : string -> int -> Elem.t Lwt.t
    val update : string -> int -> Elem.t -> unit Lwt.t
    (* lookup and update raise Subscript if index is out of bounds *)

    val page : string -> int -> int -> Elem.t list Lwt.t
  end

  module Heap(Elem:Make.Ord) : sig
    val insert : string -> Elem.t -> unit Lwt.t

    val find_min : string -> Elem.t Lwt.t
    val delete_min : string -> Elem.t Lwt.t
    (* find_min and delete_min raise Empty if heap is empty *)
  end

  module BootstrappedHeap(Elem:Make.Ord) : sig
    val insert : string -> Elem.t -> unit Lwt.t

    val find_min : string -> Elem.t Lwt.t
    val delete_min : string -> Elem.t Lwt.t
    (* find_min and delete_min raise Empty if heap is empty *)
  end
end
