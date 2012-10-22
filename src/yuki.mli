module Make(Conn:Make.Conn)(Obj:Make.Obj) : sig
  module Array : sig
    val cons : string -> Obj.t -> unit Lwt.t
    val head : string -> Obj.t Lwt.t
    val tail : string -> unit Lwt.t
    (* head and tail raise Empty if list is empty *)

    val lookup : string -> int -> Obj.t Lwt.t
    val update : string -> int -> Obj.t -> unit Lwt.t
    (* lookup and update raise Subscript if index is out of bounds *)

    val page : string -> int -> int -> Obj.t list Lwt.t
  end
end
