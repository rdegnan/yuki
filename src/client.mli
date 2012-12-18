open Riak

module Make(Conn:Make.Conn)(Elem:Make.Elem) : sig
  type t = {
    key : riak_key;
    value : Elem.t;
    vclock : riak_vclock;
    links : riak_key list;
  }

  val get : riak_key -> t Lwt.t
  val put : ?key:riak_key -> ?v:riak_vclock -> Elem.t -> riak_key list -> t Lwt.t

  val read : riak_key -> (Elem.t -> 'a Lwt.t) -> 'a Lwt.t
  val read_default : riak_key -> Elem.t -> (Elem.t -> 'a Lwt.t) -> 'a Lwt.t

  val write : riak_key -> (Elem.t -> Elem.t Lwt.t) -> riak_key Lwt.t
  val write_default : riak_key -> Elem.t -> (Elem.t -> Elem.t Lwt.t) -> unit Lwt.t
  val write' : riak_key -> (Elem.t -> ('a * Elem.t) Lwt.t) -> ('a * riak_key) Lwt.t
  val write_default' : riak_key -> Elem.t -> (Elem.t -> ('a * Elem.t) Lwt.t) -> 'a Lwt.t
end
