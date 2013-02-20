open Riak

module Make(Conn:Yuki_make.Conn)(Elem:Yuki_make.Elem) : sig
  type t = {
    key : riak_key;
    value : Elem.t;
    links : riak_key list;
  }

  val get : riak_key -> t Lwt.t
  val put : ?key:riak_key -> ?ops:riak_put_option list -> Elem.t -> riak_key list -> riak_key Lwt.t

  val read : riak_key -> (Elem.t -> 'a Lwt.t) -> 'a Lwt.t
  val read_default : riak_key -> Elem.t -> (Elem.t -> 'a Lwt.t) -> 'a Lwt.t

  val write : riak_key -> (Elem.t -> Elem.t Lwt.t) -> riak_key Lwt.t
  val write_default : riak_key -> Elem.t -> (Elem.t -> Elem.t Lwt.t) -> unit Lwt.t
  val write' : riak_key -> (Elem.t -> ('a * Elem.t) Lwt.t) -> ('a * riak_key) Lwt.t
  val write_default' : riak_key -> Elem.t -> (Elem.t -> ('a * Elem.t) Lwt.t) -> 'a Lwt.t
end
