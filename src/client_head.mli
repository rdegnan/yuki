open Riak

module type S = sig
  type t

  val get_head : riak_connection -> riak_key -> (t * riak_vclock option) Lwt.t
  val put_head : riak_connection -> riak_key -> t -> riak_vclock option -> unit Lwt.t
  val with_head : riak_key -> (riak_connection -> t -> 'a Lwt.t) -> 'a Lwt.t
  val modify_head : riak_key -> (riak_connection -> t -> t Lwt.t) -> unit Lwt.t
end

module Make(Conn:Make.Conn)(Head:Make.Elem) : sig
  type t = Head.t

  val get_head : riak_connection -> riak_key -> (Head.t * riak_vclock option) Lwt.t
  val put_head : riak_connection -> riak_key -> Head.t -> riak_vclock option -> unit Lwt.t
  val with_head : riak_key -> (riak_connection -> Head.t -> 'a Lwt.t) -> 'a Lwt.t
  val modify_head : riak_key -> (riak_connection -> Head.t -> Head.t Lwt.t) -> unit Lwt.t
end
