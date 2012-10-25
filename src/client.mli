open Riak

module type S = sig
  type t

  val with_connection : (riak_connection -> 'a Lwt.t) -> 'a Lwt.t

  val get : riak_connection -> riak_key -> (t * string list) Lwt.t
  val put : riak_connection -> t -> riak_key list -> riak_key Lwt.t
  val with_elem : riak_key -> (riak_connection -> t * riak_key list -> 'a Lwt.t) -> 'a Lwt.t
end

module Make(Conn:Make.Conn)(Elem:Make.Elem) : sig
  type t = Elem.t

  val with_connection : (riak_connection -> 'a Lwt.t) -> 'a Lwt.t

  val get : riak_connection -> riak_key -> (Elem.t * string list) Lwt.t
  val put : riak_connection -> Elem.t -> riak_key list -> riak_key Lwt.t
  val with_elem : riak_key -> (riak_connection -> Elem.t * riak_key list -> 'a Lwt.t) -> 'a Lwt.t
end
