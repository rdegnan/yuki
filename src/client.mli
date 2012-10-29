open Riak

module Make(Conn:Make.Conn)(Elem:Make.Elem) : sig
  type t = {
    key : riak_key;
    value : Elem.t;
    vclock : riak_vclock;
    links : riak_key list;
  }

  val with_connection : (riak_connection -> 'a Lwt.t) -> 'a Lwt.t

  val get : riak_connection -> riak_key -> t Lwt.t
  val put : riak_connection -> Elem.t -> riak_key list -> t Lwt.t
  val with_elem : riak_key -> (riak_connection -> t -> 'a Lwt.t) -> 'a Lwt.t
end
