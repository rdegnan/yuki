open Riak

module type Obj = sig
  type t

  val of_string : string -> t
  val to_string : t -> string
end

module type Conn = sig
  val bucket : string
  val pool : riak_connection Lwt_pool.t
end

module type Client = sig
  type t
  val get : riak_key -> (t * string list) Lwt.t
  val put : t -> string list -> string Lwt.t
end
