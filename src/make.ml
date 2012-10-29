open Riak

module type Conn = sig
  val with_connection : (riak_connection -> 'a Lwt.t) -> 'a Lwt.t
end

module type Stringable = sig
  type t
  val of_string : string -> t
  val to_string : t -> string
end

module type Elem = sig
  include Stringable
  val bucket : string
end

module type Ord = sig
  include Elem
  val compare : t -> t -> int
end
