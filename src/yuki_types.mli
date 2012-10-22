(* Auto-generated from "yuki_types.atd" *)


type rlist = (int * string) list

(* Writers for type rlist *)

val rlist_tag : Bi_io.node_tag
  (** Tag used by the writers for type {!rlist}.
      Readers may support more than just this tag. *)

val write_untagged_rlist :
  Bi_outbuf.t -> rlist -> unit
  (** Output an untagged biniou value of type {!rlist}. *)

val write_rlist :
  Bi_outbuf.t -> rlist -> unit
  (** Output a biniou value of type {!rlist}. *)

val string_of_rlist :
  ?len:int -> rlist -> string
  (** Serialize a value of type {!rlist} into
      a biniou string. *)

(* Readers for type rlist *)

val get_rlist_reader :
  Bi_io.node_tag -> (Bi_inbuf.t -> rlist)
  (** Return a function that reads an untagged
      biniou value of type {!rlist}. *)

val read_rlist :
  Bi_inbuf.t -> rlist
  (** Input a tagged biniou value of type {!rlist}. *)

val rlist_of_string :
  ?pos:int -> string -> rlist
  (** Deserialize a biniou value of type {!rlist}.
      @param pos specifies the position where
                 reading starts. Default: 0. *)


