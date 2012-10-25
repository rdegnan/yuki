(* Auto-generated from "yuki_types.atd" *)


type rlist = (int * string) list

type node = (int * string * string list)

type heap = string list

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


(* Writers for type node *)

val node_tag : Bi_io.node_tag
  (** Tag used by the writers for type {!node}.
      Readers may support more than just this tag. *)

val write_untagged_node :
  Bi_outbuf.t -> node -> unit
  (** Output an untagged biniou value of type {!node}. *)

val write_node :
  Bi_outbuf.t -> node -> unit
  (** Output a biniou value of type {!node}. *)

val string_of_node :
  ?len:int -> node -> string
  (** Serialize a value of type {!node} into
      a biniou string. *)

(* Readers for type node *)

val get_node_reader :
  Bi_io.node_tag -> (Bi_inbuf.t -> node)
  (** Return a function that reads an untagged
      biniou value of type {!node}. *)

val read_node :
  Bi_inbuf.t -> node
  (** Input a tagged biniou value of type {!node}. *)

val node_of_string :
  ?pos:int -> string -> node
  (** Deserialize a biniou value of type {!node}.
      @param pos specifies the position where
                 reading starts. Default: 0. *)


(* Writers for type heap *)

val heap_tag : Bi_io.node_tag
  (** Tag used by the writers for type {!heap}.
      Readers may support more than just this tag. *)

val write_untagged_heap :
  Bi_outbuf.t -> heap -> unit
  (** Output an untagged biniou value of type {!heap}. *)

val write_heap :
  Bi_outbuf.t -> heap -> unit
  (** Output a biniou value of type {!heap}. *)

val string_of_heap :
  ?len:int -> heap -> string
  (** Serialize a value of type {!heap} into
      a biniou string. *)

(* Readers for type heap *)

val get_heap_reader :
  Bi_io.node_tag -> (Bi_inbuf.t -> heap)
  (** Return a function that reads an untagged
      biniou value of type {!heap}. *)

val read_heap :
  Bi_inbuf.t -> heap
  (** Input a tagged biniou value of type {!heap}. *)

val heap_of_string :
  ?pos:int -> string -> heap
  (** Deserialize a biniou value of type {!heap}.
      @param pos specifies the position where
                 reading starts. Default: 0. *)


