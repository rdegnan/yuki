(* Auto-generated from "yuki.atd" *)


type array = Yuki_t.array

type node = Yuki_t.node

type heap = Yuki_t.heap

type bootstrap = Yuki_t.bootstrap

val write_array :
  Bi_outbuf.t -> array -> unit
  (** Output a JSON value of type {!array}. *)

val string_of_array :
  ?len:int -> array -> string
  (** Serialize a value of type {!array}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_array :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> array
  (** Input JSON data of type {!array}. *)

val array_of_string :
  string -> array
  (** Deserialize JSON data of type {!array}. *)

val write_node :
  Bi_outbuf.t -> node -> unit
  (** Output a JSON value of type {!node}. *)

val string_of_node :
  ?len:int -> node -> string
  (** Serialize a value of type {!node}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_node :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> node
  (** Input JSON data of type {!node}. *)

val node_of_string :
  string -> node
  (** Deserialize JSON data of type {!node}. *)

val write_heap :
  Bi_outbuf.t -> heap -> unit
  (** Output a JSON value of type {!heap}. *)

val string_of_heap :
  ?len:int -> heap -> string
  (** Serialize a value of type {!heap}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_heap :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> heap
  (** Input JSON data of type {!heap}. *)

val heap_of_string :
  string -> heap
  (** Deserialize JSON data of type {!heap}. *)

val write_bootstrap :
  Bi_outbuf.t -> bootstrap -> unit
  (** Output a JSON value of type {!bootstrap}. *)

val string_of_bootstrap :
  ?len:int -> bootstrap -> string
  (** Serialize a value of type {!bootstrap}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_bootstrap :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> bootstrap
  (** Input JSON data of type {!bootstrap}. *)

val bootstrap_of_string :
  string -> bootstrap
  (** Deserialize JSON data of type {!bootstrap}. *)

