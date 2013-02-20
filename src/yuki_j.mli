(* Auto-generated from "yuki.atd" *)


type rlist = Yuki_t.rlist

type 'a pair = 'a Yuki_t.pair

type 'a digit = 'a Yuki_t.digit

type 'a queue = 'a Yuki_t.queue

type node = Yuki_t.node

type heap = Yuki_t.heap

type bootstrap = Yuki_t.bootstrap

val write_rlist :
  Bi_outbuf.t -> rlist -> unit
  (** Output a JSON value of type {!rlist}. *)

val string_of_rlist :
  ?len:int -> rlist -> string
  (** Serialize a value of type {!rlist}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_rlist :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> rlist
  (** Input JSON data of type {!rlist}. *)

val rlist_of_string :
  string -> rlist
  (** Deserialize JSON data of type {!rlist}. *)

val write_pair :
  (Bi_outbuf.t -> 'a -> unit) ->
  Bi_outbuf.t -> 'a pair -> unit
  (** Output a JSON value of type {!pair}. *)

val string_of_pair :
  (Bi_outbuf.t -> 'a -> unit) ->
  ?len:int -> 'a pair -> string
  (** Serialize a value of type {!pair}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_pair :
  (Yojson.Safe.lexer_state -> Lexing.lexbuf -> 'a) ->
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> 'a pair
  (** Input JSON data of type {!pair}. *)

val pair_of_string :
  (Yojson.Safe.lexer_state -> Lexing.lexbuf -> 'a) ->
  string -> 'a pair
  (** Deserialize JSON data of type {!pair}. *)

val write_digit :
  (Bi_outbuf.t -> 'a -> unit) ->
  Bi_outbuf.t -> 'a digit -> unit
  (** Output a JSON value of type {!digit}. *)

val string_of_digit :
  (Bi_outbuf.t -> 'a -> unit) ->
  ?len:int -> 'a digit -> string
  (** Serialize a value of type {!digit}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_digit :
  (Yojson.Safe.lexer_state -> Lexing.lexbuf -> 'a) ->
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> 'a digit
  (** Input JSON data of type {!digit}. *)

val digit_of_string :
  (Yojson.Safe.lexer_state -> Lexing.lexbuf -> 'a) ->
  string -> 'a digit
  (** Deserialize JSON data of type {!digit}. *)

val write_queue :
  (Bi_outbuf.t -> 'a -> unit) ->
  Bi_outbuf.t -> 'a queue -> unit
  (** Output a JSON value of type {!queue}. *)

val string_of_queue :
  (Bi_outbuf.t -> 'a -> unit) ->
  ?len:int -> 'a queue -> string
  (** Serialize a value of type {!queue}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_queue :
  (Yojson.Safe.lexer_state -> Lexing.lexbuf -> 'a) ->
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> 'a queue
  (** Input JSON data of type {!queue}. *)

val queue_of_string :
  (Yojson.Safe.lexer_state -> Lexing.lexbuf -> 'a) ->
  string -> 'a queue
  (** Deserialize JSON data of type {!queue}. *)

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

