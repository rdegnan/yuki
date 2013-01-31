(* Auto-generated from "yuki_tree.atd" *)


type 'a node = 'a Yuki_tree_t.node

type 'a digit = 'a Yuki_tree_t.digit

type 'a fg = 'a Yuki_tree_t.fg

val write_node :
  (Bi_outbuf.t -> 'a -> unit) ->
  Bi_outbuf.t -> 'a node -> unit
  (** Output a JSON value of type {!node}. *)

val string_of_node :
  (Bi_outbuf.t -> 'a -> unit) ->
  ?len:int -> 'a node -> string
  (** Serialize a value of type {!node}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_node :
  (Yojson.Safe.lexer_state -> Lexing.lexbuf -> 'a) ->
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> 'a node
  (** Input JSON data of type {!node}. *)

val node_of_string :
  (Yojson.Safe.lexer_state -> Lexing.lexbuf -> 'a) ->
  string -> 'a node
  (** Deserialize JSON data of type {!node}. *)

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

val write_fg :
  (Bi_outbuf.t -> 'a -> unit) ->
  Bi_outbuf.t -> 'a fg -> unit
  (** Output a JSON value of type {!fg}. *)

val string_of_fg :
  (Bi_outbuf.t -> 'a -> unit) ->
  ?len:int -> 'a fg -> string
  (** Serialize a value of type {!fg}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_fg :
  (Yojson.Safe.lexer_state -> Lexing.lexbuf -> 'a) ->
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> 'a fg
  (** Input JSON data of type {!fg}. *)

val fg_of_string :
  (Yojson.Safe.lexer_state -> Lexing.lexbuf -> 'a) ->
  string -> 'a fg
  (** Deserialize JSON data of type {!fg}. *)

