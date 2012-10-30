(* Auto-generated from "yuki.atd" *)


type array = Yuki_t.array

type node = Yuki_t.node

type heap = Yuki_t.heap

let write__1 = (
  Ag_oj_run.write_list (
    fun ob x ->
      Bi_outbuf.add_char ob '(';
      (let x, _ = x in
      (
        Yojson.Safe.write_int
      ) ob x
      );
      Bi_outbuf.add_char ob ',';
      (let _, x = x in
      (
        Yojson.Safe.write_string
      ) ob x
      );
      Bi_outbuf.add_char ob ')';
  )
)
let string_of__1 ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write__1 ob x;
  Bi_outbuf.contents ob
let read__1 = (
  Ag_oj_run.read_list (
    fun p lb ->
      Yojson.Safe.read_space p lb;
      let std_tuple = Yojson.Safe.start_any_tuple p lb in
      let len = ref 0 in
      let end_of_tuple = ref false in
      (try
        let x0 =
          let x =
            (
              Ag_oj_run.read_int
            ) p lb
          in
          incr len;
          Yojson.Safe.read_space p lb;
          Yojson.Safe.read_tuple_sep2 p std_tuple lb;
          x
        in
        let x1 =
          let x =
            (
              Ag_oj_run.read_string
            ) p lb
          in
          incr len;
          (try
            Yojson.Safe.read_space p lb;
            Yojson.Safe.read_tuple_sep2 p std_tuple lb;
          with Yojson.End_of_tuple -> end_of_tuple := true);
          x
        in
        if not !end_of_tuple then (
          try
            while true do
              Yojson.Safe.skip_json p lb;
              Yojson.Safe.read_tuple_sep2 p std_tuple lb;
            done
          with Yojson.End_of_tuple -> ()
        );
        (x0, x1)
      with Yojson.End_of_tuple ->
        Ag_oj_run.missing_tuple_fields !len [ 0; 1 ]);
  )
)
let _1_of_string s =
  read__1 (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_array = (
  write__1
)
let string_of_array ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_array ob x;
  Bi_outbuf.contents ob
let read_array = (
  read__1
)
let array_of_string s =
  read_array (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write__2 = (
  Ag_oj_run.write_list (
    Yojson.Safe.write_string
  )
)
let string_of__2 ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write__2 ob x;
  Bi_outbuf.contents ob
let read__2 = (
  Ag_oj_run.read_list (
    Ag_oj_run.read_string
  )
)
let _2_of_string s =
  read__2 (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_node = (
  fun ob x ->
    Bi_outbuf.add_char ob '(';
    (let x, _, _ = x in
    (
      Yojson.Safe.write_int
    ) ob x
    );
    Bi_outbuf.add_char ob ',';
    (let _, x, _ = x in
    (
      Yojson.Safe.write_string
    ) ob x
    );
    Bi_outbuf.add_char ob ',';
    (let _, _, x = x in
    (
      write__2
    ) ob x
    );
    Bi_outbuf.add_char ob ')';
)
let string_of_node ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_node ob x;
  Bi_outbuf.contents ob
let read_node = (
  fun p lb ->
    Yojson.Safe.read_space p lb;
    let std_tuple = Yojson.Safe.start_any_tuple p lb in
    let len = ref 0 in
    let end_of_tuple = ref false in
    (try
      let x0 =
        let x =
          (
            Ag_oj_run.read_int
          ) p lb
        in
        incr len;
        Yojson.Safe.read_space p lb;
        Yojson.Safe.read_tuple_sep2 p std_tuple lb;
        x
      in
      let x1 =
        let x =
          (
            Ag_oj_run.read_string
          ) p lb
        in
        incr len;
        Yojson.Safe.read_space p lb;
        Yojson.Safe.read_tuple_sep2 p std_tuple lb;
        x
      in
      let x2 =
        let x =
          (
            read__2
          ) p lb
        in
        incr len;
        (try
          Yojson.Safe.read_space p lb;
          Yojson.Safe.read_tuple_sep2 p std_tuple lb;
        with Yojson.End_of_tuple -> end_of_tuple := true);
        x
      in
      if not !end_of_tuple then (
        try
          while true do
            Yojson.Safe.skip_json p lb;
            Yojson.Safe.read_tuple_sep2 p std_tuple lb;
          done
        with Yojson.End_of_tuple -> ()
      );
      (x0, x1, x2)
    with Yojson.End_of_tuple ->
      Ag_oj_run.missing_tuple_fields !len [ 0; 1; 2 ]);
)
let node_of_string s =
  read_node (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_heap = (
  write__2
)
let string_of_heap ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_heap ob x;
  Bi_outbuf.contents ob
let read_heap = (
  read__2
)
let heap_of_string s =
  read_heap (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
