(* Auto-generated from "yuki_types.atd" *)


type rlist = (int * string) list


let _1_tag = Bi_io.array_tag
let write_untagged__1 = (
  Ag_ob_run.write_untagged_list
    Bi_io.tuple_tag
    (
      fun ob x ->
        Bi_vint.write_uvint ob 2;
        (
          let x, _ = x in (
            Bi_io.write_svint
          ) ob x
        );
        (
          let _, x = x in (
            Bi_io.write_string
          ) ob x
        );
    )
)
let write__1 ob x =
  Bi_io.write_tag ob Bi_io.array_tag;
  write_untagged__1 ob x
let string_of__1 ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write__1 ob x;
  Bi_outbuf.contents ob
let get__1_reader = (
  Ag_ob_run.get_list_reader (
    fun tag ->
      if tag <> 20 then Ag_ob_run.read_error () else
        fun ib ->
          let len = Bi_vint.read_uvint ib in
          if len < 2 then Ag_ob_run.missing_tuple_fields len [ 0; 1 ];
          let x0 =
            (
              Ag_ob_run.read_int
            ) ib
          in
          let x1 =
            (
              Ag_ob_run.read_string
            ) ib
          in
          for i = 2 to len - 1 do Bi_io.skip ib done;
          (x0, x1)
  )
)
let read__1 = (
  Ag_ob_run.read_list (
    fun tag ->
      if tag <> 20 then Ag_ob_run.read_error () else
        fun ib ->
          let len = Bi_vint.read_uvint ib in
          if len < 2 then Ag_ob_run.missing_tuple_fields len [ 0; 1 ];
          let x0 =
            (
              Ag_ob_run.read_int
            ) ib
          in
          let x1 =
            (
              Ag_ob_run.read_string
            ) ib
          in
          for i = 2 to len - 1 do Bi_io.skip ib done;
          (x0, x1)
  )
)
let _1_of_string ?pos s =
  read__1 (Bi_inbuf.from_string ?pos s)
let rlist_tag = Bi_io.array_tag
let write_untagged_rlist = (
  write_untagged__1
)
let write_rlist ob x =
  Bi_io.write_tag ob Bi_io.array_tag;
  write_untagged_rlist ob x
let string_of_rlist ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_rlist ob x;
  Bi_outbuf.contents ob
let get_rlist_reader = (
  get__1_reader
)
let read_rlist = (
  read__1
)
let rlist_of_string ?pos s =
  read_rlist (Bi_inbuf.from_string ?pos s)
