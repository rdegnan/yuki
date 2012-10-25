(* Auto-generated from "yuki_types.atd" *)


type rlist = (int * string) list

type node = (int * string * string list)

type heap = string list


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
let _2_tag = Bi_io.array_tag
let write_untagged__2 = (
  Ag_ob_run.write_untagged_list
    Bi_io.string_tag
    (
      Bi_io.write_untagged_string
    )
)
let write__2 ob x =
  Bi_io.write_tag ob Bi_io.array_tag;
  write_untagged__2 ob x
let string_of__2 ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write__2 ob x;
  Bi_outbuf.contents ob
let get__2_reader = (
  Ag_ob_run.get_list_reader (
    Ag_ob_run.get_string_reader
  )
)
let read__2 = (
  Ag_ob_run.read_list (
    Ag_ob_run.get_string_reader
  )
)
let _2_of_string ?pos s =
  read__2 (Bi_inbuf.from_string ?pos s)
let node_tag = Bi_io.tuple_tag
let write_untagged_node = (
  fun ob x ->
    Bi_vint.write_uvint ob 3;
    (
      let x, _, _ = x in (
        Bi_io.write_svint
      ) ob x
    );
    (
      let _, x, _ = x in (
        Bi_io.write_string
      ) ob x
    );
    (
      let _, _, x = x in (
        write__2
      ) ob x
    );
)
let write_node ob x =
  Bi_io.write_tag ob Bi_io.tuple_tag;
  write_untagged_node ob x
let string_of_node ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_node ob x;
  Bi_outbuf.contents ob
let get_node_reader = (
  fun tag ->
    if tag <> 20 then Ag_ob_run.read_error () else
      fun ib ->
        let len = Bi_vint.read_uvint ib in
        if len < 3 then Ag_ob_run.missing_tuple_fields len [ 0; 1; 2 ];
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
        let x2 =
          (
            read__2
          ) ib
        in
        for i = 3 to len - 1 do Bi_io.skip ib done;
        (x0, x1, x2)
)
let read_node = (
  fun ib ->
    if Bi_io.read_tag ib <> 20 then Ag_ob_run.read_error_at ib;
    let len = Bi_vint.read_uvint ib in
    if len < 3 then Ag_ob_run.missing_tuple_fields len [ 0; 1; 2 ];
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
    let x2 =
      (
        read__2
      ) ib
    in
    for i = 3 to len - 1 do Bi_io.skip ib done;
    (x0, x1, x2)
)
let node_of_string ?pos s =
  read_node (Bi_inbuf.from_string ?pos s)
let heap_tag = Bi_io.array_tag
let write_untagged_heap = (
  write_untagged__2
)
let write_heap ob x =
  Bi_io.write_tag ob Bi_io.array_tag;
  write_untagged_heap ob x
let string_of_heap ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_heap ob x;
  Bi_outbuf.contents ob
let get_heap_reader = (
  get__2_reader
)
let read_heap = (
  read__2
)
let heap_of_string ?pos s =
  read_heap (Bi_inbuf.from_string ?pos s)
