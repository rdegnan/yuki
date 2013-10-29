yuki
====

Functional data structures for Riak

Installation
------------

```
brew install opam protobuf riak
opam install lwt atdgen riak

make
make install
```

Usage (from toplevel)
---------------------

```
#require "yuki";;

let pool = Lwt_pool.create 100 (fun () -> Riak.riak_connect_with_defaults "localhost" 8087);;

module Conn = struct
  let with_connection fn = Lwt_pool.use pool fn
end;;

module Elem = struct
  type t = int
  let of_string = int_of_string
  let to_string = string_of_int
  let bucket = "test"
end;;

module RandomAccessSequence = Yuki.RandomAccessSequence(Conn)(Elem);;
let head = Lwt_main.run (RandomAccessSequence.init ());;
```
