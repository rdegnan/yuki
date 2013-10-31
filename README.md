Yuki
====

[For much more information, check out the RICON presentation!](https://speakerdeck.com/rdegnan/yuki-functional-data-structures-for-riak)

Riak is extremely fast as a key-value store, but querying on secondary indexes or running MapReduce jobs can result in unpredictable latency. In practice, developers often require richer means of querying data in real-time. Yuki is an OCaml library that implements various functional data structures in Riak, giving users the ability to interact with their data as if it were a queue, a heap, a random access list or a custom data structure. Yuki has been used in practice to achieve extremely low-latency random access, flexible paging, and conditional streaming of Riak data.

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
