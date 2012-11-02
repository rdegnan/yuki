open Lwt
open Yuki_j

module Array(Conn:Make.Conn)(Elem:Make.Elem) = struct
  module Impl = Yuki_array.Make(Conn)(Elem)
  module Client = Client.Make(Conn)(struct
    type t = array
    let of_string x = array_of_string x
    let to_string x = string_of_array x
    let bucket = Elem.bucket
  end)

  let cons key x =
    try_lwt Client.write key (Impl.cons x)
    with Not_found ->
      lwt t = Impl.cons x [] in
      Client.put ~key t [] >> return ()

  let head key = Client.read key Impl.head
  let tail key = Client.write key Impl.tail

  let lookup key i = Client.read key (Impl.lookup i)
  let update key i y = Client.write key (Impl.update i y)

  let page key i n = Client.read key (Impl.page i n)
end

module Heap(Conn:Make.Conn)(Elem:Make.Ord) = struct
  module Impl = Yuki_heap.Make(Conn)(Elem)
  module Client = Client.Make(Conn)(struct
    type t = heap
    let of_string x = heap_of_string x
    let to_string x = string_of_heap x
    let bucket = Elem.bucket
  end)

  let insert key x =
    try_lwt Client.write key (Impl.insert x)
    with Not_found ->
      lwt t = Impl.insert x [] in
      Client.put ~key t [] >> return ()

  let find_min key = Client.read key Impl.find_min
  let delete_min key = Client.write' key Impl.delete_min (fun (x, ts) put ->
    put ts [] >> return x
  )
end

module BootstrappedHeap(Conn:Make.Conn)(Elem:Make.Ord) = struct
  module Impl = Yuki_bootstrap.Make(Conn)(Elem)
  module Client = Client.Make(Conn)(Impl.BootstrappedElem)

  let insert key x =
    try_lwt Client.write key (Impl.insert x)
    with Not_found ->
      Client.put ~key (x, []) [] >> return ()

  let find_min key = Client.read key Impl.find_min
  let delete_min key = Client.write' key Impl.delete_min (fun (x, ts) put ->
    match ts with
      | Some ts -> put ts [] >> return x
      | None -> Client.delete key >> return x
  )
end
