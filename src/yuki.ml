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

  let empty () =
    lwt { Client.key = key' } = Client.put Impl.empty [] in
    return key'

  let cons key x = Client.write key (Impl.cons x)
  let head key = Client.read key Impl.head
  let tail key = Client.write key Impl.tail

  let lookup key i = Client.read key (Impl.lookup i)
  let update key i y = Client.write key (Impl.update i y)

  let page key i n = Client.read key (Impl.page i n)
end

module Heap(Conn:Make.Conn)(Elem:Make.Ord) = struct
  module Impl = Yuki_bootstrap.Make(Conn)(Elem)
  module Client = Client.Make(Conn)(Impl.BootstrappedElem)

  let empty () =
    lwt { Client.key = key' } = Client.put Impl.empty [] in
    return key'

  let insert key x = Client.write key (Impl.insert x)

  let find_min key = Client.read key Impl.find_min
  let delete_min key = Client.write' key Impl.delete_min
end
