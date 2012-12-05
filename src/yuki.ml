open Lwt
open Yuki_j

module RandomAccessList(Conn:Make.Conn)(Elem:Make.Elem) = struct
  module Impl = Yuki_rlist.Make(Conn)(Elem)
  module Client = Client.Make(Conn)(struct
    type t = rlist
    let of_string x = rlist_of_string x
    let to_string x = string_of_rlist x
    let bucket = Elem.bucket
  end)

  let empty () =
    lwt { Client.key } = Client.put Impl.empty [] in
    return key

  let size head =
    lwt { Client.value } = Client.get head in
    return (List.fold_left (fun a (w, _) -> a + w) 0 value)

  let cons head ?key x = Client.write head (Impl.cons ?key x)
  let head head = Client.read head Impl.head
  let tail head = Client.write' head Impl.tail

  let lookup head i = Client.read head (Impl.lookup i)
  let update head i y = Client.write head (Impl.update i y)

  let page head i n = Client.read head (Impl.page i n)
end

module Heap(Conn:Make.Conn)(Elem:Make.Ord) = struct
  module Impl = Yuki_bootstrap.Make(Conn)(Elem)
  module Client = Client.Make(Conn)(Impl.BootstrappedElem)

  let empty () =
    lwt { Client.key } = Client.put Impl.empty [] in
    return key

  let insert head x = Client.write head (Impl.insert x)

  let find_min head = Client.read head Impl.find_min
  let delete_min head = Client.write' head Impl.delete_min
end
