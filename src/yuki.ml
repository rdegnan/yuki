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

  let init () =
    lwt { Client.key } = Client.put Impl.empty [] in
    return key

  let size head = Client.read head (fun x ->
    return (List.fold_left (fun a (w, _) -> a + w) 0 x))

  let cons head ?key x = Client.write head (Impl.cons ?key x)
  let head head = Client.read head Impl.head
  let pop head = Client.write' head Impl.pop

  let lookup head i = Client.read head (Impl.lookup i)
  let update head i y = Client.write head (Impl.update i y)

  let page head i n = Client.read head (Impl.page i n)
  let take_while head p = Client.read head (Impl.take_while p)

  let fold_left head f x = Client.read head (Impl.fold_left f x)
  let fold_right head f x = Client.read head (Impl.fold_right f x)
end

module RandomAccessListNoExn(Conn:Make.Conn)(Elem:Make.Elem) = struct
  module Impl = Yuki_rlist.Make(Conn)(Elem)
  module Client = Client.Make(Conn)(struct
    type t = rlist
    let of_string x = rlist_of_string x
    let to_string x = string_of_rlist x
    let bucket = Elem.bucket
  end)

  let size head = Client.read_default head Impl.empty (fun x ->
    return (List.fold_left (fun a (w, _) -> a + w) 0 x))

  let cons head ?key x = Client.write_default head Impl.empty (Impl.cons ?key x)
  let head head = Client.read_default head Impl.empty Impl.head
  let pop head = Client.write_default' head Impl.empty Impl.pop

  let lookup head i = Client.read_default head Impl.empty (Impl.lookup i)
  let update head i y = Client.write_default head Impl.empty (Impl.update i y)

  let page head i n = Client.read_default head Impl.empty (Impl.page i n)
  let take_while head p = Client.read_default head Impl.empty (Impl.take_while p)

  let fold_left head f x = Client.read_default head Impl.empty (Impl.fold_left f x)
  let fold_right head f x = Client.read_default head Impl.empty (Impl.fold_right f x)
end

module Heap(Conn:Make.Conn)(Elem:Make.Ord) = struct
  module Impl = Yuki_bootstrap.Make(Conn)(Elem)
  module Client = Client.Make(Conn)(Impl.BootstrappedElem)

  let init () =
    lwt { Client.key } = Client.put Impl.empty [] in
    return key

  let insert head x = Client.write head (Impl.insert x)

  let find_min head = Client.read head Impl.find_min
  let delete_min head = Client.write' head Impl.delete_min
end
