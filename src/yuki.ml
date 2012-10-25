open Yuki_types

module Make(Conn:Make.Conn) = struct
  module RandomAccessList(Elem:Make.Elem) = struct
    module Client = Client_head.Make(Conn)(struct
      type t = rlist
      let of_string x = rlist_of_string x
      let to_string x = string_of_rlist x
      let bucket = Elem.bucket
    end)

    module SkewBinary = struct
      module Impl = Yuki_rlist.Make(Conn)(Elem)

      let cons key x = Client.modify_head key (fun conn ts -> Impl.cons conn x ts)
      let head key = Client.with_head key (fun conn ts -> Impl.head conn ts)
      let tail key = Client.modify_head key (fun conn ts -> Impl.tail conn ts)

      let lookup key i = Client.with_head key (fun conn ts -> Impl.lookup conn i ts)
      let update key i y = Client.modify_head key (fun conn ts -> Impl.update conn i y ts)

      let page key i n = Client.with_head key (fun conn ts -> Impl.page conn i n ts)
    end

    include SkewBinary
  end

  (*module Heap(Elem:Make.Ord) = struct
    module Client = Client.Make(struct
      type t = node
      let of_string x = node_of_string x
      let to_string x = string_of_node x
      let bucket = Elem.bucket
      let pool = Elem.pool
    end)(struct
      type t = heap
      let of_string x = heap_of_string x
      let to_string x = string_of_heap x
    end)

    module SkewBinomial = struct
      module Impl = Yuki_heap.Make(Client)

      let insert key x = Client.modify_head key (fun conn ts -> Impl.insert conn x ts)

      let find_min key = Client.with_head key (fun conn ts -> Impl.find_min conn ts)
      let delete_min key = Client.modify_head key (fun conn ts -> Impl.delete_min conn ts)
    end

    include SkewBinomial
  end*)
end
