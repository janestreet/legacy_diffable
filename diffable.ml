module Atomic = Atomic
module Diffable_intf = Diffable_intf
module Iso = Iso
module Make_streamable = Diffable_streamable.Make
module Make_streamable_rpc = Diffable_streamable.Make_rpc
module Map = Map
module Option = Option
module Set = Set
module Tuples = Tuples

module type S = Diffable_intf.S
module type S_plain = Diffable_intf.S_plain
