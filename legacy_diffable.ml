module Atomic = Atomic
module Diffable_intf = Legacy_diffable_intf
module Iso = Iso
module Make_streamable = Legacy_diffable_streamable.Make
module Make_streamable_rpc = Legacy_diffable_streamable.Make_rpc
module Map = Map
module Option = Option
module Result = Result
module Set = Set
module Tuples = Tuples

module type S = Legacy_diffable_intf.S
module type S_plain = Legacy_diffable_intf.S_plain
