open! Core

module Make (O : Diffable_intf.S) (E : Diffable_intf.S) : sig
  include Diffable_intf.S with type t = (O.t, E.t) result
end

module Make_plain (O : Diffable_intf.S_plain) (E : Diffable_intf.S_plain) : sig
  include Diffable_intf.S_plain with type t = (O.t, E.t) result
end
