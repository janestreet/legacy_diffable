open! Core

module Make (O : Legacy_diffable_intf.S) (E : Legacy_diffable_intf.S) : sig
  include Legacy_diffable_intf.S with type t = (O.t, E.t) result
end

module Make_plain
    (O : Legacy_diffable_intf.S_plain)
    (E : Legacy_diffable_intf.S_plain) : sig
  include Legacy_diffable_intf.S_plain with type t = (O.t, E.t) result
end
