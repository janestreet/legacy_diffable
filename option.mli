open! Core

module Make (X : sig
    type t

    include Legacy_diffable_intf.S with type t := t
  end) : sig
  include Legacy_diffable_intf.S with type t = X.t option
end

module Make_plain (X : sig
    type t

    include Legacy_diffable_intf.S_plain with type t := t
  end) : sig
  include Legacy_diffable_intf.S_plain with type t = X.t option
end
