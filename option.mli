open! Core

module Make (X : sig
    type t

    include Diffable_intf.S with type t := t
  end) : sig
  include Diffable_intf.S with type t = X.t option
end

module Make_plain (X : sig
    type t

    include Diffable_intf.S_plain with type t := t
  end) : sig
  include Diffable_intf.S_plain with type t = X.t option
end
