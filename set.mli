open Core

module Make (Key : sig
    type t [@@deriving sexp, bin_io]

    include Comparator.S with type t := t
  end) : sig
  include Legacy_diffable_intf.S with type t = Base.Set.M(Key).t

  val empty : t
end

module Make_plain (Key : sig
    type t [@@deriving sexp_of]

    include Comparator.S with type t := t
  end) : sig
  include Legacy_diffable_intf.S_plain with type t = Base.Set.M(Key).t

  val empty : t
end
