module Make (V : sig
  type t [@@deriving bin_io, sexp]

  val equal : t -> t -> bool
end) : sig
  include Legacy_diffable_intf.S with type t = V.t and type Update.Diff.t = V.t
end

module Make_plain (V : sig
  type t [@@deriving sexp_of]

  val equal : t -> t -> bool
end) : sig
  include Legacy_diffable_intf.S_plain with type t = V.t and type Update.Diff.t = V.t
end
