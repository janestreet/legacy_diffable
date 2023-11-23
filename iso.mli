module Make
  (S : Legacy_diffable_intf.S) (X : sig
    type t

    val forwards : S.t -> t
    val backwards : t -> S.t
  end) : Legacy_diffable_intf.S with type t = X.t

module Make_plain
  (S : Legacy_diffable_intf.S_plain) (X : sig
    type t

    val forwards : S.t -> t
    val backwards : t -> S.t
  end) : Legacy_diffable_intf.S_plain with type t = X.t
