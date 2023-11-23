open Core

module Make (Key : sig
  type t [@@deriving sexp, bin_io]

  include Comparator.S with type t := t
end) (Value : sig
  type t [@@deriving sexp, bin_io]

  val equal : t -> t -> bool
end) : sig
  type t = Value.t Base.Map.M(Key).t [@@deriving sexp_of]

  module Update : sig
    module Diff : sig
      type t =
        | Remove of Key.t
        | Add of Key.t * Value.t
        | Idle of unit
      [@@deriving bin_io, sexp]
    end

    type t = Diff.t list [@@deriving bin_io, sexp]
  end

  include Legacy_diffable_intf.S with type t := t with module Update := Update

  val empty : t
end

module Make_plain (Key : sig
  type t [@@deriving sexp_of]

  include Comparator.S with type t := t
end) (Value : sig
  type t [@@deriving sexp_of]

  val equal : t -> t -> bool
end) : sig
  type t = Value.t Base.Map.M(Key).t [@@deriving sexp_of]

  module Update : sig
    module Diff : sig
      type t =
        | Remove of Key.t
        | Add of Key.t * Value.t
        | Idle of unit
      [@@deriving sexp_of]
    end

    type t = Diff.t list [@@deriving sexp_of]
  end

  include Legacy_diffable_intf.S_plain with type t := t and module Update := Update

  val empty : t
end

module Make_with_value_diffs (Key : sig
  type t [@@deriving sexp, bin_io]

  include Comparator.S with type t := t
end) (Value : sig
  type t

  include Legacy_diffable_intf.S with type t := t
end) : sig
  module Update : sig
    module Diff : sig
      type t =
        | Remove of Key.t
        | Change of Key.t * Value.Update.Diff.t
        | Add of Key.t * Value.Update.Diff.t
        | Idle of unit
      [@@deriving sexp, bin_io]
    end

    type t = Diff.t list [@@deriving sexp, bin_io]
  end

  include
    Legacy_diffable_intf.S
      with type t = Value.t Base.Map.M(Key).t
       and module Update := Update

  val empty : t
end

module Make_plain_with_value_diffs (Key : sig
  type t [@@deriving sexp_of]

  include Comparator.S with type t := t
end) (Value : sig
  type t

  include Legacy_diffable_intf.S_plain with type t := t
end) : sig
  module Update : sig
    module Diff : sig
      type t =
        | Remove of Key.t
        | Change of Key.t * Value.Update.Diff.t
        | Add of Key.t * Value.Update.Diff.t
        | Idle of unit
      [@@deriving sexp_of]
    end

    type t = Diff.t list [@@deriving sexp_of]
  end

  include
    Legacy_diffable_intf.S_plain
      with type t = Value.t Base.Map.M(Key).t
       and module Update := Update

  val empty : t
end
