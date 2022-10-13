open Core

module type S_plain = sig
  type t

  module Update : sig
    module Diff : sig
      type t [@@deriving sexp_of]
    end

    type t = Diff.t list [@@deriving sexp_of]
  end

  val update : t -> Update.t -> t
  val diffs : from:t -> to_:t -> Update.t

  (* This function should never return an empty list. *)
  val to_diffs : t -> Update.t
  val of_diffs : Update.t -> t
end

module type S1_plain = sig
  type 'a t

  module Update : sig
    module Diff : sig
      type 'a t [@@deriving sexp_of]
    end

    type 'a t = 'a Diff.t list [@@deriving sexp_of]
  end

  val update : 'a t -> 'a Update.t -> 'a t
  val diffs : ('a -> 'a -> int) -> from:'a t -> to_:'a t -> 'a Update.t

  (* This function should never return an empty list *)
  val to_diffs : 'a t -> 'a Update.t
  val of_diffs : 'a Update.t -> 'a t
end

module type S = sig
  module Update : sig
    module Diff : sig
      type t [@@deriving bin_io, sexp]
    end

    type t = Diff.t list [@@deriving bin_io, sexp]
  end

  include S_plain with module Update := Update
end

module type S1 = sig
  module Update : sig
    module Diff : sig
      type 'a t [@@deriving bin_io, sexp]
    end

    type 'a t = 'a Diff.t list [@@deriving bin_io, sexp]
  end

  include S1_plain with module Update := Update
end
