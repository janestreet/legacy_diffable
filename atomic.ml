open Core

module Make_plain (V : sig
    type t [@@deriving sexp_of]

    val equal : t -> t -> bool
  end) =
struct
  module Update = struct
    module Diff = struct
      type t = V.t [@@deriving sexp_of]
    end

    type t = Diff.t list [@@deriving sexp_of]
  end

  type t = V.t

  let update t d =
    match d with
    | [] -> t
    | [ t' ] -> t'
    | _ :: non_empty_tail -> List.last_exn non_empty_tail
  ;;

  let diffs ~from ~to_ = if phys_equal from to_ || V.equal from to_ then [] else [ to_ ]
  let to_diffs t = [ t ]

  let of_diffs d =
    match d with
    | [ t ] -> t
    | _ :: non_empty_tail -> List.last_exn non_empty_tail
    | [] ->
      failwith
        "Invalid of_diffs input. Update.t for atomic must contain at least one element."
  ;;
end

module Make (V : sig
    type t [@@deriving bin_io, sexp]

    val equal : t -> t -> bool
  end) =
struct
  module Plain = Make_plain (V)

  module Update = struct
    module Diff = struct
      include Plain.Update.Diff

      include (
        V :
          sig
            type t [@@deriving bin_io, sexp]
          end
          with type t := t)
    end

    type t = Diff.t list [@@deriving bin_io, sexp]
  end

  include (
    Plain :
      module type of struct
        include Plain
      end
      with module Update := Plain.Update)
end

module%test [@name "tests"] _ = struct
  module T = struct
    type t = int [@@deriving bin_io, equal, sexp]
  end

  include T
  include Make (T)

  let%test_unit "atomic round-trip works" =
    Quickcheck.test
      Int.quickcheck_generator
      ~shrinker:Int.quickcheck_shrinker
      ~sexp_of:[%sexp_of: t]
      ~f:(fun t -> [%test_result: t] ~expect:t (of_diffs (to_diffs t)))
  ;;

  let%test_unit "atomic diff/update works" =
    let open Quickcheck in
    Quickcheck.test
      (Generator.tuple2 Int.quickcheck_generator Int.quickcheck_generator)
      ~shrinker:(Shrinker.tuple2 Int.quickcheck_shrinker Int.quickcheck_shrinker)
      ~sexp_of:[%sexp_of: t * t]
      ~f:(fun (from, to_) ->
        [%test_result: t] ~expect:to_ (update from (diffs ~from ~to_)))
  ;;
end
