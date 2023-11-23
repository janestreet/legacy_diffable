open! Core

module Make_plain
  (S : Legacy_diffable_intf.S_plain) (X : sig
    type t

    val forwards : S.t -> t
    val backwards : t -> S.t
  end) =
struct
  module Update = S.Update

  type t = X.t

  let update t diffs = S.update (X.backwards t) diffs |> X.forwards

  let diffs ~from ~to_ : Update.t =
    S.diffs ~from:(X.backwards from) ~to_:(X.backwards to_)
  ;;

  let to_diffs t = S.to_diffs (X.backwards t)
  let of_diffs d = S.of_diffs d |> X.forwards
end

module Make
  (S : Legacy_diffable_intf.S) (X : sig
    type t

    val forwards : S.t -> t
    val backwards : t -> S.t
  end) =
struct
  module Plain = Make_plain (S) (X)
  module Update = S.Update

  include (
    Plain :
      module type of struct
        include Plain
      end
      with module Update := Plain.Update)
end

let%test_module "tests" =
  (module struct
    module Diffable_float = struct
      module T = struct
        type t = string [@@deriving bin_io, equal, sexp]
      end

      include T
      include Atomic.Make (T)
    end

    module U = struct
      type t = int [@@deriving sexp]

      let forwards = Int.of_string
      let backwards = Int.to_string
    end

    include U
    include Make (Diffable_float) (U)

    let%test_unit "iso round-trip works" =
      Quickcheck.test
        Int.quickcheck_generator
        ~shrinker:Int.quickcheck_shrinker
        ~sexp_of:[%sexp_of: t]
        ~f:(fun t -> [%test_result: int] ~expect:t (of_diffs (to_diffs t)))
    ;;

    let%test_unit "iso diff/update works" =
      let open Quickcheck in
      Quickcheck.test
        (Generator.tuple2 Int.quickcheck_generator Int.quickcheck_generator)
        ~shrinker:(Shrinker.tuple2 Int.quickcheck_shrinker Int.quickcheck_shrinker)
        ~sexp_of:[%sexp_of: t * t]
        ~f:(fun (from, to_) ->
        [%test_result: int] ~expect:to_ (update from (diffs ~from ~to_)))
    ;;
  end)
;;
