open Core

module Make (T : sig
  type t

  include Legacy_diffable_intf.S with type t := t
end) =
struct
  include T

  module Intermediate = struct
    type t = Update.t Stored_reversed.t

    module Part = Update.Diff

    let create () : t = Stored_reversed.empty
    let apply_part = Stored_reversed.snoc
  end

  let to_parts t = to_diffs t |> Sequence.of_list
  let finalize t = Stored_reversed.to_list t |> of_diffs
end

module Make_rpc (T : sig
  type t

  include Legacy_diffable_intf.S_plain with type t := t
end)
(Diff : sig
  type t [@@deriving bin_io]
end
with type t = T.Update.Diff.t) : Streamable.S_rpc with type t := T.t = struct
  include T

  module Intermediate = struct
    type t = Update.t Stored_reversed.t

    module Part = struct
      include Update.Diff
      include (Diff : Binable.S with type t := t)
    end

    let create () : t = Stored_reversed.empty
    let apply_part = Stored_reversed.snoc
  end

  let to_parts t = to_diffs t |> Sequence.of_list
  let finalize t = Stored_reversed.to_list t |> of_diffs
end

let%test_module "streamable test" =
  (module struct
    open Quickcheck

    module T = struct
      type t = unit Int.Map.t [@@deriving sexp_of, compare]

      let of_list =
        List.fold ~init:Int.Map.empty ~f:(fun map key -> Map.set map ~key ~data:())
      ;;

      let to_list = Map.keys

      let quickcheck_generator =
        Generator.map (List.quickcheck_generator Int.quickcheck_generator) ~f:of_list
      ;;

      let quickcheck_shrinker =
        Shrinker.map
          (List.quickcheck_shrinker Int.quickcheck_shrinker)
          ~f:of_list
          ~f_inverse:to_list
      ;;

      module Update = struct
        module Diff = struct
          type t = (int, int) Either.t [@@deriving sexp_of]
        end

        type t = Diff.t list [@@deriving sexp_of]
      end

      let update t diffs =
        List.fold diffs ~init:t ~f:(fun acc change ->
          match change with
          | First x -> Map.remove acc x
          | Second x -> Map.set acc ~key:x ~data:())
      ;;

      let diffs ~from ~to_ =
        Map.symmetric_diff from to_ ~data_equal:(fun () () -> true)
        |> Sequence.map ~f:(fun (key, change) ->
             match change with
             | `Left () -> First key
             | `Right () -> Second key
             | `Unequal ((), ()) -> failwith "BUG: Impossible case")
        |> Sequence.to_list
      ;;

      let of_diffs diffs =
        Sequence.of_list diffs
        |> Sequence.map ~f:(function
             | First _ -> failwith "of_diffs: Should not contain removals"
             | Second x -> x, ())
        |> Map.Using_comparator.of_increasing_sequence ~comparator:Int.comparator
        |> Or_error.ok_exn
      ;;

      let to_diffs to_ = diffs ~from:Int.Map.empty ~to_
    end

    open T

    module U =
      Make_rpc
        (T)
        (struct
          type t = (int, int) Either.Stable.V1.t [@@deriving bin_io]
        end)

    open U

    let%test_unit "streamable intermediate round-trip works" =
      let round_trip t =
        let acc = Intermediate.create () in
        let acc = Sequence.fold ~init:acc ~f:Intermediate.apply_part (to_parts t) in
        finalize acc
      in
      Quickcheck.test
        quickcheck_generator
        ~shrinker:quickcheck_shrinker
        ~sexp_of:[%sexp_of: t]
        ~f:(fun t -> [%test_result: t] ~expect:t (round_trip t))
    ;;

    let%test_unit "streamable round-trip works" =
      Quickcheck.test
        quickcheck_generator
        ~shrinker:quickcheck_shrinker
        ~sexp_of:[%sexp_of: t]
        ~f:(fun t -> [%test_result: t] ~expect:t (of_diffs (to_diffs t)))
    ;;

    let%test_unit "streamable diff/update works" =
      Quickcheck.test
        (Generator.tuple2 quickcheck_generator quickcheck_generator)
        ~shrinker:(Shrinker.tuple2 quickcheck_shrinker quickcheck_shrinker)
        ~sexp_of:[%sexp_of: t * t]
        ~f:(fun (from, to_) ->
        [%test_result: t] ~expect:to_ (update from (diffs ~from ~to_)))
    ;;
  end)
;;
