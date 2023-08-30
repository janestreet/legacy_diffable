open Core

module Make_plain (Key : sig
  type t [@@deriving sexp_of]

  include Comparator.S with type t := t
end) =
struct
  module Update = struct
    module Diff = struct
      type t =
        | Remove of Key.t
        | Add of Key.t
        (* We add a [unit] argument so matching the more common cases
           is more performant (no check for immediate vs. block). *)
        | Idle of unit
      [@@deriving sexp_of]

      let idle = Idle ()
    end

    type t = Diff.t list [@@deriving sexp_of]
  end

  type t = Set.M(Key).t

  let empty = Set.empty (module Key)

  let update t diffs =
    List.fold diffs ~init:t ~f:(fun t d ->
      match (d : Update.Diff.t) with
      | Remove key -> Set.remove t key
      | Add key -> Set.add t key
      | Idle () -> t)
  ;;

  let diffs ~from ~to_ : Update.t =
    Set.symmetric_diff from to_
    |> Sequence.map ~f:(function
         | First k -> Update.Diff.Remove k
         | Second k -> Add k)
    |> Sequence.to_list
  ;;

  let of_diffs (diffs : Update.t) =
    List.fold diffs ~init:empty ~f:(fun t d ->
      match d with
      | Remove _ -> failwith "Remove is not allowed in of_diffs."
      | Add key -> Set.add t key
      | Idle () -> t)
  ;;

  let to_diffs (t : t) : Update.t =
    let l = Set.to_list t |> List.map ~f:(fun k -> Update.Diff.Add k) in
    match l with
    | [] -> [ Update.Diff.idle ]
    | l -> l
  ;;
end

module Make (Key : sig
  type t [@@deriving sexp, bin_io]

  include Comparator.S with type t := t
end) =
struct
  module Plain = Make_plain (Key)

  module Update = struct
    module Diff = struct
      type t = Plain.Update.Diff.t =
        | Remove of Key.t
        | Add of Key.t
        | Idle of unit
      [@@deriving sexp, bin_io]
    end

    type t = Diff.t list [@@deriving sexp, bin_io]
  end

  include (
    Plain :
      module type of struct
        include Plain
      end
      with module Update := Plain.Update)
end

let%test_module "tests" =
  (module struct
    module T = struct
      module U = struct
        type t = int [@@deriving bin_io, compare, equal, sexp]
      end

      include U
      include Comparable.Make (U)
    end

    include T
    include Make (T)

    let%test_unit "atomic round-trip works" =
      Quickcheck.test
        (Quickcheck.Generator.list Int.quickcheck_generator)
        ~sexp_of:[%sexp_of: int list]
        ~f:(fun ns ->
        let t = T.Set.of_list ns in
        [%test_result: T.Set.t] ~expect:t (of_diffs (to_diffs t)))
    ;;

    let%test_unit "atomic diff/update works" =
      let open Quickcheck in
      Quickcheck.test
        (Generator.tuple2
           (Quickcheck.Generator.list Int.quickcheck_generator)
           (Quickcheck.Generator.list Int.quickcheck_generator))
        ~sexp_of:[%sexp_of: int list * int list]
        ~f:(fun (from, to_) ->
          let from = T.Set.of_list from in
          let to_ = T.Set.of_list to_ in
          [%test_result: T.Set.t] ~expect:to_ (update from (diffs ~from ~to_)))
    ;;
  end)
;;
