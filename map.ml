module Atomic_ = Atomic
open Core
module Atomic = Atomic_

module Make_plain
    (Key : sig
       type t [@@deriving sexp_of]

       include Comparator.S with type t := t
     end)
    (Value : sig
       type t [@@deriving sexp_of]

       val equal : t -> t -> bool
     end) =
struct
  module Update = struct
    module Diff = struct
      type t =
        | Remove of Key.t
        | Add of Key.t * Value.t
        (* We add a [unit] argument so matching the more common cases is more performant
           (no check for immediate vs. block). *)
        | Idle of unit
      [@@deriving sexp_of]

      let idle = Idle ()
    end

    type t = Diff.t list [@@deriving sexp_of]
  end

  type t = Value.t Map.M(Key).t [@@deriving sexp_of]

  let empty = Map.empty (module Key)

  let update t diffs =
    List.fold diffs ~init:t ~f:(fun t d ->
      match (d : Update.Diff.t) with
      | Remove key -> Map.remove t key
      | Add (key, data) -> Map.set t ~key ~data
      | Idle () -> t)
  ;;

  let diffs ~from ~to_ : Update.t =
    let data_equal x1 x2 = phys_equal x1 x2 || Value.equal x1 x2 in
    (* This reverses the order relative to [Map.symmetric_diff |> Sequence.to_list], but
       that's fine since the operations in this diff type all commute. *)
    Map.fold_symmetric_diff ~init:[] ~data_equal from to_ ~f:(fun acc (k, d) ->
      match d with
      | `Left _ -> Update.Diff.Remove k :: acc
      | `Right i | `Unequal (_, i) -> Add (k, i) :: acc)
  ;;

  let of_diffs diffs = update empty diffs

  let to_diffs t : Update.t =
    let l =
      Map.fold_right t ~init:[] ~f:(fun ~key ~data tail ->
        Update.Diff.Add (key, data) :: tail)
    in
    match l with
    | [] -> [ Update.Diff.idle ]
    | l -> l
  ;;
end

module Make
    (Key : sig
       type t [@@deriving sexp, bin_io]

       include Comparator.S with type t := t
     end)
    (Value : sig
       type t [@@deriving sexp, bin_io]

       val equal : t -> t -> bool
     end) =
struct
  module Plain = Make_plain (Key) (Value)

  module Update = struct
    module Diff = struct
      type t = Plain.Update.Diff.t =
        | Remove of Key.t
        | Add of Key.t * Value.t
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

module%test [@name "tests of Make and Make_plain"] _ = struct
  module T = struct
    include Make_plain (Int) (Int)

    type t = int Int.Map.t [@@deriving compare, equal, sexp_of]

    let quickcheck_generator =
      Int.Map.quickcheck_generator Int.quickcheck_generator Int.quickcheck_generator
    ;;
  end

  include T

  let%expect_test "map round-trip works" =
    let module Test_case = struct
      type t =
        { t : T.t
        ; to_diffs : T.Update.t
        }
      [@@deriving sexp_of]
    end
    in
    Quickcheck.test
      ~sexp_of:[%sexp_of: Test_case.t]
      (let open Quickcheck.Let_syntax in
       let%bind t = quickcheck_generator in
       let%map to_diffs = List.gen_permutations (to_diffs t) in
       ({ t; to_diffs } : Test_case.t))
      ~f:(fun { t; to_diffs } -> [%test_result: t] ~expect:t (of_diffs to_diffs));
    [%expect {| |}]
  ;;

  let%expect_test "map diff/update works" =
    let module Test_case = struct
      type t =
        { from : T.t
        ; to_ : T.t
        ; diffs : T.Update.t
        }
      [@@deriving sexp_of]
    end
    in
    Quickcheck.test
      ~sexp_of:[%sexp_of: Test_case.t]
      (let open Quickcheck.Let_syntax in
       let%bind from = quickcheck_generator
       and to_ = quickcheck_generator in
       let%map diffs = List.gen_permutations (diffs ~from ~to_) in
       ({ from; to_; diffs } : Test_case.t))
      ~f:(fun { from; to_; diffs } -> [%test_result: t] ~expect:to_ (update from diffs));
    [%expect {| |}]
  ;;
end

module Make_plain_with_value_diffs
    (Key : sig
       type t [@@deriving sexp_of]

       include Comparator.S with type t := t
     end)
    (Value : sig
       type t

       include Legacy_diffable_intf.S_plain with type t := t
     end) =
struct
  module Update = struct
    module Diff = struct
      type t =
        | Remove of Key.t
        | Change of Key.t * Value.Update.Diff.t
        | Add of Key.t * Value.Update.Diff.t
        | Idle of unit
        (** We add a [unit] argument so matching the more common cases is more performant
            (no check for immediate vs. block). *)
      [@@deriving sexp_of]

      let idle = Idle ()
    end

    type t = Diff.t list [@@deriving sexp_of]
  end

  type t = Value.t Map.M(Key).t

  let empty = Map.empty (module Key)

  let update =
    let group_diffs =
      let module State = struct
        type t =
          | Out_of_group of { diffs : Update.t }
          | In_group of
              { key : Key.t
              ; type_ : [ `Add | `Change ]
              ; group : Update.t Stored_reversed.t
              ; diffs : Update.t
              }
      end
      in
      let open State in
      let step = function
        | In_group { key; type_; group; diffs } ->
          (match type_, diffs with
           | `Change, (Change (next_key, _) as change) :: diffs
             when (Comparator.compare Key.comparator) next_key key = 0 ->
             Sequence.Step.Skip
               { state =
                   In_group
                     { key; type_; diffs; group = Stored_reversed.snoc group change }
               }
           | `Add, (Add (next_key, _) as add) :: diffs
             when (Comparator.compare Key.comparator) next_key key = 0 ->
             Sequence.Step.Skip
               { state =
                   In_group { key; type_; diffs; group = Stored_reversed.snoc group add }
               }
           | _, diffs ->
             Sequence.Step.Yield { value = group; state = Out_of_group { diffs } })
        | Out_of_group { diffs } ->
          (match diffs with
           | [] -> Sequence.Step.Done
           | (Remove _ as single) :: diffs ->
             Sequence.Step.Yield
               { value = Stored_reversed.singleton single
               ; state = Out_of_group { diffs }
               }
           | (Add (key, _) as add) :: diffs ->
             Sequence.Step.Skip
               { state =
                   In_group
                     { key; type_ = `Add; group = Stored_reversed.singleton add; diffs }
               }
           | (Change (key, _) as change) :: diffs ->
             Sequence.Step.Skip
               { state =
                   In_group
                     { key
                     ; type_ = `Change
                     ; group = Stored_reversed.singleton change
                     ; diffs
                     }
               }
           | Idle () :: diffs -> Sequence.Step.Skip { state = Out_of_group { diffs } })
      in
      fun diffs -> Sequence.unfold_step ~init:(Out_of_group { diffs }) ~f:step
    in
    fun t diffs ->
      Sequence.fold ~init:t (group_diffs diffs) ~f:(fun t ds ->
        match Stored_reversed.to_list_rev ds with
        | Add (key, _) :: _ ->
          Map.set
            t
            ~key
            ~data:
              (Value.of_diffs
                 (Stored_reversed.map_to_list ds ~f:(function
                   | Change _ | Remove _ | Idle () ->
                     failwith "BUG: The impossible happened. Change/Remove in add group."
                   | Add (_, x) -> x)))
        | [ Remove key ] -> Map.remove t key
        | Change (key, _) :: _ ->
          Map.update t key ~f:(function
            | None ->
              failwith "BUG: The impossible happened. Update to a non existing key."
            | Some value ->
              Value.update
                value
                (Stored_reversed.map_to_list ds ~f:(function
                  | Add _ | Remove _ | Idle () ->
                    failwith "BUG: The impossible happened. Add/Remove in change group."
                  | Change (_, x) -> x)))
        | _ ->
          failwith
            "BUG: The impossible happened. Expected single Add/Remove or multiple Change \
             in a group.")
  ;;

  let diffs ~from ~to_ : Update.t =
    (* This preserves the order relative to [Map.symmetric_diff |> Sequence.to_list],
       which is necessary for stability since the operations in this diff type do not
       commute (in particular, commutation is not safe between [Add] and [Change]
       constructors with the same key). *)
    Map.fold_symmetric_diff
      ~init:Stored_reversed.empty
      ~data_equal:phys_equal
      from
      to_
      ~f:(fun acc (k, d) ->
        match d with
        | `Left _ -> Stored_reversed.snoc acc (Update.Diff.Remove k)
        | `Right to_ ->
          let diffs = Value.to_diffs to_ in
          Stored_reversed.map_append acc diffs ~f:(fun x -> Update.Diff.Add (k, x))
        | `Unequal (from, to_) ->
          let diffs = Value.diffs ~from ~to_ in
          Stored_reversed.map_append acc diffs ~f:(fun x -> Update.Diff.Change (k, x)))
    |> Stored_reversed.to_list
  ;;

  let of_diffs = update empty

  let to_diffs t =
    let l =
      Map.fold_right t ~init:[] ~f:(fun ~key ~data tail ->
        let rev_instructions = Stored_reversed.of_list (Value.to_diffs data) in
        Stored_reversed.map_to_list ~tail rev_instructions ~f:(fun x ->
          Update.Diff.Add (key, x)))
    in
    match l with
    | [] -> [ Update.Diff.idle ]
    | l -> l
  ;;
end

module Make_with_value_diffs
    (Key : sig
       type t [@@deriving sexp, bin_io]

       include Comparator.S with type t := t
     end)
    (Value : sig
       type t

       include Legacy_diffable_intf.S with type t := t
     end) =
struct
  module Plain = Make_plain_with_value_diffs (Key) (Value)

  module Update = struct
    module Diff = struct
      type t = Plain.Update.Diff.t =
        | Remove of Key.t
        | Change of Key.t * Value.Update.Diff.t
        | Add of Key.t * Value.Update.Diff.t
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

module%test [@name "tests of Make_with_value_diffs"] _ = struct
  module T = struct
    include
      Make_with_value_diffs
        (Int)
        (struct
          include Int
          include Atomic.Make (Int)
        end)

    type t = int Int.Map.t [@@deriving compare, equal, sexp_of]

    let quickcheck_generator =
      Int.Map.quickcheck_generator Int.quickcheck_generator Int.quickcheck_generator
    ;;
  end

  include T

  let%expect_test "map with value diffs round-trip works" =
    let module Test_case = struct
      type t =
        { t : T.t
        ; to_diffs : T.Update.t
        }
      [@@deriving sexp_of]
    end
    in
    Quickcheck.test
      ~sexp_of:[%sexp_of: Test_case.t]
      (let open Quickcheck.Let_syntax in
       let%bind t = quickcheck_generator in
       let%map to_diffs = List.gen_permutations (to_diffs t) in
       ({ t; to_diffs } : Test_case.t))
      ~f:(fun { t; to_diffs } -> [%test_result: t] ~expect:t (of_diffs to_diffs));
    [%expect {| |}]
  ;;

  let%expect_test "map with value diffs diff/update works" =
    let module Test_case = struct
      type t =
        { from : T.t
        ; to_ : T.t
        ; diffs : T.Update.t
        }
      [@@deriving sexp_of]
    end
    in
    Quickcheck.test
      ~sexp_of:[%sexp_of: Test_case.t]
      (let open Quickcheck.Let_syntax in
       let%bind from = quickcheck_generator
       and to_ = quickcheck_generator in
       let%map diffs = List.gen_permutations (diffs ~from ~to_) in
       ({ from; to_; diffs } : Test_case.t))
      ~f:(fun { from; to_; diffs } -> [%test_result: t] ~expect:to_ (update from diffs));
    [%expect {| |}]
  ;;
end
