module Atomic_ = Atomic
open Core
module Atomic = Atomic_

module Make_plain (O : Legacy_diffable_intf.S_plain) (E : Legacy_diffable_intf.S_plain) =
struct
  module Update = struct
    module Diff = struct
      type t =
        | Change_ok of O.Update.Diff.t
        | Change_error of E.Update.Diff.t
      [@@deriving sexp_of, variants]

      (** Return the longest suffix of a list of [t] which is the same case of the
          variant. *)
      let longest_suffix_with_same_variant =
        let rec go xs ~start_of_group =
          match xs with
          | [] -> start_of_group
          | head :: tail ->
            let first_in_group = List.hd_exn start_of_group in
            if Variants.to_rank head = Variants.to_rank first_in_group
            then go tail ~start_of_group
            else go tail ~start_of_group:xs
        in
        let longest_suffix_with_same_variant xs = go xs ~start_of_group:xs in
        longest_suffix_with_same_variant
      ;;
    end

    type t = Diff.t list [@@deriving sexp_of]
  end

  type t = (O.t, E.t) Result.t

  module Update_or_of_diffs = struct
    type ('a, 'b) t =
      | Update : ('a, 'a) t
      | Of_diffs : ('a, unit) t
  end

  let update_or_of_diffs (type arg) (op : (t, arg) Update_or_of_diffs.t) (t : arg) diffs
    : t
    =
    (* Every time we switch to ok/error we forget any value before
       that point, so we can just look at the last group with the same kind.
    *)
    match Update.Diff.longest_suffix_with_same_variant diffs with
    | [] ->
      (match op with
       | Update -> t
       | Of_diffs -> failwith "BUG: Need at least one diff to construct a result value")
    | Change_ok _ :: _ as ok_diffs ->
      let ok_diffs =
        List.map ok_diffs ~f:(function
          | Change_ok x -> x
          | Change_error _ -> failwith "BUG: Hit impossible case")
      in
      (match op, t with
       | Update, Ok t -> Ok (O.update t ok_diffs)
       | _ -> Ok (O.of_diffs ok_diffs))
    | Change_error _ :: _ as error_diffs ->
      let error_diffs =
        List.map error_diffs ~f:(function
          | Update.Diff.Change_error x -> x
          | Change_ok _ -> failwith "BUG: Hit impossible case")
      in
      (match op, t with
       | Update, Error t -> Error (E.update t error_diffs)
       | _ -> Error (E.of_diffs error_diffs))
  ;;

  let update t diffs = update_or_of_diffs Update t diffs
  let of_diffs diffs = update_or_of_diffs Of_diffs () diffs

  let to_diffs = function
    | Ok ok -> List.map (O.to_diffs ok) ~f:Update.Diff.change_ok
    | Error error -> List.map (E.to_diffs error) ~f:Update.Diff.change_error
  ;;

  let diffs ~from ~to_ : Update.t =
    match from, to_ with
    | Error _, Ok _ | Ok _, Error _ -> to_diffs to_
    | Ok from, Ok to_ -> List.map (O.diffs ~from ~to_) ~f:Update.Diff.change_ok
    | Error from, Error to_ -> List.map (E.diffs ~from ~to_) ~f:Update.Diff.change_error
  ;;
end

module Make (O : Legacy_diffable_intf.S) (E : Legacy_diffable_intf.S) = struct
  module Plain = Make_plain (O) (E)

  module Update = struct
    module Diff = struct
      type t = Plain.Update.Diff.t =
        | Change_ok of O.Update.Diff.t
        | Change_error of E.Update.Diff.t
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

module%test [@name "diffable option"] _ = struct
  module X = Atomic.Make (Int)
  module O = Make (X) (X)
  module O2 = Make (O) (O)

  type t = (int, int) Result.t [@@deriving sexp, compare]

  let test1 ~one ~two =
    [%test_result: t] (O.of_diffs (O.to_diffs one)) ~expect:one;
    [%test_result: t] (O.of_diffs (O.to_diffs two)) ~expect:two;
    [%test_result: t] (O.update one (O.diffs ~from:one ~to_:two)) ~expect:two;
    [%test_result: t] (O.update two (O.diffs ~from:two ~to_:one)) ~expect:one
  ;;

  let test2 ~one ~two =
    [%test_result: (t, t) Result.t] (O2.of_diffs (O2.to_diffs one)) ~expect:one;
    [%test_result: (t, t) Result.t] (O2.of_diffs (O2.to_diffs two)) ~expect:two;
    [%test_result: (t, t) Result.t]
      (O2.update one (O2.diffs ~from:one ~to_:two))
      ~expect:two;
    [%test_result: (t, t) Result.t]
      (O2.update two (O2.diffs ~from:two ~to_:one))
      ~expect:one
  ;;

  let%test_unit "diffs" =
    let values =
      let%bind.List number = [ 1; 2; 3; 11; 20; 42; 99; 1024 ] in
      [ Ok number; Error number ]
    in
    List.iter (List.cartesian_product values values) ~f:(fun (one, two) ->
      test1 ~one ~two);
    let values2 =
      let%bind.List number = [ 1; 2; 5; 8; 99 ] in
      let%bind.List inner = [ Ok number; Error number ] in
      [ Ok inner; Error inner ]
    in
    List.iter (List.cartesian_product values2 values2) ~f:(fun (one, two) ->
      test2 ~one ~two)
  ;;
end
