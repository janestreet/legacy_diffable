module Atomic_ = Atomic
open Core
module Atomic = Atomic_

module Make_plain (X : sig
    type t

    include Legacy_diffable_intf.S_plain with type t := t
  end) =
struct
  module Update = struct
    module Diff = struct
      type t =
        | Change of X.Update.Diff.t
        | Set_to_none
        | Idle
      [@@deriving sexp_of]
    end

    type t = Diff.t list [@@deriving sexp_of]
  end

  type t = X.t option

  let update t (diffs : Update.t) =
    Sequence.of_list diffs
    |> Sequence.group ~break:(fun l r ->
      match l, r with
      | Change _, Change _ -> false
      | _ -> true)
    |> Sequence.fold ~init:t ~f:(fun t change ->
      match change with
      | [ Idle ] -> t
      | [ Set_to_none ] -> None
      | Change _ :: _ as diffs ->
        let diffs =
          List.map diffs ~f:(function
            | Idle | Set_to_none -> failwith "BUG: Hit impossible case"
            | Change x -> x)
        in
        (match t with
         | None -> Some (X.of_diffs diffs)
         | Some t -> Some (X.update t diffs))
      | _ -> failwith "BUG: Hit impossible case")
  ;;

  let diffs ~from ~to_ : Update.t =
    match from, to_ with
    | None, None -> [ Idle ]
    | Some _, None -> [ Set_to_none ]
    | None, Some to_ -> List.map (X.to_diffs to_) ~f:(fun diff -> Update.Diff.Change diff)
    | Some from, Some to_ ->
      List.map (X.diffs ~from ~to_) ~f:(fun diff -> Update.Diff.Change diff)
  ;;

  let of_diffs = update None
  let to_diffs t = diffs ~from:None ~to_:t
end

module Make (X : sig
    type t

    include Legacy_diffable_intf.S with type t := t
  end) =
struct
  module Plain = Make_plain (X)

  module Update = struct
    module Diff = struct
      type t = Plain.Update.Diff.t =
        | Change of X.Update.Diff.t
        | Set_to_none
        | Idle
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
  module O = Make (X)
  module O2 = Make (O)

  let test1 ~one ~two =
    [%test_result: int option] (O.of_diffs (O.to_diffs one)) ~expect:one;
    [%test_result: int option] (O.of_diffs (O.to_diffs two)) ~expect:two;
    [%test_result: int option] (O.update one (O.diffs ~from:one ~to_:two)) ~expect:two;
    [%test_result: int option] (O.update two (O.diffs ~from:two ~to_:one)) ~expect:one
  ;;

  let test2 ~one ~two =
    [%test_result: int option option] (O2.of_diffs (O2.to_diffs one)) ~expect:one;
    [%test_result: int option option] (O2.of_diffs (O2.to_diffs two)) ~expect:two;
    [%test_result: int option option]
      (O2.update one (O2.diffs ~from:one ~to_:two))
      ~expect:two;
    [%test_result: int option option]
      (O2.update two (O2.diffs ~from:two ~to_:one))
      ~expect:one
  ;;

  let%test_unit "diffs" =
    test1 ~one:None ~two:None;
    test1 ~one:None ~two:(Some 23);
    test1 ~one:(Some 23) ~two:None;
    test1 ~one:(Some 23) ~two:(Some 42);
    test2 ~one:None ~two:None;
    test2 ~one:None ~two:(Some None);
    test2 ~one:None ~two:(Some (Some 23));
    test2 ~one:(Some None) ~two:None;
    test2 ~one:(Some None) ~two:(Some None);
    test2 ~one:(Some None) ~two:(Some (Some 23));
    test2 ~one:(Some (Some 23)) ~two:None;
    test2 ~one:(Some (Some 23)) ~two:(Some None);
    test2 ~one:(Some (Some 23)) ~two:(Some (Some 23))
  ;;
end
