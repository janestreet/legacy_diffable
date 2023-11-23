open Core

module Make2_plain (A : Legacy_diffable_intf.S_plain) (B : Legacy_diffable_intf.S_plain) =
struct
  module Update = struct
    module Diff = struct
      type t =
        | A of A.Update.Diff.t
        | B of B.Update.Diff.t
      [@@deriving sexp_of]
    end

    type t = Diff.t list [@@deriving sexp_of]
  end

  type t = A.t * B.t

  let diffs ~from ~to_ =
    List.rev_append
      (A.diffs ~from:(fst from) ~to_:(fst to_)
       |> List.rev_map ~f:(fun x -> Update.Diff.A x))
      (B.diffs ~from:(snd from) ~to_:(snd to_) |> List.map ~f:(fun x -> Update.Diff.B x))
  ;;

  let update (a, b) dl =
    let da, db =
      List.partition_map dl ~f:(function
        | Update.Diff.A x -> First x
        | Update.Diff.B x -> Second x)
    in
    A.update a da, B.update b db
  ;;

  let to_diffs (ta, tb) =
    List.rev_append
      (A.to_diffs ta |> List.rev_map ~f:(fun x -> Update.Diff.A x))
      (B.to_diffs tb |> List.map ~f:(fun x -> Update.Diff.B x))
  ;;

  let of_diffs dl =
    let da, db =
      List.partition_map dl ~f:(function
        | Update.Diff.A x -> First x
        | Update.Diff.B x -> Second x)
    in
    A.of_diffs da, B.of_diffs db
  ;;
end

module Make2 (A : Legacy_diffable_intf.S) (B : Legacy_diffable_intf.S) = struct
  module Plain = Make2_plain (A) (B)

  module Update = struct
    module Diff = struct
      type t = Plain.Update.Diff.t =
        | A of A.Update.Diff.t
        | B of B.Update.Diff.t
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

module Make3_plain
  (A : Legacy_diffable_intf.S_plain)
  (B : Legacy_diffable_intf.S_plain)
  (C : Legacy_diffable_intf.S_plain) =
  Iso.Make_plain
    (Make2_plain (A) (Make2_plain (B) (C)))
       (struct
         type t = A.t * B.t * C.t

         let forwards (a, (b, c)) = a, b, c
         let backwards (a, b, c) = a, (b, c)
       end)

module Make3
  (A : Legacy_diffable_intf.S)
  (B : Legacy_diffable_intf.S)
  (C : Legacy_diffable_intf.S) =
  Iso.Make
    (Make2 (A) (Make2 (B) (C)))
       (struct
         type t = A.t * B.t * C.t

         let forwards (a, (b, c)) = a, b, c
         let backwards (a, b, c) = a, (b, c)
       end)

module Make4_plain
  (A : Legacy_diffable_intf.S_plain)
  (B : Legacy_diffable_intf.S_plain)
  (C : Legacy_diffable_intf.S_plain)
  (D : Legacy_diffable_intf.S_plain) =
  Iso.Make_plain
    (Make2_plain (A) (Make3_plain (B) (C) (D)))
       (struct
         type t = A.t * B.t * C.t * D.t

         let forwards (a, (b, c, d)) = a, b, c, d
         let backwards (a, b, c, d) = a, (b, c, d)
       end)

module Make4
  (A : Legacy_diffable_intf.S)
  (B : Legacy_diffable_intf.S)
  (C : Legacy_diffable_intf.S)
  (D : Legacy_diffable_intf.S) =
  Iso.Make
    (Make2 (A) (Make3 (B) (C) (D)))
       (struct
         type t = A.t * B.t * C.t * D.t

         let forwards (a, (b, c, d)) = a, b, c, d
         let backwards (a, b, c, d) = a, (b, c, d)
       end)

module Make5_plain
  (A : Legacy_diffable_intf.S_plain)
  (B : Legacy_diffable_intf.S_plain)
  (C : Legacy_diffable_intf.S_plain)
  (D : Legacy_diffable_intf.S_plain)
  (E : Legacy_diffable_intf.S_plain) =
  Iso.Make_plain
    (Make2_plain (A) (Make4_plain (B) (C) (D) (E)))
       (struct
         type t = A.t * B.t * C.t * D.t * E.t

         let forwards (a, (b, c, d, e)) = a, b, c, d, e
         let backwards (a, b, c, d, e) = a, (b, c, d, e)
       end)

module Make5
  (A : Legacy_diffable_intf.S)
  (B : Legacy_diffable_intf.S)
  (C : Legacy_diffable_intf.S)
  (D : Legacy_diffable_intf.S)
  (E : Legacy_diffable_intf.S) =
  Iso.Make
    (Make2 (A) (Make4 (B) (C) (D) (E)))
       (struct
         type t = A.t * B.t * C.t * D.t * E.t

         let forwards (a, (b, c, d, e)) = a, b, c, d, e
         let backwards (a, b, c, d, e) = a, (b, c, d, e)
       end)

module Make6_plain
  (A : Legacy_diffable_intf.S_plain)
  (B : Legacy_diffable_intf.S_plain)
  (C : Legacy_diffable_intf.S_plain)
  (D : Legacy_diffable_intf.S_plain)
  (E : Legacy_diffable_intf.S_plain)
  (F : Legacy_diffable_intf.S_plain) =
  Iso.Make_plain
    (Make2_plain (A) (Make5_plain (B) (C) (D) (E) (F)))
       (struct
         type t = A.t * B.t * C.t * D.t * E.t * F.t

         let forwards (a, (b, c, d, e, f)) = a, b, c, d, e, f
         let backwards (a, b, c, d, e, f) = a, (b, c, d, e, f)
       end)

module Make6
  (A : Legacy_diffable_intf.S)
  (B : Legacy_diffable_intf.S)
  (C : Legacy_diffable_intf.S)
  (D : Legacy_diffable_intf.S)
  (E : Legacy_diffable_intf.S)
  (F : Legacy_diffable_intf.S) =
  Iso.Make
    (Make2 (A) (Make5 (B) (C) (D) (E) (F)))
       (struct
         type t = A.t * B.t * C.t * D.t * E.t * F.t

         let forwards (a, (b, c, d, e, f)) = a, b, c, d, e, f
         let backwards (a, b, c, d, e, f) = a, (b, c, d, e, f)
       end)

let%test_module "tests" =
  (module struct
    module Diffable_int = struct
      module U = struct
        type t = int [@@deriving bin_io, equal, sexp]
      end

      include U
      include Atomic.Make (U)
    end

    module Diffable_float = struct
      module U = struct
        type t = float [@@deriving bin_io, equal, sexp]
      end

      include U
      include Atomic.Make (U)
    end

    include
      Make6 (Diffable_int) (Diffable_int) (Diffable_float) (Diffable_int) (Diffable_float)
        (Diffable_float)

    type t = int * int * float * int * float * float [@@deriving compare, sexp]

    let quickcheck_generator =
      Quickcheck.Generator.tuple6
        Int.quickcheck_generator
        Int.quickcheck_generator
        Float.quickcheck_generator
        Int.quickcheck_generator
        Float.quickcheck_generator
        Float.quickcheck_generator
    ;;

    let quickcheck_shrinker =
      Quickcheck.Shrinker.tuple6
        Int.quickcheck_shrinker
        Int.quickcheck_shrinker
        Float.quickcheck_shrinker
        Int.quickcheck_shrinker
        Float.quickcheck_shrinker
        Float.quickcheck_shrinker
    ;;

    let%test_unit "make6 round-trip works" =
      Quickcheck.test
        quickcheck_generator
        ~shrinker:quickcheck_shrinker
        ~sexp_of:[%sexp_of: t]
        ~f:(fun t -> [%test_result: t] ~expect:t (of_diffs (to_diffs t)))
    ;;

    let%test_unit "make6 diff/update works" =
      let open Quickcheck in
      Quickcheck.test
        (Generator.tuple2 quickcheck_generator quickcheck_generator)
        ~shrinker:(Shrinker.tuple2 quickcheck_shrinker quickcheck_shrinker)
        ~sexp_of:[%sexp_of: t * t]
        ~f:(fun (from, to_) ->
        [%test_result: t] ~expect:to_ (update from (diffs ~from ~to_)))
    ;;
  end)
;;
