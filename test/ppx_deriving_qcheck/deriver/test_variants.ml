open QCheck
open Helpers

(** {1. Test variants and polymorphic variants derivation} *)

(** {2. Variants} *)

type colors = Red | Green | Blue [@@deriving qcheck]

let pp_colors fmt x =
  let open Format in
  match x with
  | Red -> fprintf fmt "Red"
  | Green -> fprintf fmt "Green"
  | Blue -> fprintf fmt "Blue"

let eq_colors = Alcotest.of_pp pp_colors

let gen = Gen.oneofl [Red; Green; Blue]

let test_variants () =
  test_compare ~msg:"Gen.oneofl <=> deriving variants" ~eq:eq_colors gen gen_colors

type poly_colors = [`Red | `Green | `Blue] [@@deriving qcheck]

let pp_poly_colors fmt x =
  let open Format in
  match x with
  | `Red -> fprintf fmt "`Red"
  | `Green -> fprintf fmt "`Green"
  | `Blue -> fprintf fmt "`Blue"

let eq_poly_colors = Alcotest.of_pp pp_poly_colors

let gen_poly : poly_colors Gen.t = Gen.oneofl [`Red; `Green; `Blue]

let test_poly_variants () =
  test_compare ~msg:"Gen.oneofl <=> deriving variants"
    ~eq:eq_poly_colors gen_poly gen_poly_colors

(** {2. Tests weight} *)

type letters =
  | A [@weight 0]
  | B
[@@deriving qcheck]

let test_weight =
  Test.make ~name:"gen_letters always produces B"
    (make gen_letters)
    (function
     | A -> false
     | B -> true)
  |>
    QCheck_alcotest.to_alcotest

type poly_letters = [
    | `A [@weight 0]
    | `B
  ]
[@@deriving qcheck]

let test_weight_poly =
  Test.make ~name:"gen_poly_letters always produces B"
    (make gen_poly_letters)
    (function
     | `A -> false
     | `B -> true)
  |>
    QCheck_alcotest.to_alcotest

(** {2. Freq distribution with weights} *)

type languages =
  | English [@weight 100]
  | Spanish [@weight 10]
  | French [@weight 1]
[@@deriving qcheck]

let test_distrib_freq () =
  let count_en = ref 0 and count_es = ref 0 and count_fr = ref 0 in
  let l = generate ~n:1000 gen_languages in
  List.iter (function
      | English -> incr count_en
      | Spanish -> incr count_es
      | French -> incr count_fr) l;
  Alcotest.(check (triple int int int)) "test freq distribution"
    (!count_en, !count_es, !count_fr) (900, 88, 12)

type poly_languages =
  [ `English [@weight 100]
  | `Spanish [@weight 10]
  | `French [@weight 1]
  ]
[@@deriving qcheck]

let test_poly_distrib_freq () =
  let count_en = ref 0 and count_es = ref 0 and count_fr = ref 0 in
  let l = generate ~n:1000 gen_poly_languages in
  List.iter (function
      | `English -> incr count_en
      | `Spanish -> incr count_es
      | `French -> incr count_fr) l;
  Alcotest.(check (triple int int int)) "test freq distribution"
    (!count_en, !count_es, !count_fr) (900, 88, 12)

(** {2. Execute tests} *)

let () = Alcotest.run "Test_Variant"
           [("Variants",
             Alcotest.[
                 test_case "test_variants" `Quick test_variants;
                 test_case "test_poly_variants" `Quick test_poly_variants;
                 test_case "test_poly_distrib_freq" `Quick test_distrib_freq;
                 test_case "test_distrib_freq" `Quick test_poly_distrib_freq;
                 test_weight;
                 test_weight_poly
           ])]
