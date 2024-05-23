open OUnit2
open Regex
open Regex.Brzozowski

let test_matches_eps_ops _ =
  let assert_equal = assert_equal ~printer:string_of_bool in
  assert_equal false (matches_eps (parse "∅"));
  assert_equal true (matches_eps (parse "ε"));
  assert_equal false (matches_eps (parse "a"));
  assert_equal true (matches_eps (parse "ε|a"));
  assert_equal false (matches_eps (parse "εa"));
  assert_equal true (matches_eps (parse "a*"))

let test_matches_eps_combs _ =
  let assert_equal = assert_equal ~printer:string_of_bool in
  assert_equal true (matches_eps (parse "a*|ε"));
  assert_equal false (matches_eps (parse "aε*|a"));
  assert_equal true (matches_eps (parse "(a|(b|ε))*"));
  assert_equal true (matches_eps (parse "εεε"))

let test_derive _ =
  let assert_equal = assert_equal ~cmp:equal ~printer:show in
  assert_equal (parse "∅") (derive (parse "∅") 'a');
  assert_equal (parse "∅") (derive (parse "ε") 'a');
  assert_equal (parse "ε") (derive (parse "a") 'a');
  assert_equal (parse "∅") (derive (parse "a") 'b');
  assert_equal (parse "ε|∅") (derive (parse "a|b") 'a');
  assert_equal (parse "εb") (derive (parse "ab") 'a');
  assert_equal (parse "∅b") (derive (parse "ab") 'b');
  assert_equal (parse "∅a|ε") (derive (parse "εa") 'a');
  assert_equal (parse "εa*") (derive (parse "a*") 'a');
  assert_equal (parse "(εa*)b|∅") (derive (parse "a*b") 'a');
  assert_equal (parse "(∅a*)b|ε") (derive (parse "a*b") 'b');
  assert_equal (parse "(εa*)a|ε") (derive (parse "a*a") 'a')


(** Teisendab regulaaravaldise puu OCaml-i regulaaravaldiseks (sõne kujul). *)
let rec regexp_str_of_t = function
  | Empty -> invalid_arg "regexp_of_t: Empty"
  | Eps -> ""
  | Char c -> Format.sprintf "%c" c
  | Choice (l, r) -> Format.sprintf {|\(%s\|%s\)|} (regexp_str_of_t l) (regexp_str_of_t r)
  | Concat (l, r) -> Format.sprintf "%s%s" (regexp_str_of_t l) (regexp_str_of_t r)
  | Star r -> Format.sprintf {|\(%s\)*|} (regexp_str_of_t r)

(** Kas sõne sobitub tervikuna OCaml-i regulaaravaldisega?
    Vaja, sest ^$ käituvad \n-ga spetsiaalselt. *)
let str_string_full_match regexp s =
  Str.string_match regexp s 0 && Str.match_end () = String.length s

(** Omaduspõhine test, mis kontrollib meie ja OCaml-i regulaaravaldiste sobitumise samaväärsust. *)
let qcheck_matches r_str =
  let r = parse r_str in
  let regexp_str = regexp_str_of_t r in
  let regexp = Str.regexp regexp_str in
  QCheck.Test.make ~name:r_str
    QCheck.string_small_printable
    (fun s ->
      matches s r = str_string_full_match regexp s
    )
  |> QCheck_ounit.to_ounit2_test

let matches_concat_tests = List.map qcheck_matches [
    "abcde";
    "abe";
  ]

let matches_choice_tests = List.map qcheck_matches [
    "a|bc";
    "a|bc|d";
  ]

let matches_star_tests = List.map qcheck_matches [
    "a*";
    "a*b*";
    "a*b";
    "a*|b";
  ]

let matches_combs_tests = List.map qcheck_matches [
    "(ab)*";
    "(a|b)*";
    "(ab|cd)*";
    "(ab)*|cd";
    "(ab)*|(cd)*";
  ]

let matches_eps_tests = List.map qcheck_matches [
    "ε";
    "ε*";
    "aε";
    "εa";
    "ε|b";
    "εa|b";
    "(aε)*";
  ]

let matches_madness_tests = List.map qcheck_matches [
    "(a|b)*b(a|b)";
    "((a|b)*b(a|b))*|ab|b*";
    "(a|b)*b(a|b)(x|bgg)*g(aεd)*fa(ga|ε)*";
    "(aε)*|a*|b*";
    "((ε*)*(a*)*)*";
    "(((ε|b)*b(ε|ε|ε*|ε|ε))*|ab|b*)*";
  ]

let tests =
  "brzozowski" >::: [
    "matches_eps" >::: [
      "ops" >:: test_matches_eps_ops;
      "combs" >:: test_matches_eps_combs;
    ];
    "derive" >:: test_derive;
    "matches" >::: [
      "concat" >::: matches_concat_tests;
      "choice" >::: matches_choice_tests;
      "star" >::: matches_star_tests;
      "combs" >::: matches_combs_tests;
      "eps" >::: matches_eps_tests;
      "madness" >::: matches_madness_tests;
    ];
  ]
