open OUnit2
open Regex
open Regex.Simplify

let assert_equal = assert_equal ~cmp:equal ~printer:show

let test_simplify_one _ =
  assert_equal (parse "a") (simplify (parse "a|a"));
  assert_equal (parse "a") (simplify (parse "aε"));
  assert_equal (parse "a") (simplify (parse "εa"));
  assert_equal (parse "a*") (simplify (parse "(a*)*"));
  assert_equal (parse "a*") (simplify (parse "a*a*"));
  assert_equal (parse "a") (simplify (parse "∅|a"));
  assert_equal (parse "a") (simplify (parse "a|∅"));
  assert_equal (parse "∅") (simplify (parse "a∅"));
  assert_equal (parse "∅") (simplify (parse "∅a"));
  assert_equal (parse "ε") (simplify (parse "∅*"));
  assert_equal (parse "ε") (simplify (parse "ε*"))

let test_simplify_none _ =
  assert_equal (parse "ε") (simplify (parse "ε"));
  assert_equal (parse "∅") (simplify (parse "∅"));
  assert_equal (parse "b") (simplify (parse "b"));
  assert_equal (parse "a|b") (simplify (parse "a|b"));
  assert_equal (parse "ab") (simplify (parse "ab"));
  assert_equal (parse "a*") (simplify (parse "a*"))

let test_simplify_combs _ =
  assert_equal (parse "a*") (simplify (parse "(aε)*"));
  assert_equal (parse "ab") (simplify (parse "(a|∅)(εb)"));
  assert_equal (parse "a|b") (simplify (parse "(∅|a)|bε"))

let test_simplify_cascade _ =
  assert_equal (parse "a") (simplify (parse "ε(aε)"));
  assert_equal (parse "a") (simplify (parse "∅|(a|∅)"));
  assert_equal (parse "ε") (simplify (parse "(a∅)*"));
  assert_equal (parse "ε") (simplify (parse "(∅*)*"))

let test_simplify'_one _ =
  assert_equal (parse "a") (simplify' (parse "a|a"));
  assert_equal (parse "a") (simplify' (parse "aε"));
  assert_equal (parse "a") (simplify' (parse "εa"));
  assert_equal (parse "a*") (simplify' (parse "(a*)*"));
  assert_equal (parse "a*") (simplify' (parse "a*a*"));
  assert_equal (parse "a") (simplify' (parse "∅|a"));
  assert_equal (parse "a") (simplify' (parse "a|∅"));
  assert_equal (parse "∅") (simplify' (parse "a∅"));
  assert_equal (parse "∅") (simplify' (parse "∅a"));
  assert_equal (parse "ε") (simplify' (parse "∅*"));
  assert_equal (parse "ε") (simplify' (parse "ε*"))

let test_simplify'_none _ =
  assert_equal (parse "ε") (simplify' (parse "ε"));
  assert_equal (parse "∅") (simplify' (parse "∅"));
  assert_equal (parse "b") (simplify' (parse "b"));
  assert_equal (parse "a|b") (simplify' (parse "a|b"));
  assert_equal (parse "ab") (simplify' (parse "ab"));
  assert_equal (parse "a*") (simplify' (parse "a*"))

let test_simplify'_combs _ =
  assert_equal (parse "a*") (simplify' (parse "(aε)*"));
  assert_equal (parse "ab") (simplify' (parse "(a|∅)(εb)"));
  assert_equal (parse "a|b") (simplify' (parse "(∅|a)|bε"))

let test_simplify'_cascade _ =
  assert_equal (parse "a") (simplify' (parse "ε(aε)"));
  assert_equal (parse "a") (simplify' (parse "∅|(a|∅)"));
  assert_equal (parse "ε") (simplify' (parse "(a∅)*"));
  assert_equal (parse "ε") (simplify' (parse "(∅*)*"))


(** Regulaaravaldise puude generaator omaduspõhiseks testimiseks. *)
let arbitrary_re =
  let gen = QCheck.Gen.(sized @@ fix (fun self n ->
    match n with
    | 0 -> oneof [return Empty; return Eps; map (fun c -> Char c) printable]
    | n ->
      frequency [
        (1, self 0);
        (1, map2 (fun l r -> Choice (l, r)) (self (n / 2)) (self (n / 2)));
        (1, map2 (fun l r -> Concat (l, r)) (self (n / 2)) (self (n / 2)));
        (1, map (fun r -> Star r) (self (n - 1)));
      ]
  ))
  in
  let open QCheck.Iter in
  let rec shrink = function
    | Empty
    | Eps -> empty
    | Char c -> QCheck.Shrink.char_printable c >|= (fun c -> Char c)
    | Choice (l, r) ->
      of_list [l; r]
      <+>
      (shrink l >|= (fun l' -> Choice (l', r)))
      <+>
      (shrink r >|= (fun r' -> Choice (l, r')))
    | Concat (l, r) ->
      of_list [l; r]
      <+>
      (shrink l >|= (fun l' -> Concat (l', r)))
      <+>
      (shrink r >|= (fun r' -> Concat (l, r')))
    | Star r ->
      return r
      <+>
      (shrink r >|= (fun r' -> Star r'))
  in
  QCheck.make gen ~print:show ~shrink

(** Omaduspõhine test, mis kontrollib kahe lihtsustamise samaväärsust. *)
let test_equivalent =
  QCheck.Test.make ~name:"equivalent"
    arbitrary_re
    (fun r ->
      equal (simplify r) (simplify' r)
    )
  |> QCheck_ounit.to_ounit2_test

let tests =
  "simplify" >::: [
    "simplify" >::: [
      "one" >:: test_simplify_one;
      "none" >:: test_simplify_none;
      "combs" >:: test_simplify_combs;
      "cascade" >:: test_simplify_cascade;
    ];
    "simplify'" >::: [
      "one" >:: test_simplify'_one;
      "none" >:: test_simplify'_none;
      "combs" >:: test_simplify'_combs;
      "cascade" >:: test_simplify'_cascade;
    ];
    test_equivalent;
  ]
