open OUnit2
open Regex

let assert_equal = assert_equal ~printer:show

let test_atom _ =
  assert_equal Empty (parse "∅");
  assert_equal Eps (parse "ε");
  assert_equal (Char 'a') (parse "a");
  assert_equal (Char 'a') (parse "(a)");
  assert_equal (Char 'a') (parse "((a))")

let test_star _ =
  assert_equal (Star (Char 'a')) (parse "a*");
  OUnitTodo.assert_raises Parsing.Parse_error (fun () -> parse "a**");
  assert_equal (Star (Star (Char 'a'))) (parse "(a*)*")

let test_assoc _ =
  assert_equal (Choice (Choice (Char 'a', Char 'b'), Char 'c')) (parse "a|b|c");
  assert_equal (Choice (Choice (Char 'a', Char 'b'), Char 'c')) (parse "(a|b)|c");
  assert_equal (Choice (Char 'a', Choice (Char 'b', Char 'c'))) (parse "a|(b|c)");

  assert_equal (Concat (Concat (Char 'a', Char 'b'), Char 'c')) (parse "abc");
  assert_equal (Concat (Concat (Char 'a', Char 'b'), Char 'c')) (parse "(ab)c");
  assert_equal (Concat (Char 'a', Concat (Char 'b', Char 'c'))) (parse "a(bc)")

let test_prio _ =
  assert_equal (Choice (Char 'a', Concat (Char 'b', Char 'c'))) (parse "a|bc");
  assert_equal (Choice (Concat (Char 'a', Char 'b'), Char 'c')) (parse "ab|c");

  assert_equal (Concat (Char 'a', Star (Char 'b'))) (parse "ab*");
  assert_equal (Concat (Char 'a', Star (Char 'b'))) (parse "a(b*)");
  assert_equal (Star (Concat (Char 'a', Char 'b'))) (parse "(ab)*");
  assert_equal (Concat (Star (Char 'a'), Char 'b')) (parse "a*b")

let test_eps _ =
  assert_equal Eps (parse "");
  assert_equal Eps (parse "()");
  assert_equal (Choice (Char 'a', Eps)) (parse "a|");
  assert_equal (Choice (Eps, Char 'a')) (parse "|a");
  assert_equal (Choice (Eps, Eps)) (parse "|");
  OUnitTodo.assert_raises Parsing.Parse_error (fun () -> parse "*")

let tests =
  "parse" >::: [
    "atom" >:: test_atom;
    "star" >:: test_star;
    "assoc" >:: test_assoc;
    "prio" >:: test_prio;
    "eps" >:: test_eps;
  ]
