open OUnit2
open Toylangs.Rnd

let false_coin () = false
let true_coin () = true
let alt_coin start: coin =
  let state = ref (not start) in
  fun () ->
    state := not !state;
    !state


let ex1 = Add (Num 4, Num 6)
let ex2 = Flip (Num 4, Num 6)
let ex3 = Flip (Flip (Num 4, Num 5), Num 6)
let ex4 = Add (Flip (Num 4, Num 5), Num 6)
let ex5 = Flip (Num 4, Flip (Num 7, Num 66))
let ex6 = Flip (Add (Num 4, Num 5), Num 8)
let ex7 = Add (Num 3, Flip (Num 33, Num 9))
let ex8 = Flip (ex5, ex6)
let ex9 = Add (ex2, ex2)
let ex10 = Neg ex2
let ex11 = Add (Flip (Num 10, Num 20), Flip (Num 0, Num 1))


let test_basics _ =
  let assert_equal = assert_equal ~printer:string_of_int in
  assert_equal 5 (eval true_coin (Num 5));
  assert_equal (-5) (eval true_coin (Neg (Num 5)));
  assert_equal 10 (eval true_coin ex1);
  assert_equal 10 (eval false_coin ex1);
  assert_equal 4 (eval true_coin ex2);
  assert_equal 6 (eval false_coin ex2)

let test_single_choice _ =
  let assert_equal = assert_equal ~printer:string_of_int in
  assert_equal 10 (eval true_coin ex4);
  assert_equal 11 (eval false_coin ex4);

  assert_equal (-4) (eval true_coin ex10);
  assert_equal (-6) (eval false_coin ex10);

  assert_equal 36 (eval true_coin ex7);
  assert_equal 12 (eval false_coin ex7)

let test_combined _ =
  let assert_equal = assert_equal ~printer:string_of_int in
  assert_equal 4 (eval true_coin ex3);
  assert_equal 6 (eval false_coin ex3);
  assert_equal 5 (eval (alt_coin true) ex3);
  assert_equal 6 (eval (alt_coin false) ex3);

  assert_equal 4 (eval true_coin ex5);
  assert_equal 66 (eval false_coin ex5);
  assert_equal 4 (eval (alt_coin true) ex5);
  assert_equal 7 (eval (alt_coin false) ex5);

  assert_equal 4 (eval true_coin ex8);
  assert_equal 8 (eval false_coin ex8);
  assert_equal 7 (eval (alt_coin true) ex8);
  assert_equal 9 (eval (alt_coin false) ex8);

  assert_equal 8 (eval true_coin ex9);
  assert_equal 12 (eval false_coin ex9);
  assert_equal 10 (eval (alt_coin true) ex9);
  assert_equal 10 (eval (alt_coin false) ex9)

let test_order _ =
  let assert_equal = assert_equal ~printer:string_of_int in
  assert_equal 11 (eval (alt_coin true) ex11);
  assert_equal 20 (eval (alt_coin false) ex11)

let test_list _ =
  let assert_equal = assert_equal ~printer:[%show: int list] in
  assert_equal [10; 11] (eval_list ex4);
  assert_equal [36; 12] (eval_list ex7);
  assert_equal [10; 11; 36; 12] (eval_list (Flip (ex4, ex7)));
  assert_equal [46; 22; 47; 23] (eval_list (Add (ex4, ex7)))

let show_intset x = [%show: int list] (IntSet.elements x)

let test_set _ =
  let assert_equal = assert_equal ~cmp:IntSet.equal ~printer:show_intset in
  assert_equal (IntSet.of_list [10; 11]) (eval_set ex4);
  assert_equal (IntSet.of_list [12; 36]) (eval_set ex7);
  assert_equal (IntSet.of_list [10; 11; 12; 36]) (eval_set (Flip (ex4, ex7)));
  assert_equal (IntSet.of_list [22; 46; 23; 47]) (eval_set (Add (ex4, ex7)))

let tests =
  "rnd" >::: [
    "eval" >::: [
      "basics" >:: test_basics;
      "single_choice" >:: test_single_choice;
      "combined" >:: test_combined;
      "order" >:: test_order;
    ];
    "eval_list" >:: test_list;
    "eval_set" >:: test_set;
  ]
