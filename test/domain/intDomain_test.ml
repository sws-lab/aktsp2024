open OUnit2
open IntDomain

module Set_test =
struct
  let assert_equal = assert_equal ~cmp:Set.equal ~printer:Set.show

  let test_of_int _ =
    assert_equal (Set.singleton 5) (Set.of_int 5)

  let test_of_interval _ =
    assert_equal (Set.S.add 4 (Set.S.add 3 (Set.S.singleton 2))) (Set.of_interval (2, 4))

  let test_eval_binary_add _ =
    assert_equal (Set.of_interval (3, 6)) (Set.eval_binary (Set.of_interval (0, 2)) Add (Set.of_interval (3, 4)))

  let test_exclude _ =
    assert_equal (Set.of_interval (3, 5)) (Set.exclude 6 (Set.of_interval (3, 6)));
    assert_equal (Set.of_interval (4, 6)) (Set.exclude 3 (Set.of_interval (3, 6)));
    assert_equal (Set.S.remove 4 (Set.of_interval (3, 6))) (Set.exclude 4 (Set.of_interval (3, 6)));
    assert_equal Set.bot (Set.exclude 4 (Set.singleton 4));
    assert_equal Set.bot (Set.exclude 4 Set.bot)

  let tests =
    "set" >::: [
      "of_int" >:: test_of_int;
      "of_interval" >:: test_of_interval;
      "eval_binary" >::: [
        "add" >:: test_eval_binary_add;
      ];
      "exclude" >:: test_exclude;
    ]
end

module Flat_test =
struct
  let assert_equal = assert_equal ~cmp:Flat.equal ~printer:Flat.show

  let test_join _ =
    assert_equal (Lift 5) (Flat.join (Lift 5) (Lift 5));
    assert_equal Top (Flat.join (Lift 5) (Lift 6))

  let test_of_int _ =
    assert_equal (Lift 5) (Flat.of_int 5)

  let test_of_interval _ =
    assert_equal (Lift 5) (Flat.of_interval (5, 5));
    assert_equal Top (Flat.of_interval (2, 4))

  let test_eval_binary_add _ =
    assert_equal (Flat.of_int 5) (Flat.eval_binary (Flat.of_int 2) Add (Flat.of_int 3));
    assert_equal Top (Flat.eval_binary (Flat.of_int 2) Add Top);
    assert_equal Top (Flat.eval_binary Top Add (Flat.of_int 2));
    assert_equal Bot (Flat.eval_binary (Flat.of_int 2) Add Bot);
    assert_equal Bot (Flat.eval_binary Bot Add (Flat.of_int 2))

  let test_exclude _ =
    assert_equal (Flat.of_int 5) (Flat.exclude 4 (Flat.of_int 5));
    assert_equal Bot (Flat.exclude 4 (Flat.of_int 4));
    assert_equal Top (Flat.exclude 4 Top);
    assert_equal Bot (Flat.exclude 4 Bot)

  let tests =
    "flat" >::: [
      "join" >:: test_join;
      "of_int" >:: test_of_int;
      "of_interval" >:: test_of_interval;
      "eval_binary" >::: [
        "add" >:: test_eval_binary_add;
      ];
      "exclude" >:: test_exclude;
    ]
end

module Interval_test =
struct
  let test_leq _ =
    let assert_equal = assert_equal ~printer:string_of_bool in
    assert_equal true (Interval.leq (Lift (5, 5)) (Lift (5, 5)));
    assert_equal true (Interval.leq (Lift (5, 5)) (Lift (2, 10)));
    assert_equal true (Interval.leq (Lift (3, 8)) (Lift (2, 10)));
    assert_equal false (Interval.leq (Lift (5, 5)) (Lift (0, 2)));
    assert_equal false (Interval.leq (Lift (0, 3)) (Lift (0, 2)));
    assert_equal false (Interval.leq (Lift (-1, 3)) (Lift (0, 2)));
    assert_equal false (Interval.leq (Lift (1, 3)) (Lift (0, 2)));
    assert_equal false (Interval.leq (Lift (-1, 1)) (Lift (0, 2)))

  let assert_equal = assert_equal ~cmp:Interval.equal ~printer:Interval.show

  let test_join _ =
    assert_equal (Lift (5, 5)) (Interval.join (Lift (5, 5)) (Lift (5, 5)));
    assert_equal (Lift (5, 6)) (Interval.join (Lift (5, 5)) (Lift (6, 6)));
    assert_equal (Lift (5, 7)) (Interval.join (Lift (5, 5)) (Lift (7, 7)));
    assert_equal (Lift (0, 4)) (Interval.join (Lift (0, 2)) (Lift (1, 4)));
    assert_equal (Lift (1, 4)) (Interval.join (Lift (2, 3)) (Lift (1, 4)))

  let test_of_int _ =
    assert_equal (Lift (5, 5)) (Interval.of_int 5)

  let test_of_interval _ =
    assert_equal (Lift (2, 4)) (Interval.of_interval (2, 4))

  let test_eval_binary_add _ =
    assert_equal (Interval.of_int 5) (Interval.eval_binary (Interval.of_int 2) Add (Interval.of_int 3));
    assert_equal (Interval.of_interval (3, 6)) (Interval.eval_binary (Interval.of_interval (0, 2)) Add (Interval.of_interval (3, 4)))

  let test_eval_binary_sub _ =
    assert_equal (Interval.of_int 2) (Interval.eval_binary (Interval.of_int 5) Sub (Interval.of_int 3));
    assert_equal (Interval.of_interval (2, 7)) (Interval.eval_binary (Interval.of_interval (5, 10)) Sub (Interval.of_int 3));
    assert_equal (Interval.of_interval (4, 7)) (Interval.eval_binary (Interval.of_int 10) Sub (Interval.of_interval (3, 6)));
    assert_equal (Interval.of_interval (-1, 7)) (Interval.eval_binary (Interval.of_interval (5, 10)) Sub (Interval.of_interval (3, 6)))

  let test_eval_binary_mul _ =
    assert_equal (Interval.of_int 6) (Interval.eval_binary (Interval.of_int 2) Mul (Interval.of_int 3));

    assert_equal (Interval.of_interval (8, 14)) (Interval.eval_binary (Interval.of_int 2) Mul (Interval.of_interval (4, 7)));
    assert_equal (Interval.of_interval (4, 21)) (Interval.eval_binary (Interval.of_interval (1, 3)) Mul (Interval.of_interval (4, 7)));

    assert_equal (Interval.of_interval (-14, -8)) (Interval.eval_binary (Interval.of_int (-2)) Mul (Interval.of_interval (4, 7)));
    assert_equal (Interval.of_interval (-14, -8)) (Interval.eval_binary (Interval.of_int 2) Mul (Interval.of_interval (-7, -4)));
    assert_equal (Interval.of_interval (-8, 14)) (Interval.eval_binary (Interval.of_int 2) Mul (Interval.of_interval (-4, 7)));
    assert_equal (Interval.of_interval (8, 14)) (Interval.eval_binary (Interval.of_int (-2)) Mul (Interval.of_interval (-7, -4)));
    assert_equal (Interval.of_interval (-14, 8)) (Interval.eval_binary (Interval.of_int (-2)) Mul (Interval.of_interval (-4, 7)));

    assert_equal (Interval.of_interval (-21, -4)) (Interval.eval_binary (Interval.of_interval (1, 3)) Mul (Interval.of_interval (-7, -4)));
    assert_equal (Interval.of_interval (-12, 21)) (Interval.eval_binary (Interval.of_interval (1, 3)) Mul (Interval.of_interval (-4, 7)));
    assert_equal (Interval.of_interval (4, 21)) (Interval.eval_binary (Interval.of_interval (-3, -1)) Mul (Interval.of_interval (-7, -4)));
    assert_equal (Interval.of_interval (-21, 12)) (Interval.eval_binary (Interval.of_interval (-3, -1)) Mul (Interval.of_interval (-4, 7)));
    assert_equal (Interval.of_interval (-21, 7)) (Interval.eval_binary (Interval.of_interval (-1, 3)) Mul (Interval.of_interval (-7, -4)));
    assert_equal (Interval.of_interval (-12, 21)) (Interval.eval_binary (Interval.of_interval (-1, 3)) Mul (Interval.of_interval (-4, 7)));

    assert_equal (Interval.of_interval (-10, 20)) (Interval.eval_binary (Interval.of_interval (-5, 1)) Mul (Interval.of_interval (-4, 2)));
    assert_equal (Interval.of_interval (-21, -4)) (Interval.eval_binary (Interval.of_interval (-7, -4)) Mul (Interval.of_interval (1, 3)))

  let test_eval_binary_comparison _ =
    (* Kontrollime ainult, et oleks implementeeritud, aga mitte kui täpselt. *)
    ignore (Interval.eval_binary (Interval.of_int 2) Eq (Interval.of_int 3));
    ignore (Interval.eval_binary (Interval.of_int 2) Ne (Interval.of_int 3))



  let test_exclude _ =
    assert_equal (Interval.of_interval (3, 5)) (Interval.exclude 6 (Interval.of_interval (3, 6)));
    assert_equal (Interval.of_interval (4, 6)) (Interval.exclude 3 (Interval.of_interval (3, 6)));
    assert_equal (Interval.of_interval (3, 6)) (Interval.exclude 4 (Interval.of_interval (3, 6)));
    assert_equal (Interval.of_interval (3, 6)) (Interval.exclude 2 (Interval.of_interval (3, 6)));
    assert_equal (Interval.of_interval (3, 6)) (Interval.exclude 7 (Interval.of_interval (3, 6)));
    assert_equal Bot (Interval.exclude 4 (Interval.of_int 4));
    assert_equal Bot (Interval.exclude 4 Bot)

  let tests =
    "interval" >::: [
      "leq" >:: test_leq;
      "join" >:: test_join;
      "of_int" >:: test_of_int;
      "of_interval" >:: test_of_interval;
      "eval_binary" >::: [
        "add" >:: test_eval_binary_add;
        "sub" >:: test_eval_binary_sub;
        "mul" >:: test_eval_binary_mul;
        "comparison" >:: test_eval_binary_comparison;

      ];
      "exclude" >:: test_exclude;
    ]
end

let tests =
  "intDomain" >::: [
    Set_test.tests;
    Flat_test.tests;
    Interval_test.tests;
  ]
