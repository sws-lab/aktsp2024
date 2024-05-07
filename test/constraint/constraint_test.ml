open OUnit2
open Constraint
open Constraint.Solver

(** Vt. "Compiler Design: Analysis and Transformation" õpikust, näide 1.5.2. *)
module Example =
struct
  module V =
  struct
    type t = X1 | X2 | X3 [@@deriving eq, ord, hash, show]
  end

  open V

  type element = A | B | C [@@deriving ord, show]

  module D = SetDomain.Make (struct
    type t = element [@@deriving ord, show]
  end)

  let vars = [X1; X2; X3]

  let f x get =
    match x with
    | X1 -> D.join (D.singleton A) (get X3)
    | X2 -> D.S.inter (get X3) (D.add A (D.singleton B))
    | X3 -> D.join (get X1) (D.singleton C)


end

module type Solver = functor (Sys: Sys) -> sig
    module VH: Hashtbl.S with type key = Sys.V.t
    val solve: unit -> Sys.D.t VH.t
  end


module Common_test (Solver: Solver) =
struct

  module ExampleSolver = Solver (Example)

  let test_example _ =
    let open Example in
    let sol = ExampleSolver.solve () in
    let assert_equal = assert_equal ~cmp:D.equal ~printer:D.show in
    assert_equal (D.add A (D.singleton C)) (ExampleSolver.VH.find sol X1);
    assert_equal (D.singleton A) (ExampleSolver.VH.find sol X2);
    assert_equal (D.add A (D.singleton C)) (ExampleSolver.VH.find sol X3)

  let tests =
    "common" >::: [
      "example" >:: test_example;
    ]
end

module Kleene_test =
struct
  module Common = Common_test (Kleene)
  let tests =
    "kleene" >::: [
      Common.tests;
    ]
end

module RoundRobin_test =
struct
  module Common = Common_test (RoundRobin)
  let tests =
    "roundrobin" >::: [
      Common.tests;
    ]
end



let tests =
  "constraint" >::: [
    "solver" >::: [
      Kleene_test.tests;
      RoundRobin_test.tests;

    ];
  ]

let () = run_test_tt_main (OUnitTodo.wrap tests)
