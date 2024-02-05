open OUnit2

let wrap = OUnitTest.test_decorate (fun test ->
    fun ctxt ->
      try
        test ctxt
      with
      | Failure "TODO" [@warning "-fragile-literal-pattern"]
      | QCheck.Test.Test_error (_, _, Failure "TODO", _) [@warning "-fragile-literal-pattern"] ->
        todo "unimplemented"
  )

let assert_raises ?msg exn (f: unit -> 'a) =
  match f () with
  | _ ->
    let str = Format.sprintf "expected exception %s, but no exception was raised." (Printexc.to_string exn) in
    begin match msg with
      | None -> assert_failure str
      | Some s -> assert_failure (s ^ "\n" ^ str)
    end
  | exception Failure "TODO" [@warning "-fragile-literal-pattern"] ->
    todo "unimplemented"
  | exception e ->
    assert_equal ?msg ~printer:Printexc.to_string exn e

let passes (test: test_fun) (ctxt: test_ctxt) =
  match test ctxt with
  | () -> true
  | exception _ -> false

let skip_if_fails test ctxt msg =
  skip_if (not (passes test ctxt)) msg
