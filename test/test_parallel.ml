let test_parallel =
  let make_test ~name ~input ~f ~expected () =
    let test_name = Printf.sprintf "map: %s" name in
    let test_fun () =
      let actual = Duniverse_lib.Parallel.map ~f input in
      Alcotest.(check (slist int ( - ))) test_name expected actual
    in
    (test_name, `Quick, test_fun)
  in
  let id x = x in
  let wait_and_sum x =
    let rng = Random.float 0.5 in
    Thread.delay rng;
    x + 1
  in
  let input = List.init 50 id in
  let input_plus_1 = List.map (fun x -> x + 1) input in
  [ make_test ~name:"Identity" ~input ~f:id ~expected:input ();
    make_test ~name:"Sleep and add 1" ~input ~f:wait_and_sum ~expected:input_plus_1 ()
  ]

let suite = ("Parallel", test_parallel)
