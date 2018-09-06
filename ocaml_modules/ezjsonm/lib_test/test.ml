(* Check we're compatible with sexplib *)
type test = {
  foo: Ezjsonm.value;
  bar: Ezjsonm.t;
} [@@deriving sexp]

let json_t: Ezjsonm.t Alcotest.testable =
  (module struct
    type t = Ezjsonm.t
    let equal = (=)
    let pp ppf x = Format.pp_print_string ppf (Ezjsonm.to_string x)
  end)

let json_v: Ezjsonm.value Alcotest.testable =
  (module struct
    type t = Ezjsonm.value
    let equal = (=)
    let pp ppf x = Format.pp_print_string ppf Ezjsonm.(to_string @@ wrap x)
  end)

let random_int i =
  if i <= 1 then 0
  else Random.int i

let random_string len =
  let s = Bytes.create (random_int len) in
  for i = 0 to Bytes.length s - 1 do
    Bytes.set s i @@ Char.chr (random_int 127)
  done;
  Bytes.to_string s

let random_list len gen =
  Array.to_list (Array.init len gen)

let random_bool () =
  Random.int 2 = 0

type 'a x = {
  to_json: 'a -> Ezjsonm.value;
  of_json: Ezjsonm.value -> 'a;
  test   : 'a Alcotest.testable;
}

let test x t =
  let j =  x.to_json t in
  let t' = x.of_json j in
  Alcotest.(check x.test) "idempotent JSON conversion" t t';
  let str = Ezjsonm.(to_string (wrap j)) in
  let j' = Ezjsonm.(unwrap (from_string str)) in
  let t' = x.of_json j' in
  Alcotest.(check x.test) "idempotent string conversion" t t'

let string =
  { to_json = Ezjsonm.string;
    of_json = Ezjsonm.get_string;
    test    = Alcotest.string; },
  [""; "foo"; random_string 1024]

let list =
  { to_json = Ezjsonm.(list string);
    of_json = Ezjsonm.(get_list get_string);
    test    = Alcotest.(list string); },
  [
    random_list 30 random_string;
    random_list 10 (fun _ -> "foo")
  ]

let test_stream jsons () =
  let jsons = List.map Ezjsonm.wrap jsons in
  let jsons, last = match jsons with
    | []   -> assert false
    | h::t -> t, h
  in
  let string j = Ezjsonm.to_string j in
  let strings = List.map (fun j -> string j ^ ",") jsons in
  let strings = "[" :: strings @ [string last; "]"] in
  let jsons = jsons @ [last] in
  Lwt_main.run begin
    let open Lwt.Infix in
    let stream = Lwt_stream.of_list strings in
    let stream = Ezjsonm_lwt.from_stream stream in
    Lwt_stream.to_list stream >|= fun json' ->
    Alcotest.(check @@ list json_v) "stream" jsons json'
  end

let tests t ts () = List.iter (test t) ts

let stream0 =
  random_list 42 (fun i -> Ezjsonm.(string @@ random_string i))

let test_sexp_of_t () =
  let t = `A [ `String "hello" ] in
  let open Ezjsonm in
  let t'' = t_of_sexp @@ sexp_of_t t in
  Alcotest.(check json_t) "sexp_of_t" t t''

let test_sexp_of_value () =
  let v = `A [ `String "hello" ] in
  let open Ezjsonm in
  let v'' = value_of_sexp @@ sexp_of_value v in
  Alcotest.(check json_v) "sexp_of_value" v v''

let () =
  let suite k (t, ts) = k, ["test", `Quick, tests t ts] in
  Alcotest.run "ezjsonm" [
    suite "string" string;
    suite "list"   list;
    "stream", [
      "stream0", `Quick, test_stream stream0;
    ];
    "sexp", [
      "sexp_of_t", `Quick, test_sexp_of_t;
      "sexp_of_value", `Quick, test_sexp_of_value;
    ];
  ]
