open Import

module Conv = struct
  type ('repr, 'true_type) t = {
    from_repr : 'repr -> ('true_type, Rresult.R.msg) result;
    to_repr : 'true_type -> 'repr;
    equal : ('true_type -> 'true_type -> bool) option;
    pp : 'true_type Fmt.t option;
  }

  let make ~from_repr ~to_repr ?equal ?pp () = { from_repr; to_repr; equal; pp }
end

type _ t =
  | Sbool : bool t
  | Sstring : string t
  | Slist : 'a t -> 'a list t
  | Spair : 'a t * 'b t -> ('a * 'b) t
  | Choice2 : 'a t * 'b t -> [ `C1 of 'a | `C2 of 'b ] t
  | Choice3 : 'a t * 'b t * 'c t -> [ `C1 of 'a | `C2 of 'b | `C3 of 'c ] t
  | Conv : ('repr, 'true_type) Conv.t * 'repr t -> 'true_type t

let bool = Sbool
let string = Sstring
let list s = Slist s
let pair s s' = Spair (s, s')
let choice2 s s' = Choice2 (s, s')
let choice3 s s' s'' = Choice3 (s, s', s'')
let conv conv s = Conv (conv, s)

let rec shallow_description : type a. a t -> string = function
  | Slist _ -> "a list"
  | Spair _ -> "a pair"
  | Sstring -> "a string"
  | Sbool -> "a boolean"
  | Choice2 (s, s') ->
      let d = shallow_description s in
      let d' = shallow_description s' in
      Printf.sprintf "%s or %s" d d'
  | Choice3 (s, s', s'') ->
      let d = shallow_description s in
      let d' = shallow_description s' in
      let d'' = shallow_description s'' in
      Printf.sprintf "%s, %s or %s" d d' d''
  | Conv (_, s) -> shallow_description s

let rec from_opam_val :
    type a. a t -> OpamParserTypes.FullPos.value -> (a, Rresult.R.msg) result =
 fun shape value ->
  let open Result.O in
  let parse_error () =
    let expected = shallow_description shape in
    Opam.Pos.unexpected_value_error ~expected value
  in
  match (shape, value) with
  | Sbool, { pelem = Bool b; _ } -> Ok b
  | Sstring, { pelem = String s; _ } -> Ok s
  | Spair (s, s'), { pelem = List { pelem = [ v; v' ]; _ }; _ } ->
      let* fst = from_opam_val s v in
      let* snd = from_opam_val s' v' in
      Ok (fst, snd)
  | Slist s, { pelem = List _; _ } ->
      let* l = Opam.Value.List.from_value (from_opam_val s) value in
      Ok l
  | Conv (conv, s), value ->
      let* repr = from_opam_val s value in
      conv.from_repr repr
  | Choice2 (s, s'), value -> (
      match from_opam_val s value with
      | Ok c1 -> Ok (`C1 c1)
      | Error _ -> (
          match from_opam_val s' value with
          | Ok c2 -> Ok (`C2 c2)
          | Error _ -> parse_error ()))
  | Choice3 (s, s', s''), value -> (
      match from_opam_val s value with
      | Ok c1 -> Ok (`C1 c1)
      | Error _ -> (
          match from_opam_val s' value with
          | Ok c2 -> Ok (`C2 c2)
          | Error _ -> (
              match from_opam_val s'' value with
              | Ok c3 -> Ok (`C3 c3)
              | Error _ -> parse_error ())))
  | _ -> parse_error ()

let rec to_opam_val : type a. a t -> a -> OpamParserTypes.FullPos.value =
 fun shape value ->
  match (shape, value) with
  | Sbool, b -> Opam.Pos.with_default (OpamParserTypes.FullPos.Bool b)
  | Sstring, s -> Opam.Value.String.to_value s
  | Spair (s, s'), (v, v') ->
      let fst = to_opam_val s v in
      let snd = to_opam_val s' v' in
      Opam.Value.List.to_value Fun.id [ fst; snd ]
  | Slist s, l -> Opam.Value.List.to_value (to_opam_val s) l
  | Conv (conv, s), value ->
      let repr = conv.to_repr value in
      to_opam_val s repr
  | Choice2 (s, _), `C1 v -> to_opam_val s v
  | Choice2 (_, s), `C2 v -> to_opam_val s v
  | Choice3 (s, _, _), `C1 v -> to_opam_val s v
  | Choice3 (_, s, _), `C2 v -> to_opam_val s v
  | Choice3 (_, _, s), `C3 v -> to_opam_val s v

let unmatched_list_delimiter ~delim =
  Rresult.R.error_msgf "unmatched list delimiter '%c'" delim

let str_get_opt s i =
  match s.[i] with c -> Some c | exception Invalid_argument _ -> None

let split_list ~start ~len s =
  let last = start + len - 1 in
  let rec search_sep ~count_open ~acc ~current_elm_start i =
    match str_get_opt s i with
    | _ when i > last && count_open > 0 -> unmatched_list_delimiter ~delim:'['
    | _ when i > last ->
        let last =
          String.sub s ~pos:current_elm_start ~len:(last + 1 - current_elm_start)
        in
        Ok (List.rev (last :: acc))
    | Some '[' ->
        search_sep ~count_open:(count_open + 1) ~acc ~current_elm_start (i + 1)
    | Some (']' as delim) when count_open = 0 -> unmatched_list_delimiter ~delim
    | Some ']' ->
        search_sep ~count_open:(count_open - 1) ~acc ~current_elm_start (i + 1)
    | Some ',' when count_open = 0 ->
        let elm =
          String.sub s ~pos:current_elm_start ~len:(i - current_elm_start)
        in
        search_sep ~count_open ~acc:(elm :: acc) ~current_elm_start:(i + 1)
          (i + 1)
    | Some _ -> search_sep ~count_open ~acc ~current_elm_start (i + 1)
    | None -> assert false
  in
  if len = 0 then Ok []
  else search_sep ~count_open:0 ~acc:[] ~current_elm_start:start start

let parse_list s =
  let len = String.length s in
  if len = 0 then Ok []
  else
    match (s.[0], s.[len - 1]) with
    | '[', ']' -> split_list ~start:1 ~len:(len - 2) s
    | ('[' as delim), _ -> unmatched_list_delimiter ~delim
    | _ -> split_list ~start:0 ~len s

let rec cmdliner_parse : type a. a t -> string -> (a, Rresult.R.msg) result =
 fun shape value ->
  let open Result.O in
  let parse_error () =
    let expected = shallow_description shape in
    Rresult.R.error_msgf "Expected %s but got: %s" expected value
  in
  match (shape, value) with
  | Sbool, "true" -> Ok true
  | Sbool, "false" -> Ok false
  | Sstring, s -> Ok s
  | Spair (s, s'), value -> (
      let* l = parse_list value in
      match l with
      | [ v; v' ] ->
          let* fst = cmdliner_parse s v in
          let* snd = cmdliner_parse s' v' in
          Ok (fst, snd)
      | _ -> parse_error ())
  | Slist s, value ->
      let* l = parse_list value in
      Result.List.map l ~f:(cmdliner_parse s)
  | Conv (conv, s), value ->
      let* repr = cmdliner_parse s value in
      conv.from_repr repr
  | Choice2 (s, s'), value -> (
      match cmdliner_parse s value with
      | Ok c1 -> Ok (`C1 c1)
      | Error _ -> (
          match cmdliner_parse s' value with
          | Ok c2 -> Ok (`C2 c2)
          | Error _ -> parse_error ()))
  | Choice3 (s, s', s''), value -> (
      match cmdliner_parse s value with
      | Ok c1 -> Ok (`C1 c1)
      | Error _ -> (
          match cmdliner_parse s' value with
          | Ok c2 -> Ok (`C2 c2)
          | Error _ -> (
              match cmdliner_parse s'' value with
              | Ok c3 -> Ok (`C3 c3)
              | Error _ -> parse_error ())))
  | _ -> parse_error ()

let rec cmdliner_print : type a. a t -> Format.formatter -> a -> unit =
 fun shape fmt value ->
  match (shape, value) with
  | Sbool, b -> Fmt.pf fmt "%a" Fmt.bool b
  | Sstring, s -> Fmt.pf fmt "%s" s
  | Spair (s, s'), (v, v') ->
      Fmt.pf fmt "[%a,%a]" (cmdliner_print s) v (cmdliner_print s') v'
  | Slist s, l ->
      Fmt.pf fmt "[%a]"
        (Fmt.list ~sep:Fmt.(const char ',') (cmdliner_print s))
        l
  | Conv (conv, s), value ->
      let repr = conv.to_repr value in
      cmdliner_print s fmt repr
  | Choice2 (s, _), `C1 v -> cmdliner_print s fmt v
  | Choice2 (_, s), `C2 v -> cmdliner_print s fmt v
  | Choice3 (s, _, _), `C1 v -> cmdliner_print s fmt v
  | Choice3 (_, s, _), `C2 v -> cmdliner_print s fmt v
  | Choice3 (_, _, s), `C3 v -> cmdliner_print s fmt v

let cmdliner_conv shape =
  let parse = cmdliner_parse shape in
  let print = cmdliner_print shape in
  Cmdliner.Arg.conv (parse, print)

let rec equal : type a. a t -> a -> a -> bool =
 fun shape v v' ->
  match (shape, v, v') with
  | Sbool, b, b' -> Bool.equal b b'
  | Sstring, s, s' -> String.equal s s'
  | Spair (sfst, ssnd), (fst, snd), (fst', snd') ->
      equal sfst fst fst' && equal ssnd snd snd'
  | Slist s, l, l' -> List.equal (equal s) l l'
  | Choice2 (s, _), `C1 v, `C1 v' -> equal s v v'
  | Choice2 (_, s), `C2 v, `C2 v' -> equal s v v'
  | Choice2 _, _, _ -> false
  | Choice3 (s, _, _), `C1 v, `C1 v' -> equal s v v'
  | Choice3 (_, s, _), `C2 v, `C2 v' -> equal s v v'
  | Choice3 (_, _, s), `C3 v, `C3 v' -> equal s v v'
  | Choice3 _, _, _ -> false
  | Conv ({ equal = Some eq; _ }, _), v, v' -> eq v v'
  | Conv (conv, s), v, v' ->
      let repr = conv.to_repr v in
      let repr' = conv.to_repr v' in
      equal s repr repr'

let rec pp : type a. a t -> a Fmt.t =
 fun shape fmt v ->
  match (shape, v) with
  | Sbool, b -> Fmt.bool fmt b
  | Sstring, s -> Fmt.pf fmt "%S" s
  | Spair (s, s'), (v, v') -> Fmt.pf fmt "(%a, %a)" (pp s) v (pp s') v'
  | Slist s, l ->
      Fmt.pf fmt "[%a]" (Fmt.list ~sep:Fmt.(const char ';') (pp s)) l
  | Choice2 (s, _), `C1 v -> pp s fmt v
  | Choice2 (_, s), `C2 v -> pp s fmt v
  | Choice3 (s, _, _), `C1 v -> pp s fmt v
  | Choice3 (_, s, _), `C2 v -> pp s fmt v
  | Choice3 (_, _, s), `C3 v -> pp s fmt v
  | Conv ({ pp = Some pp; _ }, _), v -> pp fmt v
  | Conv (conv, s), v ->
      let repr = conv.to_repr v in
      pp s fmt repr
