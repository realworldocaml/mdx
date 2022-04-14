module String = Stdlib.String
include StringLabels

let extract_words s ~is_word_char =
  let rec skip_blanks i =
    if i = length s then []
    else if is_word_char s.[i] then parse_word i (i + 1)
    else skip_blanks (i + 1)
  and parse_word i j =
    if j = length s then [ sub s ~pos:i ~len:(j - i) ]
    else if is_word_char s.[j] then parse_word i (j + 1)
    else sub s ~pos:i ~len:(j - i) :: skip_blanks (j + 1)
  in
  skip_blanks 0

let extract_blank_separated_words s =
  extract_words s ~is_word_char:(function ' ' | '\t' -> false | _ -> true)

let rec check_prefix s ~prefix len i =
  i = len || (s.[i] = prefix.[i] && check_prefix s ~prefix len (i + 1))

let rec check_suffix s ~suffix suffix_len offset i =
  i = suffix_len
  || s.[offset + i] = suffix.[i]
     && check_suffix s ~suffix suffix_len offset (i + 1)

let is_prefix s ~prefix =
  let len = length s in
  let prefix_len = length prefix in
  len >= prefix_len && check_prefix s ~prefix prefix_len 0

let is_suffix s ~suffix =
  let len = length s in
  let suffix_len = length suffix in
  len >= suffix_len && check_suffix s ~suffix suffix_len (len - suffix_len) 0

let drop_prefix s ~prefix =
  if is_prefix s ~prefix then
    if length s = length prefix then Some ""
    else Some (sub s ~pos:(length prefix) ~len:(length s - length prefix))
  else None

let drop_suffix s ~suffix =
  if is_suffix s ~suffix then
    if length s = length suffix then Some s
    else Some (sub s ~pos:0 ~len:(length s - length suffix))
  else None

let index = index_opt
let rindex = rindex_opt

let lsplit2 s ~on =
  match index s on with
  | None -> None
  | Some i ->
      Some (sub s ~pos:0 ~len:i, sub s ~pos:(i + 1) ~len:(length s - i - 1))

let rsplit2 s ~on =
  match rindex s on with
  | None -> None
  | Some i ->
      Some (sub s ~pos:0 ~len:i, sub s ~pos:(i + 1) ~len:(length s - i - 1))

module Map = Map.Make (String)
