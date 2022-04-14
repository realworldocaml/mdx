include module type of struct
  include StringLabels
end

val index : string -> char -> int option
val rindex : string -> char -> int option
val lsplit2 : string -> on:char -> (string * string) option
val rsplit2 : string -> on:char -> (string * string) option
val extract_blank_separated_words : string -> string list
val is_prefix : string -> prefix:string -> bool
val is_suffix : string -> suffix:string -> bool
val drop_prefix : string -> prefix:string -> string option
val drop_suffix : string -> suffix:string -> string option

module Map : Map.S with type key = string
