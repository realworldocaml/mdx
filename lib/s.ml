type section = int * string

type output = [`Output of string | `Ellipsis]

type cram = {
  command  : string list;
  output   : output list;
  exit_code: int;
}

type toplevel = {
  line   : int;
  command: string list;
  output : output list;
}

type block_value =
  | Raw
  | Cram of { pad: int; tests: cram list }
  | Toplevel of { pad: int; tests: toplevel list }

type block = {
  line    : int;
  file    : string;
  section : section option;
  labels  : string list;
  header  : string option;
  contents: string list;
  value   : block_value;
}

type line =
  | Section of section
  | Text    of string
  | Block   of block
