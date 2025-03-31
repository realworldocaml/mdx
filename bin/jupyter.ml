open Mdx.Util.Result.Infix
open Cmdliner

let raw t =
  Notebook_t.
    {
      cell_type = `Raw;
      metadata = { collapsed = None; scrolled = None };
      source = String.concat "\n" t;
      outputs = None;
      execution_count = None;
    }

let txt source =
  Notebook_t.
    {
      cell_type = `Markdown;
      metadata = { collapsed = None; scrolled = None };
      source;
      outputs = None;
      execution_count = None;
    }

let execution_count = ref 1

let ocaml contents =
  let cell =
    Notebook_t.
      {
        cell_type = `Code;
        metadata = { collapsed = None; scrolled = None };
        source = String.concat "\n" contents;
        outputs = Some [];
        execution_count = Some !execution_count;
      }
  in
  incr execution_count;
  cell

let toplevel x =
  let cell =
    Notebook_t.
      {
        cell_type = `Code;
        metadata = { collapsed = None; scrolled = None };
        source = String.concat "\n" x.Mdx.Toplevel.command;
        outputs = Some [];
        execution_count = Some !execution_count;
      }
  in
  incr execution_count;
  cell

let metadata =
  Notebook_t.
    {
      kernelspec =
        {
          display_name = "OCaml 4.07.1";
          language = "OCaml";
          name = "ocaml-jupyter";
        };
      language_info =
        {
          name = "OCaml";
          version = "4.07.1";
          codemirror_mode = Some "text/x-ocaml";
          file_extension = ".ml";
          mimetype = "text/x-ocaml";
          nbconverter_exporter = None;
          pygments_lexer = "OCaml";
        };
    }

let rec collapse_text = function
  | Mdx.Text x :: Mdx.Text y :: xs ->
      collapse_text (Mdx.Text (x ^ "\n" ^ y) :: xs)
  | (Mdx.Section _ as s) :: Mdx.Text y :: xs ->
      let s = Mdx.to_string [ s ] in
      collapse_text (Mdx.Text (s ^ "\n" ^ y) :: xs)
  | (Mdx.Section _ as s) :: xs ->
      let s = Mdx.to_string [ s ] in
      collapse_text (Mdx.Text s :: xs)
  | x :: ys -> x :: collapse_text ys
  | [] -> []

let run _setup (`File file) =
  Mdx.run_to_stdout file ~f:(fun _file_contents items ->
      let cells =
        List.fold_left
          (fun acc -> function
            | Mdx.Text "" -> acc
            | Mdx.Text x -> txt x :: acc
            | Mdx.Block { value = OCaml { env = User_defined _; _ }; _ }
            | Mdx.Block { value = Toplevel { env = User_defined _; _ }; _ } ->
                failwith
                  "internal error, cannot handle user defined environments"
            | Mdx.Block { value = OCaml _; contents; _ } ->
                ocaml contents :: acc
            | Mdx.Block { value = Toplevel _; contents; loc; _ } ->
                let blocks = Mdx.Toplevel.of_lines ~loc contents in
                let newcells = List.rev_map toplevel blocks.Mdx.Toplevel.tests in
                newcells @ acc
            | Mdx.Block { value = Raw _; contents; _ } -> raw contents :: acc
            | x ->
                failwith
                  (Printf.sprintf "internal error, cannot handle: %s"
                     (Mdx.to_string [ x ])))
          [] (collapse_text items)
        |> List.rev
      in
      Notebook_j.string_of_notebook
        Notebook_t.{ metadata; nbformat = 4; nbformat_minor = 2; cells })
  >>! fun () -> 0

let term = Term.(const run $ Cli.setup $ Cli.file)
let doc = "Convert an mdx file to a jupyter notebook."
let info = Cmd.info "jupyter" ~doc
let cmd = Cmd.v info term
