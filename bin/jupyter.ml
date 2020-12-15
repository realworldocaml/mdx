open Mdx.Util.Result.Infix
open Cmdliner

let raw t =
  let cell =
    Notebook_t.
      {
        cell_type = `Raw;
        metadata = { collapsed = None; scrolled = None };
        source = String.concat "\n" t;
        outputs = None;
        execution_count = None;
      }
  in
  cell

let txt t =
  let cell =
    Notebook_t.
      {
        cell_type = `Markdown;
        metadata = { collapsed = None; scrolled = None };
        source = t;
        outputs = None;
        execution_count = None;
      }
  in
  cell

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

let run _setup (`Syntax syntax) (`File file) =
  let cells = ref [] in
  Mdx.run ?syntax file ~f:(fun _file_contents items ->
      let syntax =
        match syntax with
        | Some s -> s
        | None -> (
            match Mdx.Syntax.infer ~file with
            | Some s -> s
            | None -> failwith "Couldn't get syntax" )
      in
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
      in
      List.iter
        (function
          | Mdx.Text "" -> ()
          | Mdx.Text x -> cells := txt x :: !cells
          | Mdx.Block { value = OCaml { env = User_defined _; _ }; _ }
          | Mdx.Block { value = Toplevel { env = User_defined _; _ }; _ } ->
              failwith "internal error, cannot handle user defined environments"
          | Mdx.Block { value = OCaml _; contents; _ } ->
              cells := ocaml contents :: !cells
          | Mdx.Block { value = Toplevel _; contents; file; column; line; _ } ->
              let blocks =
                Mdx.Toplevel.of_lines ~syntax ~file ~column ~line contents
              in
              let newcells = List.rev_map toplevel blocks in
              cells := newcells @ !cells
          | Mdx.Block { value = Raw _; contents; _ } ->
              cells := raw contents :: !cells
          | x ->
              failwith
                (Printf.sprintf "internal error, cannot handle: %s"
                   (Mdx.to_string [ x ])))
        (collapse_text items);
      "OK")
  >>! fun () ->
  ();
  let notebook =
    Notebook_t.
      {
        metadata =
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
          };
        nbformat = 4;
        nbformat_minor = 2;
        cells = List.rev !cells;
      }
  in
  Printf.fprintf stdout "%s" (Notebook_j.string_of_notebook notebook);
  0

let cmd : int Term.t * Term.info =
  let doc = "Convert an mdx file to a jupyter notebook." in
  (Term.(pure run $ Cli.setup $ Cli.syntax $ Cli.file), Term.info "jupyter" ~doc)
