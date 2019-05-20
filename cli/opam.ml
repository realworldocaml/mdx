let run repo branch explicit_root_packages excludes pins remotes () =
  Duniverse_lib.Opam_cmd.init_duniverse repo branch explicit_root_packages excludes pins remotes

let branch =
  let doc =
    "Branch that represents the working tree of the source code. Defaults to $(i,master)"
  in
  Cmdliner.Arg.(value & opt string "master" & info [ "b" ] ~docv:"BRANCH" ~doc)

let excludes =
  let doc =
    "Packages to exclude from the output list. You can use this to remove the root packages so \
     they are not duplicated in the vendor directory.  Repeat this flag multiple times for more \
     than one exclusion."
  in
  Cmdliner.Arg.(value & opt_all string [] & info [ "exclude"; "x" ] ~docv:"EXCLUDE" ~doc)

let remotes =
  let doc =
    "Extra opam remotes to add when resolving package names. Repeat this flag multiple times for \
     more than one remote."
  in
  Cmdliner.Arg.(value & opt_all string [] & info [ "opam-remote" ] ~docv:"OPAM_REMOTE" ~doc)

let pin_converter =
  let open Duniverse_lib.Types.Opam in
  let fin s =
    match Astring.String.cuts ~sep:"," s with
    | [] -> failwith "unexpected pin error"
    | [ pin ] -> Ok { pin; url = None; tag = None }
    | [ pin; url ] -> Ok { pin; url = Some url; tag = None }
    | [ pin; url; tag ] -> Ok { pin; url = Some url; tag = Some tag }
    | _ -> failwith "pins must have maximum of 3 commas"
  in
  let fout ppf { pin; url; tag } =
    match (url, tag) with
    | None, _ -> Fmt.(pf ppf "%s" pin)
    | Some url, None -> Fmt.(pf ppf "%s,%s" pin url)
    | Some url, Some tag -> Fmt.(pf ppf "%s,%s,%s" pin url tag)
  in
  Cmdliner.Arg.conv ~docv:"PIN" (fin, fout)

let pins =
  let doc =
    "Packages to pin for the latest opam metadata and source. You can separate the package name \
     and a url and a remote branch via commas to specify a manual url (e.g. \
     $(i,mirage,git://github.com/avsm/mirage,fixme)).  If a url is not specified then the \
     $(i,--dev) pin is used.  If a branch is not specified then the default remote branch is \
     used. Repeat this flag multiple times for more than one exclusion."
  in
  Cmdliner.Arg.(value & opt_all pin_converter [] & info [ "pin"; "p" ] ~docv:"PIN" ~doc)

let explicit_root_packages =
  let doc =
    "opam packages to calculate duniverse for. If not supplied, any local opam metadata files are \
     used as the default package list."
  in
  Cmdliner.Arg.(value & pos_all string [] & info [] ~doc ~docv:"PACKAGES")

let info =
  let open Cmdliner in
  let doc = "analyse opam metadata to generate a standalone package list" in
  let exits = Term.default_exits in
  let man =
    [ `S Manpage.s_description;
      `P
        "This command analyses the opam package metadata and generates an $(i,opam.sxp) \
         configuration file that contains the full dependency list of packages and their \
         compatibility status with Dune.";
      `P
        "In the situation where a package has not been ported to Dune upstream, we maintain a \
         hardcoded list of forks that contain Dune overlays at \
         $(i,https://github.com/dune-universe).  This command will detect these packages and \
         supply the forked version if available."
    ]
  in
  Term.info "opam" ~doc ~exits ~man

let term =
  let open Cmdliner.Term in
  term_result
    ( const run $ Common.Arg.repo $ branch $ explicit_root_packages $ excludes $ pins $ remotes
    $ Common.Arg.setup_logs () )

let cmd = (term, info)
