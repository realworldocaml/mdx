Let us setup a simple project with two local packages, defined at the root

  $ cat > a.opam << EOF
  > opam-version: "2.0"
  > EOF

  $ cat > b.opam << EOF
  > opam-version: "2.0"
  > EOF

The project has no dune-project file. That means that if we run `opam-monorepo lock`,
it will have more than onw target: `a` and `b`. It therefore has to determine the name of the
lockfile based on the project's name in the dune-project file. It's expected to fail but it should
to that nicely, letting the user know that it couldn't infer the lockfile and that they should
either explicitly specify it on the command line or add a valid dune-project file at the root.

  $ opam-monorepo lock
  ==> Using 2 locally scanned packages as the targets.
  opam-monorepo: internal error, uncaught exception:
                 (Sys_error
                   "$TESTCASE_ROOT/dune-project: No such file or directory")
                 Raised by primitive operation at Stdlib.open_in_gen in file "stdlib.ml", line 399, characters 28-54
                 Called from Stdlib.open_in in file "stdlib.ml" (inlined), line 404, characters 2-45
                 Called from Sexplib__Pre_sexp.gen_load_rev_sexps in file "duniverse/sexplib/src/pre_sexp.ml", line 688, characters 11-23
                 Called from Sexplib__Pre_sexp.load_rev_sexps in file "duniverse/sexplib/src/pre_sexp.ml" (inlined), line 699, characters 31-75
                 Called from Sexplib__Pre_sexp.load_sexps in file "duniverse/sexplib/src/pre_sexp.ml", line 700, characters 36-62
                 Called from Duniverse_lib__Dune_file.Raw.as_sexps in file "lib/dune_file.ml", line 73, characters 11-59
                 Called from Duniverse_lib__Project.name in file "lib/project.ml", line 40, characters 2-37
                 Called from Duniverse_lib__Project.lockfile in file "lib/project.ml", line 50, characters 9-15
                 Called from Duniverse_cli__Lock.run.(fun) in file "cli/lock.ml", line 310, characters 2-56
                 Called from Cmdliner_term.app.(fun) in file "duniverse/cmdliner/src/cmdliner_term.ml", line 25, characters 19-24
                 Called from Cmdliner.Term.term_result.(fun) in file "duniverse/cmdliner/src/cmdliner.ml", line 33, characters 27-34
                 Called from Cmdliner.Term.run in file "duniverse/cmdliner/src/cmdliner.ml", line 117, characters 32-39
  [125]
