We have a simple project with two local packages, defined at the root

  $ cat missing-dune-project.opam
  opam-version: "2.0"

  $ cat missing-dune-project-as-well.opam
  opam-version: "2.0"

The project has no dune-project file. That means that if we run `opam-monorepo lock`,
it will have more than one target: `a` and `b`. It therefore has to determine the name of the
lockfile based on the project's name in the dune-project file. It's expected to fail but it should
to that nicely, letting the user know that it couldn't infer the lockfile and that they should
either explicitly specify it on the command line or add a valid dune-project file at the root.

  $ opam-monorepo lock
  ==> Using 2 locally scanned packages as the targets.
  opam-monorepo: [ERROR] Could not infer the target lockfile name: Missing dune-project file at the root: $TESTCASE_ROOT/dune-project
  Try setting it explicitly using --lockfile or add a project name in a root dune-project file.
  [1]
