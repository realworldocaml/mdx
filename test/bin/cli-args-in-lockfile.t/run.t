When generating a lockfile, the CLI arguments passed to lock
should be save to the lockfile

  $ gen-minimal-repo
  $ opam-monorepo lock --recurse --opam-provided [b] --opam-repositories '[file://$OPAM_MONOREPO_CWD/minimal-repo]' --ocaml-version 4.13.1 > /dev/null
  $ opam show --just-file -fx-opam-monorepo-cli-args ./cli-args-in-lockfile.opam.locked
  --recurse, --opam-provided, [b], --opam-repositories, [file://$OPAM_MONOREPO_CWD/minimal-repo], --ocaml-version, 4.13.1
