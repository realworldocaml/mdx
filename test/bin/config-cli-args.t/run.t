We have a local package 'config-cli-args' that has the following
opam file:

  $ cat > config-cli-args.opam << EOF
  > opam-version: "2.0"
  > depends: [
  >   "dune"
  >   "b"
  >   "c"
  > ]
  > EOF

It needs to use two local repositories for 'lock' to be successful.
The minimal repo that we generate here:

  $ gen-minimal-repo

that contains 'dune' but also the locally defined repo that contains
'b' and 'c'.
At the moment it has no 'x-opam-monorepo-opam-repositories' extension set.
We can use the '--opam-repositories' option to set it via the command line:

  $ opam-monorepo lock --opam-repositories '[file://$OPAM_MONOREPO_CWD/minimal-repo,file://$OPAM_MONOREPO_CWD/repo]' > /dev/null
  $ opam show --just-file -fx-opam-monorepo-opam-repositories ./config-cli-args.opam.locked
  file://$OPAM_MONOREPO_CWD/minimal-repo, file://$OPAM_MONOREPO_CWD/repo

We can see from the generated lock file that it used the set of repos we provided on
the command line.

Now we can also use the '--add-opam-repositories' option to complement the local
opam extensions.

  $ echo 'x-opam-monorepo-opam-repositories: ["file://$OPAM_MONOREPO_CWD/minimal-repo"]' >> ./config-cli-args.opam
  $ opam show --just-file -fx-opam-monorepo-opam-repositories ./config-cli-args.opam
  file://$OPAM_MONOREPO_CWD/minimal-repo

Here we just added one of the two repos to our opam file. We can tell the solver
to use an additional one by invoking:

  $ rm ./config-cli-args.opam.locked
  $ opam-monorepo lock --add-opam-repositories '[file://$OPAM_MONOREPO_CWD/repo]' > /dev/null
  $ opam show --just-file -fx-opam-monorepo-opam-repositories ./config-cli-args.opam.locked
  file://$OPAM_MONOREPO_CWD/minimal-repo, file://$OPAM_MONOREPO_CWD/repo

Once again, we can see from the generated lock file that it used both repos.

Note that you can use the '--opam-repositories' option to overwrite any local
extension.

  $ sed -i '$d' ./config-cli-args.opam
  $ echo 'x-opam-monorepo-opam-repositories: ["https://a.com/a.tbz"]' >> ./config-cli-args.opam
  $ opam show --just-file -fx-opam-monorepo-opam-repositories ./config-cli-args.opam
  https://a.com/a.tbz

Here we replaced our opam-repositories to point to 'https://a.com/a.tbz'. We
can once again use '--opam-repositories' as in our first 'lock' attempt to
overwrite that and use the proper repositories instead:

  $ opam-monorepo lock --opam-repositories '[file://$OPAM_MONOREPO_CWD/minimal-repo,file://$OPAM_MONOREPO_CWD/repo]' > /dev/null
  $ opam show --just-file -fx-opam-monorepo-opam-repositories ./config-cli-args.opam.locked
  file://$OPAM_MONOREPO_CWD/minimal-repo, file://$OPAM_MONOREPO_CWD/repo
