We have a simple project with a single package which depends on
packages 'a' and 'b': 

  $ opam show --just-file -fdepends ./minimal-update.opam
  dune, a, b

We run an initial lock and get the following locked verions of
our dependencies:

  $ gen-minimal-repo
  $ opam-monorepo lock > /dev/null
  $ opam show --just-file -fdepends ./minimal-update.opam.locked | grep -e "\"b\"" -e "\"a\""
  "a" {= "0.1" & ?vendor}
  "b" {= "0.1" & ?vendor}

Now we add a dependency to 'c' to our project

  $ sed -i '/"b"/a "c"' minimal-update.opam 
  $ opam show --just-file -fdepends ./minimal-update.opam
  dune, a, b, c

We would then like to update our lock file to include 'c' but we do not
want to update the other dependencies if it is not required

Both 'a' and 'b' got a new release:

  $ mkdir repo/packages/a/a.0.2
  $ cp repo/packages/a/a.0.1/opam repo/packages/a/a.0.2/opam
  $ mkdir repo/packages/b/b.0.2
  $ cp repo/packages/b/b.0.1/opam repo/packages/b/b.0.2/opam

To stick to the minimal lock file update, we use the --minimal-update
flag. This should add c to the lock file while keeping 'a' and 'b' to their
previous version.

  $ opam-monorepo lock --minimal-update > /dev/null
  $ opam show --just-file -fdepends ./minimal-update.opam.locked | grep -e "\"b\"" -e "\"a\"" -e "\"c\""
  "a" {= "0.1" & ?vendor}
  "b" {= "0.1" & ?vendor}
  "c" {= "0.1" & ?vendor}

Now say we want to also update 'b' because we need a feature that is only
available in the latest version:

  $ sed -i 's/"b"/"b" {>= "0.2"}/' minimal-update.opam
  $ opam show --just-file -fdepends ./minimal-update.opam
  "dune" "a" "b" {>= "0.2"} "c"

Locking with --minimal-update should update 'b' but not 'a':

  $ opam-monorepo lock --minimal-update > /dev/null
  $ opam show --just-file -fdepends ./minimal-update.opam.locked | grep -e "\"b\"" -e "\"a\"" -e "\"c\""
  "a" {= "0.1" & ?vendor}
  "b" {= "0.2" & ?vendor}
  "c" {= "0.1" & ?vendor}

Alternatively, if we remove a dependency:

  $ sed -i '/"c"/d' ./minimal-update.opam
  $ opam show --just-file -fdepends ./minimal-update.opam
  "dune" "a" "b" {>= "0.2"}

Locking with `--minimal-update` should still allow removing the unnecessary
package from the lock file:

  $ opam-monorepo lock --minimal-update > /dev/null
  $ opam show --just-file -fdepends ./minimal-update.opam.locked | grep -e "\"b\"" -e "\"a\"" -e "\"c\""
  "a" {= "0.1" & ?vendor}
  "b" {= "0.2" & ?vendor}
