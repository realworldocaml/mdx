We have a simple project with a single package that we will use to generate
a lockfile for our current version.

  $ gen-minimal-repo
  $ opam-monorepo lock > /dev/null
  $ opam show --no-lint --raw -fx-opam-monorepo-version ./lockfile-version.opam.locked
  "0.3"

This is our current version.
At the moment we are not backward compatible so if we downgrade this version number,
pull will refuse the lockfile and suggest it is regenerated with the current plugin:

  $ sed -i "s/x-opam-monorepo-version: \"0.3\"/x-opam-monorepo-version: \"0.2\"/" lockfile-version.opam.locked
  $ opam-monorepo pull > /dev/null
  opam-monorepo: [ERROR] opam-monorepo lockfile version 0.2 is too old. Please regenerate the lockfile using your current opam-monorepo plugin or install an older version of the plugin.
  [1]

We also obviously do not support future versions and they should be rejected as well,
suggesting that the user upgrade their plugin to be able to interpret that lockfile:

  $ sed -i "s/x-opam-monorepo-version: \"0.2\"/x-opam-monorepo-version: \"0.999\"/" lockfile-version.opam.locked
  $ opam-monorepo pull > /dev/null
  opam-monorepo: [ERROR] Incompatible opam-monorepo lockfile version 0.999. Please upgrade your opam-monorepo plugin.
  [1]
