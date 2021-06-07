v0.4.1 (22/04/2021)
-------------------

- opam-0install-cudf: Remove unused (`cmdliner`) and unnecessary (`fmt`) dependencies
  for easier integration with opam.
  (@kit-ty-kate #28)

- opam-0install: Be explicit that `Ok` values are not passed to `Term.exit` (@talex5 #24)

v0.4 (09/10/2020)
----------------

- opam-0install-cudf: Fix conflict detection (@kit-ty-kate #20)

v0.3 (17/09/2020)
----------------

- opam-0install-cudf: Allow to tag packages as recommended when giving them to the solver (@kit-ty-kate #16)
  Recommanded packages might or might not be chosen by the solver depending on whether
  the most up-to-date Essential packages available are compatible with them.

- Add an option to get the least up-to-date version of each packages (@kit-ty-kate #18)
  Option available in both opam-0install and opam-0install-cudf libraries
  as well as a new --prefer-oldest option to the opam-0install binary.

- opam-0install-cudf: Remove the unnecessary dependency towards the opam library (@kit-ty-kate #15)

- Documentation: Add a link to API docs in the README (@talex5 #14 #17)

v0.2 (17/06/2020)
----------------

- Add a new `opam-0install-cudf` package (@kit-ty-kate #11).
  This uses opam's CUDF API, allowing the solver to be used directly from within opam.

- `Dir_context.std_env` now has some optional arguments, and also responds for `opam-version` (@talex5 #12).
  You will need to add an extra `()` argument to it to upgrade.

- Evaluate a package's `available` expression in `Dir_context` (@talex5 #12).
  This isn't needed for `Switch_context` because the switch does it for us, but
  `Dir_context` could return packages with `available: false`.

- Simplify the `CONTEXT` API (@talex5 #12).
  `candidates` now returns either `Ok opam` or `Error pkg` for each package.
  This is clearer than using an option type and avoids the need for a separate
  `load` function. It also makes it possible to filter packages based on the
  content of the opam file without having to load it twice. We also no longer
  bother loading the opam file for rejects (all we need is the name).

v0.1 (26/05/2020)
----------------

Initial release.
