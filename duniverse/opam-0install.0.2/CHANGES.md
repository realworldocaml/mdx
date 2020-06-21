### v0.2

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

### v0.1

Initial release.
