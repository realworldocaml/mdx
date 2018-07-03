# duniverse -- the spice of OCaml build life

This is an experimental vendoring system for Dune.  Not for public consumption
yet.

The high-level commands that implement a vendor Git branching strategy are:

```
# generate lock files from opam metadata
duniverse vendor-lock
# pull external trees into a `duniverse-vendor-of-master` branch
duniverse vendor-pull
# merge the vendor branch into the `master` branch as a single squashed commit
duniverse vendor-merge
```

Only the final `vendor-merge` will affect the master branch, and the squashed
vendor commits can easily be rebased out in the future if you change your mind
about vendoring.

The low-level commands are:

```
duniverse opam <packages>
duniverse lock
duniverse pull
duniverse status
```

State files are:
- [.duniverse/opam.sxp](.duniverse/opam.sxp): the opam-specific metadata
- [.duniverse/dune.sxp](.duniverse/dune.sexp): the Duniverse Git remotes (calculated from the opam, or manually written as you prefer)

The details of individual vendor merges from upstream repos are kept in the [duniverse-vendor-of-master](https://github.com/avsm/duniverse/tree/duniverse-vendor-of-master) branch to avoid polluting the mainline `master` branch with many small commits.
