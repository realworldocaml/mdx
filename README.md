# duniverse -- the spice of OCaml build life

This is an experimental vendoring system for Dune.  Not for public consumption
yet.

```
duniverse opam <packages>
duniverse lock
duniverse pull
duniverse status
```

State files are:
- `.duniverse-opam.sxp`: the opam-specific metadata
- `.duniverse.sxp`: the Duniverse Git remotes (calculated from the opam, or manually written as you prefer)
