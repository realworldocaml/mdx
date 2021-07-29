## unreleased

### Added

- When querying the solver for the local packages, if no explicit version was
  provided, use the value of the version field in the opam files instead of the
  default `zdev` if it is defined (#183, @emillon)

### Changed

### Deprecated

### Fixed

- Improve `lock` performance (about 2x faster) by loading the repository state
  only once (#188, @emillon)

### Removed

### Security

## 0.2.3

### Fixed

- Fix compat with opam 2.1.0~rc2 by upgrading vendored opam libs

## 0.2.2

### Added

- Add `--ocaml-version` argument to `lock`: it allows to determine the ocaml version in the
  lockfile that's being generated (#161, @pitag-ha)

### Changed

- Exclude packages depending on `jbuilder` from the lock step. Since dune 2.0, `jbuild` files are
  not supported. A new `--allow-jbuilder` option have been added to enable the old behavior. 
- Recognize packages with an optional dependency on dune as building with dune. This allows
  opam-monorepo to rightfully recognize `opam-file-format` latest versions as building with 
  dune. (#176, @NathanReb)
- Only print the full list of selected root packages once and only in verbose mode, simply printing
  the number in the default logs. (#173, @NathanReb)
- Improve the solving process so it only accepts base-compilers unless one explicitly requires
  a compiler variant, either directly or using `ocaml-option-*` packages. (#178, @NathanReb)

### Fixed

- Fix the default branch mechanism when the opam remote starts with `git+https` (#166, @TheLortex)
- Fix a log that was still refering to the old tool name `duniverse` (#158, @rizo)
- Improve how the default branch for a git repository is queried, fixing a bug
  where opam-monorepo wouldn't work outside of of git repo and a bug where it wouldn't
  work on non-english systems. (#157, fixes #114, @TheLortex)

## 0.2.1

### Fixed

- Fix `--recurse-opam` option for the monorepo lock phase: correctly perform special directory
  filtering, add an error message when two versions of the same package opam file exist in the
  source tree, perform package name filtering before checking for uniqueness (#151, @TheLortex)

## 0.2.0

### Changed

- Include transitive depexts in the lockfile (#144, @NathanReb)

## 0.1.0

Initial release
