## 0.3.0

### Added

- Add opam extensions `x-opam-monorepo-opam-repositories` and
  `x-opam-monorepo-global-opam-vars` to make `lock` fully reproducible.
  (#250, #253, @NathanReb)
- Show an error message when the solver can't find any version that satisfies
  the requested version constraint in the user's OPAM file (#215, #248, #290
  @Leonidas-from-XIV)
- Allow packages to be marked as being provided by Opam and not to be pulled by
  `opam-monorepo`. To control this a new optional Opam file field,
  `x-opam-monorepo-opam-provided` is introduced. Its value is a list of package
  names that are to be excluded from being pulled (#234, @Leonidas-from-XIV)
- Show an error message when the OCaml version of the lock file does not match
  the OCaml version of the switch (#267, #268, @Leonidas-from-XIV)
- Generate a `duniverse/README.md` file to explain the basics of
  `opam-monorepo` in the vendored directory (#272, #274, @Leonidas-from-XIV)
- Add a `--prefer-cross-compile` flag for the solver to select cross-compiling
  versions of packages when available. This is determined through the presence
  of the `"cross-compile"` tag in the opam metadata.

### Changed

- Bump lockfile version to 0.3 (#285, @NathanReb)
- Mark packages to be pulled by opam-monorepo with the `vendor` variable so
  using OPAM with `opam install --deps-only --locked .` will not install
  packages that will be installed with `opam-monorepo pull` (#237,
  @Leonidas-from-XIV)

### Fixed

- Fix a bug where a package which had a single version that built with dune and got selected by the solver
  would be reported has having no version building with dune. (#245, @Leonidas-from-XIV)
- Fix the solver so it does not select beta versions of the compiler unless
  forced to by version constraints or `--ocaml-version`. (#269, @NathanReb)

### Removed

- Drop support for lockfile versions 0.2 and lower (#285, @NathanReb)

## 0.2.7

### Added

- Add a list subcommand to list the duniverse packages in the lockfile
  (#217, @samoht)

### Changed

- Only warn users about missing dune-ports repo in OPAM switch if no solution
  can be found due to packages not building with dune (#210, @Leonidas-from-XIV)
- Rename the `--repo` option to `--root` to make it more
  straightforward  that this is referring to the project root (#218, @samoht)
- Improve the wording of the lockfile selection log (#222, @NathanReb)
- Display the full solver error with `--verbose` (#229, @emillon)

### Fixed

- Better errors for `opam-monorepo depext`, especially for non-interactive
  shells (#216, @samoht)
- Properly detect all opam packages defined in the current repository, preventing it
  from later pulling duplicates into the duniverse if they were part of the target packages
  dependencies. (#203, @Leonidas-from-XIV)
- Properly report missing dune-project file when trying to determine the
  to-be-genrated lockfile name (#227, @NathanReb)

## 0.2.6

### Added

- Add a depext subcommand to install the external system dependencies listed
  in lock file (#207, @samoht)
- Add the `--keep-git-dir` flag to the `pull` command that can be used to keep
  the [.git] directory after pulling the vendored sources. (#160, @rizo)

### Fixed

- Fix tool name in generated dune file so that it does not refer to the tool as
  `duniverse` (#206, @emillon)

## 0.2.5

### Fixed

- Fix setting `OPAMROOT` to accept non-default paths (#197, #198,
  @Leonidas-from-XIV)
- Fix a bug where opam-monorepo would erase the opam cache by upgrading
  opam-libs from `2.1~rc2` to `2.1` (#204, @NathanReb)

## 0.2.4

### Added

- When querying the solver for the local packages, if no explicit version was
  provided, use the value of the version field in the opam files instead of the
  default `zdev` if it is defined (#183, @emillon)

- Add a `-l`/`--lockfile` command line option to explicitly set the lockfile
  to use or generate in `pull` or `lock` (#163, @NathanReb)

- Honor `pin-depends` field in opam files. When present, these will be used by
  the solver (#153, #159, #167, @rizo, @TheLortex, @NathanReb).

### Fixed

- Improve `lock` performance (about 2x faster) by loading the repository state
  only once (#188, #192, @emillon)

- Fix a bug where the dune-project parsing in the `pull` command would fail
  if it used CRLF for new lines. (#191, @NathanReb)

- Simply warn instead of exiting when the dune-project file can't be parsed
  by `pull` as it only use it to suggest updating the lang version for
  convenience (#191, @NathanReb)

- Fix a bug where `pull` and `lock` would expect the lockfile to sit in a
  different place and pull would fail. `pull` now simply looks for a
  `.opam.locked` file and pulls it unless there are multiple matching files in
  the repository's root. (#163, @NathanReb)

- Fix failure when a package is pinned to a specific commit. `lock` now skips
  resolution when the ref is actually a commit pointed by a remote branch or
  when it looks like a commit (hexadecimal characters only, at least 7
  characters-long). (#195, fixes #127, @TheLortex)

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
