## unreleased

### Added

### Changed

### Deprecated

### Fixed

- Fix a log that was still refering to the old tool name `duniverse` (#158, @rizo)
- Improve how the default branch for a git repository is queried, fixing a bug
  where opam-monorepo wouldn't work outside of of git repo and a bug where it wouldn't
  work on non-english systems. (#157, fixes #114, @TheLortex)

### Removed

### Security

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
