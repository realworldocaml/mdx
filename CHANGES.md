## unreleased

### Added

### Changed

### Deprecated

### Fixed

- Fix a log that was still refering to the old tool name `duniverse` (#158, @rizo)
- Use `git ls-remote --symref <remote> HEAD` to find the main branch of a given remote. 
  (#157, fixes #114, @TheLortex)

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
