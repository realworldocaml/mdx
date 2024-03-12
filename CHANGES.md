### unreleased

#### Changed

- Revert #446: "Allow execution of included OCaml code blocks" (#451, @gpetiot).
  Included OCaml code blocks preserve their pre-2.4.0 behavior.

### 2.4.0

#### Added

- Handle the error-blocks syntax (#439, @jonludlam, @gpetiot)
- Allow execution of included OCaml code blocks. Add `skip` to `include` blocks
  to revert to the old behavior (#446, @panglesd, @gpetiot)
  *Warning: this is a breaking change that is reverted in the next release.*
- Make MDX compatible with OCaml 5.2 (#448, @gpetiot)

#### Fixed

- Reduce false-positives while detecting warnings (#440, @Julow)

### 2.3.1

#### Added

- Add `os_type` label to enable/disable based on `Sys.os_type` (#433,
  @polytypic)

- Make MDX compatible with OCaml 5.1 (#435, @polytypic and @kit-ty-kate)

#### Changed

- Vendored the odoc-parser library, removing the need to have it
  as a dependency. (#430, @jonludlam)

### 2.3.0

#### Added

- Added support for `mld` files (#423, @jonludlam)

#### Changed

- Switch to using the parser that toplevel uses (found in a mutable
  `ref`, instead of always the official OCaml parser).  This allows
  Camlp5's parser to be used with MDX. (#417, @chetmurthy)

### 2.2.1

#### Fixed

- Undid the change to the pipe code to restore compatibility with Windows
  (#403, @MisterDA)

### 2.2.0

#### Added

- Report all parsing errors in Markdown files (#389, @NathanReb)

#### Changed

- Preserve indentation in multiline OCaml blocks in .mli files (#395, @panglesd)
- Rename the `Normal` syntax to `Markdown` to better explain what the syntax is
  and moved it to `Mdx.Syntax` (#412, @Leonidas-from-XIV)

#### Fixed

- Fixed compatibility with Cmdliner 1.1.0 (#371, @Leonidas-from-XIV)
- Report errors and exit codes of toplevel directives (#382, @talex5,
  @Leonidas-from-XIV)
- Fix block locations in error reporting (#389, @NathanReb)
- Include the content of the line that features the `part-end` MDX directive in
  the output, before that line would've been dropped (#374, #387,
  @Leonidas-from-XIV)
- Handle EINTR signal on waitpid call by restarting the syscall. (#409, @tmcgilchrist)
- Fix parsing of multiline toplevel phrases in .mli files (#394, #397,
  @Leonidas-from-XIV)

#### Removed

- Removed warning about missing semicolons added in MDX 1.11.0 and the
  automatic insertion of semicolons in the corrected files introduced in MDX
  2.0.0. (#398, @Leonidas-from-XIV)

### 2.1.0

#### Added

- Add support for adding language tags and metadata labels in `mli` files.
  (#339, #357, @julow, @Leonidas-from-XIV)
- Add support for running non-deterministic tests in `dune` MDX 0.2 stanza by
  setting the `MDX_RUN_NON_DETERMINISTIC` environment variable. (#365,
  #366, @Leonidas-from-XIV)

### 2.0.0

#### Added

- Add trailing `;;` to the output of toplevel phrases that were missing it.
  (#346, @Leonidas-from-XIV)
- Make MDX compatible with OCaml 4.14 (#356, @NathanReb)

#### Fixed

- Use the same output as the normal toplevel. Mdx used to carry an unsafe patch
  to work around a bug fixed in OCaml 4.06 and that patch would change the
  printed types in some corner cases. (#322, @emillon)

#### Removed

- Dropped compatibility with older OCaml versions. The minimal supported range
  is 4.08 to 4.13 now (#345, @Leonidas-from-XIV)
- Do not install deprecated `mdx` binary anymore (#274, @gpetiot)
- Remove deprecated `rule` command (#312, @gpetiot)
- Remove support for `require-package` label, use the `mdx` stanza in dune
  instead. This label was only used for the `rule` command and can now be
  safely removed. (#363, @Leonidas-from-XIV)

### 1.11.0

#### Added

#### Changed

- Use odoc-parser.0.9.0 (#333, @julow)

#### Deprecated

- Add a deprecation warning for toplevel blocks that are not terminated with `;;` (#342, @Leonidas-from-XIV)

#### Fixed

- Fix accidental redirect of stderr to stdout (#343, @Leonidas-from-XIV)
- Remove trailing whitespaces that were added to indent empty lines (#341, @gpetiot)

#### Removed

#### Security

### 1.10.1

#### Added

- Support for OCaml 4.13 (#330, @emillon)

### 1.10.0

#### Added

- Display OCaml warnings in mdx-error blocks (#293, @gpetiot)

#### Fixed

- Show exceptions in the correct order (#332, @talex5)

### 1.9.0

#### Added

- Add a new `dune-gen` subcommand that generates testing code for Dune to build
  and run with the new `mdx` stanza. (#305, @voodoos)

### 1.8.1

#### Changed

- Dropped OMP dependency and use handwritten compat layers instead
  (#317, @NathanReb)

### 1.8.0

#### Added

- Allow to explicitly set the kind of blocks in labels: `ocaml`, `cram`, `toplevel` or `include`. (#237, @gpetiot)
- Include blocks do not require an empty block anymore (#286, @gpetiot)
- Support for OCaml 4.12 (#298, @kit-ty-kate)

#### Changed

- Improve error message of cram test exceptions due to empty lines in a block (#270, @pitag-ha)

#### Fixed

- Report `#require` directive errors (#276, @gpetiot)
- Handle no such file exception: the input file and the values of options `--root` and `--prelude` are checked (#292, @gpetiot)
- Keep locations from parsing instead of recomputing the lines, providing better error messages (#241, @gpetiot)
- Use `create_process` instead of `execvp` to call `mdx-test` from `mdx`. This fixes running mdx from dune on Windows (#299, @emillon)

### 1.7.0

#### Added

- HTML comments can carry block labels (#234, @gpetiot)
  The syntax is: `<!-- $MDX labels -->`, where `labels` is a list of valid
  labels separated by a comma. This line has to immediately precede the block
  it is attached to. The legacy syntax is preserved and will be deprecated in a
  later release.
- Add support for toplevel blocks in `.mli` files' doc comments (#206, @jsomers)
- Add support for OCaml 4.11 (#261, @kit-ty-kate)

#### Changed

- Apply unnamed preludes to all environments (#271, @gpetiot)
  New behavior:
   * `env_and_file "a:f"` associates `f` to the environment named `a`
   * `env_and_file " :f"` associates `f` to the default environment
   * `env_and_file "f"` associates `f` to all environments.
- Errors in non toplevel OCaml blocks are now printed to a seperate `mdx-error` code block
  following the ocaml block instead of crashing the mdx process. Those `mdx-error` blocks
  are recognized and checked by mdx and can be intentionally used to show case specific
  compile errors. (#238, @gpetiot)
- Improve error reporting for invalid `(* $MDX part-... *)` delimiters (#250, @gpetiot)

#### Deprecated

- The command 'mdx rule' is deprecated and will be removed in 2.0.0 (#251, @gpetiot)

#### Fixed

- Fix the environment selection for preludes and slightly improve quality
  of type names in evaluations of toplevel phrases in certain cases. (#225, @gpetiot)
- Fix toplevel parsing when phrases contain tabs (#240, @gpetiot)
- Avoid adding newlines to empty blocks (#253, @gpetiot)
- Preserve the indentation of included files (#259, @gpetiot)
- Preserve the header in shell blocks (#249, @craigfe)
- Support underscores in environment variables in `set-` and `unset-` labels (#257, @shonfeder)
- Fix mdx on Windows which was looking for the ocaml-mdx-test binary at the wrong place
  (#263, @hcarty)
- Properly report mdx parsing errors instead of crashing with an uncaught exception (#267, @gpetiot)

### 1.6.0

#### Added

- Add a `--duniverse-mode` to `ocaml-mdx rule` so that the generated rules work
  within a duniverse
- Allow to import arbitrary files (not only .ml/.mli ones) into code blocks using
  the `file` label. (#203, #207, @voodoos)
- Allow to set the `--non-deterministic` option through the `MDX_RUN_NON_DETERMINISTIC`
  env variables (#208, @NathanReb)
- Add support for OCaml 4.10 (#204, @kit-ty-kate)
- Infer syntax kind when `--syntax` is not set, and add 'markdown' as an alias to 'normal' (#222, @gpetiot)
- Add `ocaml-mdx deps` command to be used by dune to compute file and dir dependencies of an
  mdx file. (#217, @voodoos)
- Add new delimiters syntax using comments for partial OCaml files include (#212, @voodoos).

#### Changed

- Do not unset `INSIDE_DUNE` when executing shell commands by default (#224, @NathanReb)

#### Fixed

- Fix a bug that could cause `ocaml-mdx test` to crash on some `include` in toplevel code blocks
  (#202, @trefis)

#### Removed

- Remove the `direction` option, only synchronize from .ml to .md files (#214, @gpetiot)

### 1.5.0 (2019-11-29)

#### Added

- Add a `--output`/`-o` option to the `test` subcommand to allow specifying a different
  output file to write the corrected to, or to write it to the standard output (#194, @NathanReb)
- Migrate to OCaml 4.08 AST to add support for `let*` bindings (#190, @gpetiot)
- Add `--syntax` option to `rule` subcommand to allow generating rules for cram
  tests (#177, @craigfe)
- Add a `require-package` label to explicitly declare dune `package` dependencies of a code block
  (#149, @Julow)
- Add an `unset-` label to unset env variables in shell blocks (#132, @clecat)

#### Changed

- Format rules generated by `ocaml-mdx rule` using `dune format-dune-file` (#184, @NathanReb)
- Run promotion of markdown files before `.ml` files in generated dune rules (#140, @clecat)

#### Fixed

- Use module_presence information on Env.summary to prevent fetching absent modules from the
  toplevel env (#186, @clecat)
- Remove trailing whitespaces at the end of toplevel or bash evaluation lines
  (#166, @clecat)
- Improve error reporting of ocaml-mdx test (#172, @Julow)
- Rule: Pass the --section option to `test` (#176, @Julow)
- Remove trailing whitespaces from shell outputs and toplevel evals (#166, @clecat)
- Remove inappropriate empty lines in generated dune rules (#163, @Julow)
- Fix ignored `skip` label in `ocaml-mdx pp` (#1561, @CraigFe)
- Fix synchronization of new parts from markdown to `.ml` (#156, @Julow)
- Fix ignored `[@@@parts ...]` markers within module definitions (#155, @Julow)
- Fix a bug in internal OCaml version comparison that lead to crashes in some cases (#145, @gpetiot)
- Promote to empty `.ml` file when using `to-ml` direction (#139, @clecat)
- Apply `--force-output` to `.ml` file as well (#137, @clecat)
- Fix a bug preventing `.corrected` files to be written in some cases (#136, @clecat)
- Add compatibility with `4.09.0` (#133, @xclerc)

#### Removed

- Remove the `output` subcommand as it was very specific to RealWorldOCaml needs (#195, @NathanReb)
- Remove the `infer-timestamp` direction (#171 @Julow)

### 1.4.0 (2019-06-11)

- Add `--force-output` option to force generation of diff file (#118 @clecat)
- Support OCaml 4.08.0 (#121 @xclerc)
- README and documentation fixes (#122 #118 @andreypopp @clecat @samoht)
- Use latest ocaml-migrate-parsetree interfaces (@avsm)

### 1.3.0 (2019-03-01)

- Updated readme file with the new features: dune rules, named environment and
  ocaml versions, Some grammar correction too (@gpetiot, #101, aantron, #102)
- Better lexer error messages (@avsm, #103)
- Added cram syntax parsing (@trefis, #106)
- Renamed mdx to ocaml-mdx to avoid conflicts/for more precision (@clecat, #110, #115)
- Fix blank spaces causing parsing errors (@gpetiot, #97)
- Fix empty lines causing a String.sub (@clecat, #107)

### 1.2.0 (2018-01-03)

- Support end-of-line ellipsis (@dra27, #85)
- Support OCaml 4.02.3 (@gpetiot, #86)
- Support `version=..`, `version<=..` and `version>=..` keywords to run
  a code-block depending on the currently installed OCaml version
  (@gpetiot, #87, #90)
- Upgrade Travis tests to use opam 2.0.2 (@avsm, #89)
- Do not depend on `ppx_tools` for toplevel (@avsm, #89)
- Fix embedding in a larger Dune project with a cppo override (@avsm, #89)
- `mdx output`: escape HTML entities in code blocks (#91, @samoht)

### 1.1.0 (2018-11-16)

- Add a mechanism to promote files to blocks and blocks to file
  (@gpetiot, #37)
- Support multiple toplevel environments (@gpetiot, #38)
- Use ocaml-migrate-parsetree to compile in 4.06.1 & 4.07.0 (@gpetiot, #41)
- Add a `mdx rule` command to generate dune rules (@gpetiot, #44)
- Add a `mdx output` command to generate an HTML document (@samoht, #45)
- Support empty code blocks (@samoht, #46)
- Fix detection of OCaml code/toplevel (@samoht, #47)
- Better handling of multi-line shell scripts (@samoht, #48)
- Fix regression in toplevel blocks when creating newtype (@samoht, #49)
- Fix evaluation of non-determinitic test (@samoht, #50)
- Improve mdx rules to take into account more precise dependencies (@samoht, #51)
- Fix promotion of blocks to complete ML files (@samoht, #52)
- mdx does not use the `cppo` library, just the binary (@samoht, #53)
- fix ellipsis in code blocks (@samoht, #57)
- Fix relative paths for promoted blocks to files (@samoht, #58)
- Fix location of errors for multi-line commands (@samoht, #60)
- improve the parser for shell blocks (@samoht, #61)
- Allow to load preludes in specific environments (@samoht, #63)
- Fix evaluation of code after directives in prelude (@samoht, #64)
- Improve promotion to ml files (@samoht, #66)
- mdx rule: generates (source_tree) dependencies for directory metadata (@samoht, #67)
- Fix handling of 'module type' in multiple toplevel environment (@samoht, #68)
- Add an eval=false label to skip the evaluation of a code block (@samoht, #69)
- Fix parsing of shell blocks with multiple exit codes (@samoht, #71)
- Support source-tree as extra block metadata (@samoht, #72)
- Better formatting of non-compiling promoted contents (@samoht, #73)
- Be sure to remove the .corrected files if the promotion to ML file works (mdx74)
- Add missing dependency in test/dune (@samoht, #75)
- Support `dir=..` labels in ml code blocks (@samoht, #76)
- Allow to promote to mli files too (@samoht, #77)
- Support multi-line strings (@samoht, #78)
- fail (and exit 1) if prelude and ml blocks cannot be evaluated properly
  (@samoht, #80, @samoht, #83)
- Allow to pass --root to `mdx rule` (@samoht, #81)
- mdx rule: do not add (package mdx) in the dependencies (@samoht, #82)

## 1.0.0 (2018-09-23)

- Initial release
