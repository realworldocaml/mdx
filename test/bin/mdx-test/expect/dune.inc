
(rule
 (target code.corrected)
 (deps (package mdx) (source_tree code))
 (action
  (with-stdout-to %{target}
   (chdir code
    (run ocaml-mdx test --output - test-case.md)))))

(alias
 (name runtest)
 (action (diff code/test-case.md code.corrected)))

(rule
 (target cram.corrected)
 (deps (package mdx) (source_tree cram))
 (action
  (with-stdout-to %{target}
   (chdir cram
    (run ocaml-mdx test --output - --syntax=cram test-case.md)))))

(alias
 (name runtest)
 (action (diff cram/test-case.md cram.corrected)))

(rule
 (target ellipsis.corrected)
 (deps (package mdx) (source_tree ellipsis))
 (action
  (with-stdout-to %{target}
   (chdir ellipsis
    (run ocaml-mdx test --output - test-case.md)))))

(alias
 (name runtest)
 (action (diff ellipsis/test-case.md ellipsis.corrected)))

(rule
 (target ellipsis-updates.corrected)
 (deps (package mdx) (source_tree ellipsis-updates))
 (action
  (with-stdout-to %{target}
   (chdir ellipsis-updates
    (run ocaml-mdx test --output - test-case.md)))))

(alias
 (name runtest)
 (action (diff ellipsis-updates/test-case.md.expected ellipsis-updates.corrected)))

(rule
 (target empty-line.corrected)
 (deps (package mdx) (source_tree empty-line))
 (action
  (with-stdout-to %{target}
   (chdir empty-line
    (run ocaml-mdx test --output - test-case.md)))))

(alias
 (name runtest)
 (action (diff empty-line/test-case.md empty-line.corrected)))

(rule
 (target empty-lines.corrected)
 (deps (package mdx) (source_tree empty-lines))
 (action
  (with-stdout-to %{target}
   (chdir empty-lines
    (run ocaml-mdx test --output - test-case.md)))))

(alias
 (name runtest)
 (action (diff empty-lines/test-case.md.expected empty-lines.corrected)))

(rule
 (target environment-variable.corrected)
 (deps (package mdx) (source_tree environment-variable))
 (action
  (with-stdout-to %{target}
   (chdir environment-variable
    (run ocaml-mdx test --output - test-case.md)))))

(alias
 (name runtest)
 (action (diff environment-variable/test-case.md environment-variable.corrected)))

(rule
 (target environment-variable-set.corrected)
 (deps (package mdx) (source_tree environment-variable-set))
 (action
  (with-stdout-to %{target}
   (chdir environment-variable-set
    (run ocaml-mdx test --output - test-case.md)))))

(alias
 (name runtest)
 (action (diff environment-variable-set/test-case.md environment-variable-set.corrected)))

(rule
 (target environment-variable-unset.corrected)
 (deps (package mdx) (source_tree environment-variable-unset))
 (action
  (with-stdout-to %{target}
   (chdir environment-variable-unset
    (run ocaml-mdx test --output - test-case.md)))))

(alias
 (name runtest)
 (action (diff environment-variable-unset/test-case.md environment-variable-unset.corrected)))

(rule
 (target envs.corrected)
 (deps (package mdx) (source_tree envs))
 (action
  (with-stdout-to %{target}
   (chdir envs
    (run ocaml-mdx test --output - test-case.md)))))

(alias
 (name runtest)
 (action (diff envs/test-case.md envs.corrected)))

(rule
 (target errors.corrected)
 (deps (package mdx) (source_tree errors))
 (action
  (with-stdout-to %{target}
   (chdir errors
    (run ocaml-mdx test --output - test-case.md)))))

(alias
 (name runtest)
 (action (diff errors/test-case.md errors.corrected)))

(rule
 (target exit.corrected)
 (deps (package mdx) (source_tree exit))
 (action
  (with-stdout-to %{target}
   (chdir exit
    (run ocaml-mdx test --output - test-case.md)))))

(alias
 (name runtest)
 (action (diff exit/test-case.md exit.corrected)))

(rule
 (target heredoc.corrected)
 (deps (package mdx) (source_tree heredoc))
 (action
  (with-stdout-to %{target}
   (chdir heredoc
    (run ocaml-mdx test --output - test-case.md)))))

(alias
 (name runtest)
 (action (diff heredoc/test-case.md heredoc.corrected)))

(rule
 (target labels.corrected)
 (deps (package mdx) (source_tree labels))
 (action
  (with-stdout-to %{target}
   (chdir labels
    (run ocaml-mdx test --output - -s Testing test-case.md)))))

(alias
 (name runtest)
 (action (diff labels/test-case.md.expected labels.corrected)))

(rule
 (target lines.corrected)
 (deps (package mdx) (source_tree lines))
 (action
  (with-stdout-to %{target}
   (chdir lines
    (run ocaml-mdx test --output - test-case.md)))))

(alias
 (name runtest)
 (action (diff lines/test-case.md lines.corrected)))

(rule
 (target lwt.corrected)
 (deps (package mdx) (source_tree lwt))
 (action
  (with-stdout-to %{target}
   (chdir lwt
    (run ocaml-mdx test --output - test-case.md)))))

(alias
 (name runtest)
 (action (diff lwt/test-case.md lwt.corrected)))

(rule
 (target mlt.corrected)
 (deps (package mdx) (source_tree mlt))
 (action
  (with-stdout-to %{target}
   (chdir mlt
    (run ocaml-mdx test --output - test-case.md)))))

(alias
 (name runtest)
 (action (diff mlt/test-case.md mlt.corrected)))

(rule
 (target module-alias.corrected)
 (deps (package mdx) (source_tree module-alias))
 (action
  (with-stdout-to %{target}
   (chdir module-alias
    (run ocaml-mdx test --output - test-case.md)))))

(alias
 (name runtest)
 (action (diff module-alias/test-case.md module-alias.corrected)))

(rule
 (target multilines.corrected)
 (deps (package mdx) (source_tree multilines))
 (action
  (with-stdout-to %{target}
   (chdir multilines
    (run ocaml-mdx test --output - test-case.md)))))

(alias
 (name runtest)
 (action (diff multilines/test-case.md multilines.corrected)))

(rule
 (target non-det.corrected)
 (deps (package mdx) (source_tree non-det))
 (action
  (with-stdout-to %{target}
   (chdir non-det
    (run ocaml-mdx test --output - test-case.md)))))

(alias
 (name runtest)
 (action (diff non-det/test-case.md non-det.corrected)))

(rule
 (target ocaml-408-syntax.corrected)
 (deps (package mdx) (source_tree ocaml-408-syntax))
 (action
  (with-stdout-to %{target}
   (chdir ocaml-408-syntax
    (run ocaml-mdx test --output - test-case.md)))))

(alias
 (name runtest)
 (action (diff ocaml-408-syntax/test-case.md ocaml-408-syntax.corrected)))

(rule
 (target padding.corrected)
 (deps (package mdx) (source_tree padding))
 (action
  (with-stdout-to %{target}
   (chdir padding
    (run ocaml-mdx test --output - test-case.md)))))

(alias
 (name runtest)
 (action (diff padding/test-case.md padding.corrected)))

(rule
 (target prelude.corrected)
 (deps (package mdx) (source_tree prelude))
 (action
  (with-stdout-to %{target}
   (chdir prelude
    (run ocaml-mdx test --output - --prelude-str "#require \"lwt\"" --prelude-str "toto:let x = \"42\"" test-case.md)))))

(alias
 (name runtest)
 (action (diff prelude/test-case.md prelude.corrected)))

(rule
 (target prelude-file.corrected)
 (deps (package mdx) (source_tree prelude-file))
 (action
  (with-stdout-to %{target}
   (chdir prelude-file
    (run ocaml-mdx test --output - --prelude prelude.ml test-case.md)))))

(alias
 (name runtest)
 (action (diff prelude-file/test-case.md prelude-file.corrected)))

(rule
 (target recursive-compile.corrected)
 (deps (package mdx) (source_tree recursive-compile))
 (action
  (with-stdout-to %{target}
   (chdir recursive-compile
    (run ocaml-mdx test --output - test-case.md)))))

(alias
 (name runtest)
 (action (diff recursive-compile/test-case.md recursive-compile.corrected)))

(rule
 (target root-option.corrected)
 (deps (package mdx) (source_tree root-option))
 (action
  (with-stdout-to %{target}
   (chdir root-option
    (run ocaml-mdx test --output - --root=somedir test-case.md)))))

(alias
 (name runtest)
 (action (diff root-option/test-case.md.expected root-option.corrected)))

(rule
 (target section.corrected)
 (deps (package mdx) (source_tree section))
 (action
  (with-stdout-to %{target}
   (chdir section
    (run ocaml-mdx test --output - -s Testing test-case.md)))))

(alias
 (name runtest)
 (action (diff section/test-case.md.expected section.corrected)))

(rule
 (target semisemi.corrected)
 (deps (package mdx) (source_tree semisemi))
 (action
  (with-stdout-to %{target}
   (chdir semisemi
    (run ocaml-mdx test --output - test-case.md)))))

(alias
 (name runtest)
 (action (diff semisemi/test-case.md semisemi.corrected)))

(rule
 (target spaces.corrected)
 (deps (package mdx) (source_tree spaces))
 (action
  (with-stdout-to %{target}
   (chdir spaces
    (run ocaml-mdx test --output - test-case.md)))))

(alias
 (name runtest)
 (action (diff spaces/test-case.md spaces.corrected)))

(rule
 (target sync-to-md.corrected)
 (deps (package mdx) (source_tree sync-to-md))
 (action
  (with-stdout-to %{target}
   (chdir sync-to-md
    (run ocaml-mdx test --output - --direction=to-md test-case.md)))))

(alias
 (name runtest)
 (action (diff sync-to-md/test-case.md.expected sync-to-md.corrected)))

(rule
 (target trailing-whitespaces.corrected)
 (deps (package mdx) (source_tree trailing-whitespaces))
 (action
  (with-stdout-to %{target}
   (chdir trailing-whitespaces
    (run ocaml-mdx test --output - test-case.md)))))

(alias
 (name runtest)
 (action (diff trailing-whitespaces/test-case.md.expected trailing-whitespaces.corrected)))
