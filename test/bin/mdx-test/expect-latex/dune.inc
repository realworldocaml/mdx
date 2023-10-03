
(rule
 (target bash-fence.actual)
 (deps (package mdx) (source_tree bash-fence))
 (action
  (with-stdout-to %{target}
   (chdir bash-fence
    (run ocaml-mdx test --output - test-case.tex)))))

(rule
 (alias runtest)
 (action (diff bash-fence/test-case.tex.expected bash-fence.actual)))

(rule
 (target casual-file-inc.actual)
 (deps (package mdx) (source_tree casual-file-inc))
 (action
  (with-stdout-to %{target}
   (chdir casual-file-inc
    (run ocaml-mdx test --output - test-case.tex)))))

(rule
 (alias runtest)
 (action (diff casual-file-inc/test-case.tex.expected casual-file-inc.actual)))

(rule
 (target code.actual)
 (deps (package mdx) (source_tree code))
 (action
  (with-stdout-to %{target}
   (chdir code
    (run ocaml-mdx test --output - test-case.tex)))))

(rule
 (alias runtest)
 (action (diff code/test-case.tex code.actual)))

(rule
 (target compenv-exit.actual)
 (deps (package mdx) (source_tree compenv-exit))
 (action
  (with-stdout-to %{target}
   (chdir compenv-exit
    (run ocaml-mdx test --output - test-case.tex)))))

(rule
 (alias runtest)
 (action (diff compenv-exit/test-case.tex.expected compenv-exit.actual)))

(rule
 (target ellipsis.actual)
 (deps (package mdx) (source_tree ellipsis))
 (action
  (with-stdout-to %{target}
   (chdir ellipsis
    (run ocaml-mdx test --output - test-case.tex)))))

(rule
 (alias runtest)
 (action (diff ellipsis/test-case.tex ellipsis.actual)))

(rule
 (target ellipsis-updates.actual)
 (deps (package mdx) (source_tree ellipsis-updates))
 (action
  (with-stdout-to %{target}
   (chdir ellipsis-updates
    (run ocaml-mdx test --output - test-case.tex)))))

(rule
 (alias runtest)
 (action (diff ellipsis-updates/test-case.tex.expected ellipsis-updates.actual)))

(rule
 (target empty.actual)
 (deps (package mdx) (source_tree empty))
 (action
  (with-stdout-to %{target}
   (chdir empty
    (run ocaml-mdx test --output - test-case.tex)))))

(rule
 (alias runtest)
 (action (diff empty/test-case.tex empty.actual)))

(rule
 (target empty-line.actual)
 (deps (package mdx) (source_tree empty-line))
 (action
  (with-stdout-to %{target}
   (chdir empty-line
    (run ocaml-mdx test --output - test-case.tex)))))

(rule
 (alias runtest)
 (action (diff empty-line/test-case.tex empty-line.actual)))

(rule
 (target empty-lines.actual)
 (deps (package mdx) (source_tree empty-lines))
 (action
  (with-stdout-to %{target}
   (chdir empty-lines
    (run ocaml-mdx test --output - test-case.tex)))))

(rule
 (alias runtest)
 (action (diff empty-lines/test-case.tex.expected empty-lines.actual)))

(rule
 (target env-and-prelude.actual)
 (deps (package mdx) (source_tree env-and-prelude))
 (action
  (with-stdout-to %{target}
   (chdir env-and-prelude
    (run ocaml-mdx test --output - --prelude all.ml --prelude " :default.ml" --prelude a:a.ml --prelude b:b.ml test-case.tex)))))

(rule
 (alias runtest)
 (action (diff env-and-prelude/test-case.tex env-and-prelude.actual)))

(rule
 (target environment-variable.actual)
 (deps (package mdx) (source_tree environment-variable))
 (action
  (with-stdout-to %{target}
   (chdir environment-variable
    (run ocaml-mdx test --output - test-case.tex)))))

(rule
 (alias runtest)
 (action (diff environment-variable/test-case.tex environment-variable.actual)))

(rule
 (target environment-variable-set.actual)
 (deps (package mdx) (source_tree environment-variable-set))
 (action
  (with-stdout-to %{target}
   (chdir environment-variable-set
    (run ocaml-mdx test --output - test-case.tex)))))

(rule
 (alias runtest)
 (action (diff environment-variable-set/test-case.tex environment-variable-set.actual)))

(rule
 (target envs.actual)
 (deps (package mdx) (source_tree envs))
 (action
  (with-stdout-to %{target}
   (chdir envs
    (run ocaml-mdx test --output - test-case.tex)))))

(rule
 (alias runtest)
 (action (diff envs/test-case.tex envs.actual)))

(rule
 (target errors.actual)
 (deps (package mdx) (source_tree errors))
 (action
  (with-stdout-to %{target}
   (chdir errors
    (run ocaml-mdx test --output - test-case.tex)))))

(rule
 (alias runtest)
 (action (diff errors/test-case.tex errors.actual)))

(rule
 (target exit.actual)
 (deps (package mdx) (source_tree exit))
 (action
  (with-stdout-to %{target}
   (chdir exit
    (run ocaml-mdx test --output - test-case.tex)))))

(rule
 (alias runtest)
 (action (diff exit/test-case.tex exit.actual)))

(rule
 (target heredoc.actual)
 (deps (package mdx) (source_tree heredoc))
 (action
  (with-stdout-to %{target}
   (chdir heredoc
    (run ocaml-mdx test --output - test-case.tex)))))

(rule
 (alias runtest)
 (action (diff heredoc/test-case.tex heredoc.actual)))

(rule
 (target include-block.actual)
 (deps (package mdx) (source_tree include-block))
 (action
  (with-stdout-to %{target}
   (chdir include-block
    (run ocaml-mdx test --output - test-case.tex)))))

(rule
 (alias runtest)
 (action (diff include-block/test-case.tex.expected include-block.actual)))

(rule
 (target labels-syntax.actual)
 (deps (package mdx) (source_tree labels-syntax))
 (action
  (with-stdout-to %{target}
   (chdir labels-syntax
    (run ocaml-mdx test --output - test-case.tex)))))

(rule
 (alias runtest)
 (action (diff labels-syntax/test-case.tex labels-syntax.actual)))

(rule
 (target line-delimiters.actual)
 (deps (package mdx) (source_tree line-delimiters))
 (action
  (with-stdout-to %{target}
   (chdir line-delimiters
    (run ocaml-mdx test --output - test-case.tex)))))

(rule
 (alias runtest)
 (action (diff line-delimiters/test-case.tex line-delimiters.actual)))

(rule
 (target lines.actual)
 (deps (package mdx) (source_tree lines))
 (action
  (with-stdout-to %{target}
   (chdir lines
    (run ocaml-mdx test --output - test-case.tex)))))

(rule
 (alias runtest)
 (action (diff lines/test-case.tex lines.actual)))

(rule
 (target lwt.actual)
 (deps (package mdx) (source_tree lwt))
 (action
  (with-stdout-to %{target}
   (chdir lwt
    (run ocaml-mdx test --output - test-case.tex)))))

(rule
 (alias runtest)
 (action (diff lwt/test-case.tex lwt.actual)))

(rule
 (target mlt.actual)
 (deps (package mdx) (source_tree mlt))
 (action
  (with-stdout-to %{target}
   (chdir mlt
    (run ocaml-mdx test --output - test-case.tex)))))

(rule
 (alias runtest)
 (action (diff mlt/test-case.tex mlt.actual)))

(rule
 (target multilines.actual)
 (deps (package mdx) (source_tree multilines))
 (action
  (with-stdout-to %{target}
   (chdir multilines
    (run ocaml-mdx test --output - test-case.tex)))))

(rule
 (alias runtest)
 (action (diff multilines/test-case.tex multilines.actual)))

(rule
 (target non-det.actual)
 (deps (package mdx) (source_tree non-det))
 (action
  (with-stdout-to %{target}
   (chdir non-det
    (run ocaml-mdx test --output - test-case.tex)))))

(rule
 (alias runtest)
 (action (diff non-det/test-case.tex non-det.actual)))

(rule
 (target non-det-default-preserved.actual)
 (deps (package mdx) (source_tree non-det-default-preserved))
 (action
  (with-stdout-to %{target}
   (chdir non-det-default-preserved
    (run ocaml-mdx test --output - test-case.tex)))))

(rule
 (alias runtest)
 (action (diff non-det-default-preserved/test-case.tex non-det-default-preserved.actual)))

(rule
 (target ocaml-408-syntax.actual)
 (deps (package mdx) (source_tree ocaml-408-syntax))
 (action
  (with-stdout-to %{target}
   (chdir ocaml-408-syntax
    (run ocaml-mdx test --output - test-case.tex)))))

(rule
 (alias runtest)
 (action (diff ocaml-408-syntax/test-case.tex ocaml-408-syntax.actual)))

(rule
 (target ocaml-errors.actual)
 (deps (package mdx) (source_tree ocaml-errors))
 (action
  (with-stdout-to %{target}
   (chdir ocaml-errors
    (run ocaml-mdx test --output - test-case.tex)))))

(rule
 (alias runtest)
 (action (diff ocaml-errors/test-case.tex.expected ocaml-errors.actual)))

(rule
 (target ocaml-errors-ellipsis.actual)
 (deps (package mdx) (source_tree ocaml-errors-ellipsis))
 (action
  (with-stdout-to %{target}
   (chdir ocaml-errors-ellipsis
    (run ocaml-mdx test --output - test-case.tex)))))

(rule
 (alias runtest)
 (action (diff ocaml-errors-ellipsis/test-case.tex ocaml-errors-ellipsis.actual)))

(rule
 (target padding.actual)
 (deps (package mdx) (source_tree padding))
 (action
  (with-stdout-to %{target}
   (chdir padding
    (run ocaml-mdx test --output - test-case.tex)))))

(rule
 (alias runtest)
 (action (diff padding/test-case.tex padding.actual)))

(rule
 (target parts-begin-end.actual)
 (deps (package mdx) (source_tree parts-begin-end))
 (action
  (with-stdout-to %{target}
   (chdir parts-begin-end
    (run ocaml-mdx test --output - test-case.tex)))))

(rule
 (alias runtest)
 (action (diff parts-begin-end/test-case.tex.expected parts-begin-end.actual)))

(rule
 (target prelude.actual)
 (deps (package mdx) (source_tree prelude))
 (action
  (with-stdout-to %{target}
   (chdir prelude
    (run ocaml-mdx test --output - --prelude-str "#require \"lwt\"" --prelude-str "toto:let x = \"42\"" test-case.tex)))))

(rule
 (alias runtest)
 (action (diff prelude/test-case.tex prelude.actual)))

(rule
 (target prelude-file.actual)
 (deps (package mdx) (source_tree prelude-file))
 (action
  (with-stdout-to %{target}
   (chdir prelude-file
    (run ocaml-mdx test --output - --prelude prelude.ml test-case.tex)))))

(rule
 (alias runtest)
 (action (diff prelude-file/test-case.tex prelude-file.actual)))

(rule
 (target requires.actual)
 (deps (package mdx) (source_tree requires))
 (action
  (with-stdout-to %{target}
   (chdir requires
    (run ocaml-mdx test --output - test-case.tex)))))

(rule
 (alias runtest)
 (action (diff requires/test-case.tex requires.actual)))

(rule
 (target root-option.actual)
 (deps (package mdx) (source_tree root-option))
 (action
  (with-stdout-to %{target}
   (chdir root-option
    (run ocaml-mdx test --output - --root=somedir test-case.tex)))))

(rule
 (alias runtest)
 (action (diff root-option/test-case.tex root-option.actual)))

(rule
 (target shell-file-inc.actual)
 (deps (package mdx) (source_tree shell-file-inc))
 (action
  (with-stdout-to %{target}
   (chdir shell-file-inc
    (run ocaml-mdx test --output - test-case.tex)))))

(rule
 (alias runtest)
 (action (diff shell-file-inc/test-case.tex.expected shell-file-inc.actual)))

(rule
 (target spaces.actual)
 (deps (package mdx) (source_tree spaces))
 (action
  (with-stdout-to %{target}
   (chdir spaces
    (run ocaml-mdx test --output - test-case.tex)))))

(rule
 (alias runtest)
 (action (diff spaces/test-case.tex spaces.actual)))

(rule
 (target sync-from-subdir.actual)
 (deps (package mdx) (source_tree sync-from-subdir))
 (action
  (with-stdout-to %{target}
   (chdir sync-from-subdir
    (run ocaml-mdx test --output - test-case.tex)))))

(rule
 (alias runtest)
 (action (diff sync-from-subdir/test-case.tex.expected sync-from-subdir.actual)))

(rule
 (target sync-to-md.actual)
 (deps (package mdx) (source_tree sync-to-md))
 (action
  (with-stdout-to %{target}
   (chdir sync-to-md
    (run ocaml-mdx test --output - test-case.tex)))))

(rule
 (alias runtest)
 (action (diff sync-to-md/test-case.tex.expected sync-to-md.actual)))

(rule
 (target tabs.actual)
 (deps (package mdx) (source_tree tabs))
 (action
  (with-stdout-to %{target}
   (chdir tabs
    (run ocaml-mdx test --output - test-case.tex)))))

(rule
 (alias runtest)
 (action (diff tabs/test-case.tex.expected tabs.actual)))

(rule
 (target toploop-getvalue.actual)
 (deps (package mdx) (source_tree toploop-getvalue))
 (action
  (with-stdout-to %{target}
   (chdir toploop-getvalue
    (run ocaml-mdx test --output - test-case.tex)))))

(rule
 (alias runtest)
 (action (diff toploop-getvalue/test-case.tex toploop-getvalue.actual)))

(rule
 (target trailing-whitespaces.actual)
 (deps (package mdx) (source_tree trailing-whitespaces))
 (action
  (with-stdout-to %{target}
   (chdir trailing-whitespaces
    (run ocaml-mdx test --output - test-case.tex)))))

(rule
 (alias runtest)
 (action (diff trailing-whitespaces/test-case.tex.expected trailing-whitespaces.actual)))

(rule
 (target warnings.actual)
 (deps (package mdx) (source_tree warnings))
 (action
  (with-stdout-to %{target}
   (chdir warnings
    (run ocaml-mdx test --output - test-case.tex)))))

(rule
 (alias runtest)
 (action (diff warnings/test-case.tex warnings.actual)))
