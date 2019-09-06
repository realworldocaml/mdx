(alias
 (name   runtest)
 (deps   (:x pp.md)
         (package mdx))
 (action (progn
           (run ocaml-mdx test --direction=infer-timestamp %{x})

           (diff? %{x} %{x}.corrected))))
(alias
 (name   runtest)
 (deps   (:x ellipsis.md)
         (package mdx))
 (action (progn
           (run ocaml-mdx test --direction=infer-timestamp %{x})

           (diff? %{x} %{x}.corrected))))
(alias
 (name   runtest)
 (deps   (:x envs.md)
         (package mdx))
 (action (progn
           (run ocaml-mdx test --direction=infer-timestamp %{x})

           (diff? %{x} %{x}.corrected))))
(alias
 (name   runtest)
 (deps   (:x empty_line.md)
         (package mdx))
 (action (progn
           (run ocaml-mdx test --direction=infer-timestamp %{x})

           (diff? %{x} %{x}.corrected))))
(alias
 (name   runtest)
 (deps   (:x spaces.md)
         (package mdx))
 (action (progn
           (run ocaml-mdx test --direction=infer-timestamp %{x})

           (diff? %{x} %{x}.corrected))))
(alias
 (name   runtest)
 (deps   (:x cram.t)
         (package mdx))
 (action (progn
           (run ocaml-mdx test --direction=infer-timestamp %{x})

           (diff? %{x} %{x}.corrected))))
(alias
 (name   runtest)
 (deps   (:x errors.md)
         (package mdx))
 (action (progn
           (run ocaml-mdx test --direction=infer-timestamp %{x})

           (diff? %{x} %{x}.corrected))))
(alias
 (name   runtest-all)
 (deps   (:x errors.md)
         (package mdx))
 (action (progn
           (run ocaml-mdx test --direction=infer-timestamp --non-deterministic %{x})

           (diff? %{x} %{x}.corrected))))
(alias
 (name   runtest)
 (deps   (:x heredoc.md)
         (package mdx))
 (action (progn
           (run ocaml-mdx test --direction=infer-timestamp %{x})

           (diff? %{x} %{x}.corrected))))
(alias
 (name   runtest)
 (deps   (:x mlt.md)
         (package mdx))
 (action (progn
           (run ocaml-mdx test --direction=infer-timestamp %{x})

           (diff? %{x} %{x}.corrected))))
(alias
 (name   runtest)
 (deps   (:x semisemi.md)
         (package mdx))
 (action (progn
           (run ocaml-mdx test --direction=infer-timestamp %{x})

           (diff? %{x} %{x}.corrected))))
(alias
 (name   runtest)
 (deps   (:x exit.md)
         (package mdx))
 (action (progn
           (run ocaml-mdx test --direction=infer-timestamp %{x})

           (diff? %{x} %{x}.corrected))))
(alias
 (name   runtest)
 (deps   (:x padding.md)
         (package mdx))
 (action (progn
           (run ocaml-mdx test --direction=infer-timestamp %{x})

           (diff? %{x} %{x}.corrected))))
(alias
 (name   runtest)
 (deps   (:x multilines.md)
         (package mdx))
 (action (progn
           (run ocaml-mdx test --direction=infer-timestamp %{x})

           (diff? %{x} %{x}.corrected))))
(alias
 (name   runtest)
 (deps   (:x lines.md)
         (package mdx))
 (action (progn
           (run ocaml-mdx test --direction=infer-timestamp %{x})

           (diff? %{x} %{x}.corrected))))
(alias
 (name   runtest)
 (deps   (:x lwt.md)
         (package mdx))
 (action (progn
           (run ocaml-mdx test --direction=infer-timestamp %{x})

           (diff? %{x} %{x}.corrected))))
(alias
 (name   runtest)
 (deps   (:x non-det.md)
         (package mdx))
 (action (progn
           (run ocaml-mdx test --direction=infer-timestamp %{x})

           (diff? %{x} %{x}.corrected))))
(alias
 (name   runtest-all)
 (deps   (:x non-det.md)
         (package mdx))
 (action (progn
           (run ocaml-mdx test --direction=infer-timestamp --non-deterministic %{x})

           (diff? %{x} %{x}.corrected))))
(alias
 (name   runtest)
 (deps   (:x code.md)
         (package mdx))
 (action (progn
           (run ocaml-mdx test --direction=infer-timestamp %{x})

           (diff? %{x} %{x}.corrected))))
(alias
 (name   runtest)
 (deps   (:x set_environment_variable.md)
         (package mdx))
 (action (progn
           (run ocaml-mdx test --direction=infer-timestamp %{x})

           (diff? %{x} %{x}.corrected))))
(alias
 (name   runtest)
 (deps   (:x unset_environment_variable.md)
         (package mdx))
 (action (progn
           (run ocaml-mdx test --direction=infer-timestamp %{x})

           (diff? %{x} %{x}.corrected))))
(alias
 (name   runtest)
 (deps   (:x prelude.md)
         (package mdx))
 (action (progn
           (run ocaml-mdx test --prelude-str "#require \"lwt\"" --prelude-str "toto:let x = \"42\"" --direction=infer-timestamp %{x})

           (diff? %{x} %{x}.corrected))))
(alias
 (name   runtest)
 (deps   (:x prelude_file.md)
         (package mdx)
         prelude.ml)
 (action (progn
           (run ocaml-mdx test --prelude=prelude.ml --direction=infer-timestamp %{x})

           (diff? %{x} %{x}.corrected))))
(alias
 (name   runtest)
 (deps   (:x dune_rules.md)
         (package mdx)
         (:y1 dune_rules_1.ml)
         (:y0 dune_rules_2.ml)
         (source_tree foo))
 (action (progn
           (run ocaml-mdx test --direction=to-ml %{x})
           (diff? %{y1} %{y1}.corrected)
           (diff? %{y0} %{y0}.corrected)
           (diff? %{x} %{x}.corrected))))
(alias
 (name   runtest)
 (deps   (:x require/require-package.md)
         (package mdx)
         (package example_lib))
 (action (progn
           (run ocaml-mdx test --direction=infer-timestamp %{x})

           (diff? %{x} %{x}.corrected))))
