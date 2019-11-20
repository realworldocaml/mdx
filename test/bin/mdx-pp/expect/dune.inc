
(rule
 (target spaces.actual)
 (deps (package mdx) (source_tree spaces))
 (action
  (with-stdout-to %{target}
   (chdir spaces
    (run ocaml-mdx pp test-case.md)))))

(alias
 (name runtest)
 (action (diff spaces/test_case.ml.expected spaces.actual)))
