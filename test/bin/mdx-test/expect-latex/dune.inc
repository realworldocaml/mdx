
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
