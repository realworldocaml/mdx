
(rule
 (target toplevel-blocks.actual)
 (deps (package mdx) (source_tree toplevel-blocks))
 (action
  (with-stdout-to %{target}
   (chdir toplevel-blocks
    (run ocaml-mdx format --output - test-case.md)))))

(rule
 (alias runtest)
 (action (diff toplevel-blocks/test-case.md.expected toplevel-blocks.actual)))
