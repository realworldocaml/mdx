
(rule
 (target both-prelude.corrected)
 (deps (package mdx) both-prelude/test-case.md both-prelude/test-case.opts)
 (action
  (with-outputs-to %{target}
   (chdir both-prelude
    (system "! ocaml-mdx test --prelude=a --prelude-str=b test-case.md")))))

(alias
 (name runtest)
 (action (diff both-prelude/test-case.md.expected both-prelude.corrected)))

(rule
 (target in-toplevel.corrected)
 (deps (package mdx) in-toplevel/test-case.md)
 (action
  (with-outputs-to %{target}
   (chdir in-toplevel
    (system "! ocaml-mdx test test-case.md")))))

(alias
 (name runtest)
 (action (diff in-toplevel/test-case.md.expected in-toplevel.corrected)))

(rule
 (target ml-file-not-found.corrected)
 (deps (package mdx) ml-file-not-found/test-case.md)
 (action
  (with-outputs-to %{target}
   (chdir ml-file-not-found
    (system "! ocaml-mdx test test-case.md")))))

(alias
 (name runtest)
 (action (diff ml-file-not-found/test-case.md.expected ml-file-not-found.corrected)))

(rule
 (target part-not-found.corrected)
 (deps (package mdx) part-not-found/test-case.md part-not-found/test-case.opts)
 (action
  (with-outputs-to %{target}
   (chdir part-not-found
    (system "! ocaml-mdx test --direction=to-md test-case.md")))))

(alias
 (name runtest)
 (action (diff part-not-found/test-case.md.expected part-not-found.corrected)))
