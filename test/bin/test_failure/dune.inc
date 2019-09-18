
(rule
  (targets both_prelude.md.actual)
  (action
    (with-outputs-to %{targets}
      (system "! %{bin:ocaml-mdx} test --prelude=a --prelude-str=b %{dep:both_prelude.md}"))))

(alias
  (name runtest)
  (action
    (diff both_prelude.md.expected both_prelude.md.actual)))

(rule
  (targets in_toplevel.md.actual)
  (action
    (with-outputs-to %{targets}
      (system "! %{bin:ocaml-mdx} test  %{dep:in_toplevel.md}"))))

(alias
  (name runtest)
  (action
    (diff in_toplevel.md.expected in_toplevel.md.actual)))

(rule
  (targets ml_file_not_found.md.actual)
  (action
    (with-outputs-to %{targets}
      (system "! %{bin:ocaml-mdx} test  %{dep:ml_file_not_found.md}"))))

(alias
  (name runtest)
  (action
    (diff ml_file_not_found.md.expected ml_file_not_found.md.actual)))

(rule
  (targets part_not_found.md.actual)
  (action
    (with-outputs-to %{targets}
      (system "! %{bin:ocaml-mdx} test --direction=to-md %{dep:part_not_found.md}"))))

(alias
  (name runtest)
  (action
    (diff part_not_found.md.expected part_not_found.md.actual)))
