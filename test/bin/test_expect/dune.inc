
(alias
  (name runtest)
  (deps (:x dir1.md) (:y dir1.md.expected) (package mdx))
  (action
    (progn
      (run ocaml-mdx test --force-output --root "../../../bin" %{x})
      (diff? %{y} %{x}.corrected))
  )
)

(alias
  (name runtest)
  (deps (:x ellipsis_updates.md) (:y ellipsis_updates.md.expected) (package mdx))
  (action
    (progn
      (run ocaml-mdx test --force-output  %{x})
      (diff? %{y} %{x}.corrected))
  )
)

(alias
  (name runtest)
  (deps (:x empty_lines.md) (:y empty_lines.md.expected) (package mdx))
  (action
    (progn
      (run ocaml-mdx test --force-output  %{x})
      (diff? %{y} %{x}.corrected))
  )
)

(alias
  (name runtest)
  (deps (:x labels.md) (:y labels.md.expected) (package mdx))
  (action
    (progn
      (run ocaml-mdx test --force-output  %{x})
      (diff? %{y} %{x}.corrected))
  )
)

(alias
  (name runtest)
  (deps (:x section.md) (:y section.md.expected) (package mdx))
  (action
    (progn
      (run ocaml-mdx test --force-output -s Testing %{x})
      (diff? %{y} %{x}.corrected))
  )
)

(alias
  (name runtest)
  (deps (:x sync_to_md.md) (:y sync_to_md.md.expected) (package mdx))
  (action
    (progn
      (run ocaml-mdx test --force-output --direction=to-md %{x})
      (diff? %{y} %{x}.corrected))
  )
)

(alias
  (name runtest)
  (deps (:x trailing.md) (:y trailing.md.expected) (package mdx))
  (action
    (progn
      (run ocaml-mdx test --force-output  %{x})
      (diff? %{y} %{x}.corrected))
  )
)
