(alias
 (name   runtest)
 (deps   (:x dune_rules.md)
         (:y1 dune_rules_2.ml)
         (:y0 dune_rules_1.ml)
         (package mdx))
 (action (progn
           (run mdx test --direction=to-ml %{x})
           (diff? %{x} %{x}.corrected)
           (diff? %{y1} %{y1}.corrected)
           (diff? %{y0} %{y0}.corrected))))
