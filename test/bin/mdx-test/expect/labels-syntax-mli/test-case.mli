(** We are testing the label syntax in [mli] files.

    {1 Set environment variables}

    Environment variables can be loaded in an ocaml block environment.

    {@ocaml set-FOO=bar,set-BAR=foo[
    # print_endline (Sys.getenv "FOO");;
    bar
    - : unit = ()
    # print_endline (Sys.getenv "BAR");;
    foo
    - : unit = ()
    ]}

    {3 Non-deterministic Outputs}

    {@sh non-deterministic=output[
    $ echo $RANDOM
    4150
    ]}

    {3 Non-deterministic Commands}

    {@sh non-deterministic=command[
    $ touch toto
    ]}
*)
