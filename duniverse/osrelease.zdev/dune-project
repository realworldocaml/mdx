(lang dune 2.3)
(name osrelease)
(generate_opam_files true)

(source (github avsm/osrelease))
(license ISC)
(authors "Anil Madhavapeddy <anil@recoil.org>")
(maintainers "anil@recoil.org")

(package
  (name osrelease)
  (synopsis "Detect operating system, distro and version information")
  (description "
This library is used to detect local information about 
which operating system, architecture, distribution and
version the program is running under.")
  (depends
    (ocaml (>= 4.06.0))
    bos
    ppx_sexp_conv
    sexplib
    astring
  ))
