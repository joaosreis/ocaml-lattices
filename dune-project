(lang dune 2.8)

(name lattices)

(version dev)

(generate_opam_files true)

(source
 (uri "git+https://github.com/joaosreis/ocaml-lattices.git"))

(bug_reports "https://github.com/joaosreis/ocaml-lattices/issues")

(homepage "https://github.com/joaosreis/ocaml-lattices")

(license MIT)

(authors "João Santos Reis <joaosreis@outlook.pt>")

(maintainers "João Santos Reis <joaosreis@outlook.pt>")

(package
 (name lattices)
 (synopsis "A library with various implementations and functors of lattices")
 (depends
  (dune :build)
  (containers
   (and
    (>= 3.0)
    (< 4.0)))
  (zarith
   (and
    (>= 1.0)
    (< 2.0)))
  ppx_deriving
  ppx_string
  ppx_inline_test
  (qcheck :with-test)
  (qcheck-alcotest :with-test)
  (alcotest :with-test)))
