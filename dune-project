(lang dune 3.10)

(name aback)
(version 5312b7f-dirty)

(generate_opam_files true)

(source
 (github jkenda/aback))

(authors "Jakob Kenda")
(maintainers "Jakob Kenda")

(license MIT)

(package
 (name aback)
 (synopsis "A programming language")
 (description "Aback is a programming language that uses polish notation, which can be reversed")
 (depends ocaml dune ppx_inline_test ppx_deriving)
 (tags
  (topics "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
