(lang dune 3.7)

(name eopl)

(using menhir 2.1)

(generate_opam_files true)

(source
 (github xsong1985/reponame))

(authors "Xiang Song")

(maintainers "Xiang Song")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name eopl)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends ocaml dune)
 (tags
  (topics "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
