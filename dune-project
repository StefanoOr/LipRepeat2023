(lang dune 3.4)

(name lip22)

(generate_opam_files true)

(source
 (github username/reponame))

(authors "Orrù Stefano, Curreli Stefano, Solinas Gianluca, Pisano Leonardo")

(maintainers "Maintainer Name")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name lip22)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends ocaml dune)
 (tags
  (topics "to describe" your project)))

(using menhir 2.1)
; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
