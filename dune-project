(lang dune 3.6)

(name advent-of-code)

(generate_opam_files true)

(source
 (github nordfjord/advent-of-code))

(authors "Einar Nordfjord")

(maintainers "Einar Nordfjord")

(license MIT)

(package
 (name advent-of-code)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends ocaml dune)
 (allow_empty)
 (tags
  (topics "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
