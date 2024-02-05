(lang dune 3.0)
(name aktsp2024)

(source (github sws-lab/aktsp2024))
(authors "Simmo Saan")
(maintainers "Simmo Saan")

(generate_opam_files true)

(package
 (name aktsp2024)
 (synopsis "AKT süvenduspraktikumi 2024 materjalid")
 (depends
  ocamlgraph
  ppx_deriving
  ppx_deriving_hash
  ounit2
  qcheck-ounit
  ocaml-lsp-server
  utop)
 (allow_empty))
