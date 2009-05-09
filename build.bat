del sqlgg.exe
git rev-parse HEAD | ocaml make_version.ml > git.ml
ocamlbuild -classic-display -no-links sqlgg.byte
copy _build\sqlgg.byte sqlgg.exe
