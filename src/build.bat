del sqlgg.exe
git describe | ocaml make_version.ml > git.ml
ocamlbuild -classic-display -no-links sqlgg.byte
copy _build\sqlgg.byte sqlgg.exe
