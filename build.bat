del sqlgg.exe
ocamlbuild -classic-display -no-links src\sqlgg.byte
copy _build\src\sqlgg.byte sqlgg.exe
