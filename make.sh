eval `opam config -env`

set -x
# clean
rm -rf a.out *.cmi *.cmx *.o

# compile

ocamlfind ocamlopt \
  -thread \
  -syntax camlp4o \
  -package sexplib.syntax,core \
  -linkpkg \
  -w YSPUZF -warn-error YSPUZ \
  -c linear.mli

ocamlfind ocamlopt \
  -thread \
  -syntax camlp4o \
  -package sexplib.syntax,core \
  -linkpkg \
  -w YSPUZF -warn-error YSPUZ \
  -c linear.ml

ocamlfind ocamlopt \
  -thread \
  -syntax camlp4o \
  -package sexplib.syntax,core \
  -linkpkg \
  -w YSPUZF -warn-error YSPUZ \
  -c diff_logic.mli

ocamlfind ocamlopt \
  -thread \
  -syntax camlp4o \
  -package sexplib.syntax,core \
  -linkpkg \
  -w YSPUZF -warn-error YSPUZ \
  -c diff_logic.ml

# link
ocamlfind ocamlopt \
  -thread \
  -package core \
  -linkpkg \
  linear.cmx \
  diff_logic.cmx \
  -o main.exe

