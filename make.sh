eval `opam config -env`

# parameters

PACKAGES=core
MODULES='linear diff_logic'

# rules

OCAMLOPT='ocamlfind ocamlopt'
OCAMLDEP='ocamlfind ocamldep'

FIND_OPTS=''
FIND_OPTS="$FIND_OPTS -syntax camlp4o"
FIND_OPTS="$FIND_OPTS -package sexplib.syntax -package $PACKAGES"

COMPILE_OPTS="$FIND_OPTS"
COMPILE_OPTS="$COMPILE_OPTS -thread"
COMPILE_OPTS="$COMPILE_OPTS -linkpkg"
COMPILE_OPTS="$COMPILE_OPTS -w YSPUZF"
COMPILE_OPTS="$COMPILE_OPTS -warn-error YSPUZ"

function compile-interface {
  BASE="$1"; shift
  ${OCAMLOPT} ${COMPILE_OPTS} -c $BASE.mli
  if ! [ -f $BASE.cmi ]; then
      echo >&2 failed to compile $BASE.cmi
      exit 1
  fi
}

function compile-implementation {
  ${OCAMLOPT} ${COMPILE_OPTS} -c $BASE.ml
  if ! [ -f $BASE.cmx ]; then
      echo >&2 failed to compile $BASE.cmx
      exit 1
  fi
}

function cmx-files {
  for M in $MODULES; do
    echo $M.cmx
  done
}

function build-exe {
  TGT="$1"; shift
  ${OCAMLOPT} \
    -thread \
    -package $PACKAGES \
    -linkpkg \
    $(cmx-files) \
    -o $TGT.exe
}

set -x

# clean

rm -rf *.exe *.cmi *.cmx *.o

# compile

for M in $MODULES; do
  compile-interface $M
  compile-implementation $M
done

# link

build-exe main

${OCAMLDEP} ${FIND_OPTS} *.mli *.ml

