PACKAGES = core
SYNTAX_EXTS = sexplib
MODULES= linear diff_logic std

B=solvers
T=$B.cmxa

FOR_PACK_OPT = -for-pack Solvers
SYNTAX_EXT_OPT = -syntax campl4o

### BEGIN BOILERPLATE ##########################################################

COMPILE_FLAGS=-w YSPUZF -warn-error YSPUZ

OCAMLOPT=ocamlfind ocamlopt
OCAMLDEP=ocamlfind ocamldep
OCAMLYACC=ocamlfind ocamlyacc
OCAMLLEX=ocamlfind ocamllex -q

all: init main.exe
	@echo done

init:
	eval `opam config -env`

OBJECTS=$(addsuffix .cmx, $(MODULES))
SYNTAX_EXT_PACKAGES=$(addsuffix .syntax, $(SYNTAX_EXTS))

$B.cmx: $(OBJECTS)
	$(OCAMLOPT) -pack $(OBJECTS) -o $B.cmx

$B.cmo: $(OBJECTS)
	$(OCAMLC) -pack $(OBJECTS) -o $B.cmo

std.cmx: std.ml
	$(OCAMLOPT) $(FOR_PACK_OPT) -c std.ml

%.exe: $(OBJECTS)
	$(OCAMLOPT) -thread -package $(PACKAGES) -linkpkg $(OBJECTS) -o $*.exe

%.cma: %.cmo
	$(OCAMLC) -thread -a $*.cmo -o $*.cma

%.cmxa: %.cmx
	$(OCAMLOPT) -a $*.cmx -o $*.cmxa

%_intf.mli: %_intf.ml
	ln -sf $*_intf.ml $*_intf.mli 

%.cmi: %.mli
	$(OCAMLOPT) -thread $(SYNTAX_EXT_OPT) -package $(SYNTAX_EXT_PACKAGES) -package $(PACKAGES) -linkpkg $(COMPILE_FLAGS) -c $*.mli

%.cmx: %.cmi %.ml
	$(OCAMLOPT) -thread $(SYNTAX_EXT_OPT) -package $(SYNTAX_EXT_PACKAGES) -package $(PACKAGES) -linkpkg $(COMPILE_FLAGS) -c $*.ml

%.ml %.mli: %.mly
	$(OCAMLYACC) $*.mly

%.ml: %.mli %.mll
	$(OCAMLLEX) $*.mll

.depend: $(SRC)
	$(OCAMLDEP) *.mli *.ml > .depend

clean:
	rm -rf .depend *.o *.a *.cmi *.cmo *.cma *.cmx *.cmxa $(SRC) $T

include .depend

