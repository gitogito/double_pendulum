RESULT = a.out
SOURCES = orddiff.ml a.ml
PACKS = extlib
INCDIRS = ~/src/ocaml-mylib
LIBS = mylib
THREADS =
ANNOTATE = yes
OCAMLFLAGS = -bin-annot -w A-44

all: debug-code

export OCAMLMAKEFILE = ~/src/ocamlmakefile/OCamlMakefile
include $(OCAMLMAKEFILE)
