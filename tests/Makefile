# Makefile,v
# Copyright (c) INRIA 2007-2017

WD=$(shell pwd)
TOP=..
include $(TOP)/config/Makefile.sub

PACKAGEPREFIX=pa_ppx_

DESTDIR=

PACKAGES := $(PACKAGES),oUnit,fmt,rresult,compiler-libs.common,sexplib0,uint,pa_ppx.utils,pa_ppx.testutils,pa_ppx.import,pa_ppx_migrate,camlp5.pa_r.link,pa_ppx.base.link
export IMPORT_OCAMLCFLAGS = -ppopt -pa_import-I -ppopt . -ppopt -pa_passthru-debug \
	-ppopt -pa_import-package -ppopt $(PACKAGES) \

ML=\
	ex_ast.ml ex_migrate.ml test_ex.ml \
	ex_camlp5.ml ex_map_camlp5.ml test_ex_camlp5.ml \

MLI=\
	ex_migrate.mli \

ALLSRC=$(ML) $(MLI)
ALLPACKAGES=$(PACKAGES)

TESTS= \
	test_ex.byte test_ex_camlp5.byte \

all: $(TESTS)

test: $(TESTS:.byte=)

% : %.byte
	mkdir -p _build && ./$<

test_ex.byte: ex_ast.cmo ex_migrate.cmo test_ex.cmo
	$(LAUNCH) $(OCAMLFIND) ocamlc $(DEBUG) $(WARNERR) -package $(PACKAGES) \
	-linkpkg -syntax camlp5o $^ -o $@

test_ex_camlp5.byte: ex_camlp5.cmo ex_map_camlp5.cmo test_ex_camlp5.cmo
	$(LAUNCH) $(OCAMLFIND) ocamlc $(DEBUG) $(WARNERR) -package $(PACKAGES) \
	-linkpkg -syntax camlp5o $^ -o $@

clean::
	rm -f  *.ppo.*
