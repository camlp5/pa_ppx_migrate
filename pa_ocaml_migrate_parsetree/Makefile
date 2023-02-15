# Makefile,v
# Copyright (c) INRIA 2007-2017

WD=$(shell pwd)
TOP=..
include $(TOP)/config/Makefile.sub

DESTDIR=

PACKAGES := $(PACKAGES),pa_ppx.base,fmt,bos,pa_ppx.import
PACKAGES := $(PACKAGES),pa_ppx_migrate
export IMPORT_OCAMLCFLAGS += -ppopt -pa_import-I -ppopt . -ppopt -pa_passthru-debug 

TARGET=pa_omp.cma
ML=all_ast.ml \
	migrate_402_403.ml migrate_403_402.ml \
	migrate_402_404.ml migrate_404_402.ml \
	migrate_402_405.ml migrate_405_402.ml \
	migrate_402_406.ml migrate_406_402.ml \
	migrate_402_407.ml migrate_407_402.ml \
	migrate_403_404.ml migrate_404_403.ml \
	migrate_404_405.ml migrate_405_404.ml \
	migrate_405_406.ml migrate_406_405.ml \
	migrate_406_407.ml migrate_407_406.ml \
	migrate_407_408.ml migrate_408_407.ml \
	migrate_408_409.ml migrate_409_408.ml \
	migrate_409_410.ml migrate_410_409.ml \
	migrate_410_411.ml migrate_411_410.ml \
	migrate_411_412.ml migrate_412_411.ml \
	migrate_412_413.ml migrate_413_412.ml \
	migrate_413_414.ml migrate_414_413.ml \
	migrate_414_500.ml migrate_500_414.ml \

ALLSRC=$(ML)
ALLPACKAGES=$(PACKAGES)


test all: $(TARGET) $(TARGET:.cma=.cmxa) local-install

doc: $(CMT) $(CMTI)

all_ast.ml: all_ast.ORIG.ml $(wildcard gen/ast_*.ml) 
	cppo all_ast.ORIG.ml > all_ast.ml.NEW && mv all_ast.ml.NEW all_ast.ml

install:: META
	mkdir -p $(DESTDIR)/lib
	$(NOT_OCAMLFIND) reinstall-if-diff pa_ppx_ocaml_migrate_parsetree -destdir $(DESTDIR)/lib META $(TARGET) $(TARGET:.cma=.cmxa) $(TARGET:.cma=.a) $(CMI) $(wildcard *.cmt*)

local-install:: $(TARGET) $(TARGET:.cma=.cmxa)
	$(MAKE) DESTDIR=$(WD)/$(TOP)/local-install/ install

clean::
	rm -rf META gen/*.cm* gen/*.o all_ast.ml

$(TARGET): $(CMO)
	$(OCAMLFIND) ocamlc $(DEBUG) $(CMO) -a -o $(TARGET)

$(TARGET:.cma=.cmxa): $(CMO:.cmo=.cmx)
	$(OCAMLFIND) ocamlopt $(DEBUG) $(CMO:.cmo=.cmx) -a -o $(TARGET:.cma=.cmxa)

$(TARGET): $(CMO)
$(TARGET:.cma=.cmxa): $(CMO:.cmo=.cmx)
