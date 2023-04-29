# Makefile,v
# Copyright (c) INRIA 2007-2017

WD=$(shell pwd)
TOP=..
include $(TOP)/config/Makefile.sub

DESTDIR=

PACKAGES := $(PACKAGES),pa_ppx.base,fmt,bos
export IMPORT_OCAMLCFLAGS += -ppopt -pa_import-I -ppopt . -ppopt -pa_passthru-debug -package pa_ppx.import,pa_ppx_migrate
TARGET_PACKAGE=pa_ppx_migrate_ocaml_migrate_parsetree
TARGET=$(TARGET_PACKAGE).cma

ML=all_ast.ml reorg_ast.ml \
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
	migrations.ml

MLI=\
	migrate_402_403.mli migrate_403_402.mli \
	migrate_402_404.mli migrate_404_402.mli \
	migrate_402_405.mli migrate_405_402.mli \
	migrate_402_406.mli migrate_406_402.mli \
	migrate_402_407.mli migrate_407_402.mli \
	migrate_403_404.mli migrate_404_403.mli \
	migrate_404_405.mli migrate_405_404.mli \
	migrate_405_406.mli migrate_406_405.mli \
	migrate_406_407.mli migrate_407_406.mli \
	migrate_407_408.mli migrate_408_407.mli \
	migrate_408_409.mli migrate_409_408.mli \
	migrate_409_410.mli migrate_410_409.mli \
	migrate_410_411.mli migrate_411_410.mli \
	migrate_411_412.mli migrate_412_411.mli \
	migrate_412_413.mli migrate_413_412.mli \
	migrate_413_414.mli migrate_414_413.mli \
	migrate_414_500.mli migrate_500_414.mli \

TOOLSML = mk_meta.ml
ALLSRC=$(ML) $(MLI) $(TOOLSML)
ALLPACKAGES=$(PACKAGES)

all: $(ARCHIVE_TARGET_INSTALL_FILES) META
	$(MAKE) local-install

doc: $(CMT) $(CMTI)

TARGET_INSTALL_FILES=$(ARCHIVE_TARGET_INSTALL_FILES) $(FILES_TARGET_INSTALL_FILES)
install:: default_install

realclean:: clean
	rm -f all_ast.ml

local-install::
	$(MAKE) DESTDIR=$(WD)/$(TOP)/local-install/ install

all_ast.ml: all_ast.ORIG.ml
	cppo $< > all_ast.ml.NEW && mv all_ast.ml.NEW all_ast.ml

$(TARGET): $(CMO)
	$(OCAMLFIND) ocamlc $(DEBUG) $^ -a -o $(TARGET)

$(TARGET:.cma=.cmxa) $(TARGET:.cma=.a): $(CMX)
	$(OCAMLFIND) ocamlopt $(DEBUG) $^ -a -o $(TARGET:.cma=.cmxa)

