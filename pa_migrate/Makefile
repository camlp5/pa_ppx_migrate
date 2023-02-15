# Makefile,v
# Copyright (c) INRIA 2007-2017

WD=$(shell pwd)
TOP=..
include $(TOP)/config/Makefile.sub

DESTDIR=

PACKAGES := $(PACKAGES),pa_ppx.deriving,pa_ppx.base,pa_ppx.unmatched_vala,pa_ppx.deriving_plugins.params,pa_ppx.params_runtime

ML=\
	pa_deriving_migrate.ml \

TOOLSML = mk_meta.ml
ALLSRC=$(ML) $(TOOLSML)
ALLPACKAGES=$(PACKAGES)

all: $(CMO) $(CMX)
	$(MAKE) local-install

doc: $(CMT) $(CMTI)

KITS=$(CMO)

install:: META
	mkdir -p $(DESTDIR)/lib
	$(NOT_OCAMLFIND) reinstall-if-diff pa_ppx_migrate -destdir $(DESTDIR)/lib META $(CMO) $(CMI) $(CMX) $(MLI:.mli=.cmi) $(KITS:.cmo=.o) $(wildcard *.cmt*)

local-install::
	$(MAKE) DESTDIR=$(WD)/$(TOP)/local-install/ install

clean::
	rm -rf META mk_meta
