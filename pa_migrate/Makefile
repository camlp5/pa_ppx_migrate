# Makefile,v
# Copyright (c) INRIA 2007-2017

WD=$(shell pwd)
TOP=..
include $(TOP)/config/Makefile.sub

DESTDIR=

PACKAGES := $(PACKAGES),pa_ppx.deriving,pa_ppx.base,pa_ppx.unmatched_vala,pa_ppx.deriving_plugins.params,pa_ppx.params_runtime
TARGET_PACKAGE=pa_ppx_migrate

ML=\
	pa_deriving_migrate.ml \

TOOLSML = mk_meta.ml
ALLSRC=$(ML) $(TOOLSML)
ALLPACKAGES=$(PACKAGES)

all: $(CMO) $(CMX)
	$(MAKE) local-install

doc: $(CMT) $(CMTI)

KITS=$(CMO)

TARGET_INSTALL_FILES=$(CMO) $(CMI) $(CMX)
install:: default_install

local-install::
	$(MAKE) DESTDIR=$(WD)/$(TOP)/local-install/ install

clean::
	rm -rf META mk_meta
