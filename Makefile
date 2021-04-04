GLOBAL_CONTRIB_PATH := /etc/common-lisp/source-registry.conf.d/
LOCAL_CONTRIB_PATH  := $(HOME)/.config/common-lisp/source-registry.conf.d
HERE                := $(shell echo $(CURDIR) | sed -e 's|$(HOME)/||')

LIB_PATH            ?= /usr/local/lib/climes
BIN_PATH            ?= /usr/local/bin

default: preprocess

install: CONTRIB_PATH=$(GLOBAL_CONTRIB_PATH)
install: preprocess _install
	mkdir -p /usr/local/lib/climes
	cp -r * /usr/local/lib/climes

dev_install: CONTRIB_PATH=$(LOCAL_CONTRIB_PATH)
dev_install: LIB_PATH=$(CURDIR)
dev_install: dev_preprocess _install

_install:
	mkdir -p $(CONTRIB_PATH)
	install -m 0755 /tmp/climes.sh $(DESTRIR)/$(BIN_PATH)/climes
	install -m 0644 /tmp/50-climes.conf $(CONTRIB_PATH)/50-climes.conf

preprocess:
	sed -e 's|%CLIMES_PATH%|$(LIB_PATH)|'        < bin/climes.in             > /tmp/climes.sh
	cp contrib/50-climes.conf /tmp/50-climes.conf

dev_preprocess:
	sed -e 's|%CLIMES_PATH%|$(CURDIR)|'        < bin/climes.in             > /tmp/climes.sh
	sed -e 's|%PATH%|$(HERE)|'                 < contrib/50-climes-dev.conf.in > /tmp/50-climes.conf

.PHONY: _install install preprocess dev_install dev_preprocess
