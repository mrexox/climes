CONTRIB_PATH := $(HOME)/.config/common-lisp/source-registry.conf.d
HERE         := $(shell echo $(CURDIR) | sed -e 's|$(HOME)/||')

dev_preprocess:
	sed -e 's|%CLIMES_PATH%|$(CURDIR)|'        < bin/climes.in             > /tmp/climes.sh
	sed -e 's|%PATH%|$(HERE)|'                 < contrib/50-climes.conf.in > /tmp/50-climes.conf

dev_install: dev_preprocess
	install -m 0755 /tmp/climes.sh $(DESTRIR)/usr/local/bin/climes
	install -m 0644 /tmp/50-climes.conf $(CONTRIB_PATH)/50-climes.conf

.PHONY: dev_install dev_preprocess
