CONTRIB_PATH := $(HOME)/.config/common-lisp/source-registry.conf.d
HERE := $(shell echo $(CURDIR) | sed -e 's|$(HOME)/||')

install:
	mkdir -p $(CONTRIB_PATH)
	echo $(HERE)
	sed -e 's|%PATH%|$(HERE)|' < contrib/50-climes.conf.in > /tmp/50-climes.conf
	cp /tmp/50-climes.conf $(CONTRIB_PATH)
