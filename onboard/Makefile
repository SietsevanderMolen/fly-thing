UNAME_M := $(shell uname -m)
ifneq ($(UNAME_M),armv71)
	dockerstring = docker run -w /source --rm -i -t -v $(shell pwd):/source ada-gnat
endif

all:
	$(dockerstring) gprbuild -p -P /source/ravn.gpr

ctags:
	$(dockerstring) ctags -R --exclude=./src/i2c/i2c_interface/ .

clean:
	$(dockerstring) gprclean ravn.gpr
	$(dockerstring) rm -rf ./tags

.PHONY: all clean
