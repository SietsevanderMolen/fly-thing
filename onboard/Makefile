all:
	docker run -w /source --rm -i -t -v $(shell pwd):/source ada-gnat gprbuild -p -P /source/ravn.gpr

native:
	gprbuild -p -P ravn.gpr

ctags:
	docker run -w /source --rm -i -t -v $(shell pwd):/source ada-gnat ctags -R --exclude=./src/i2c/i2c_interface/ .

clean:
	docker run -w /source --rm -i -t -v $(shell pwd):/source ada-gnat gprclean ravn.gpr
	docker run -w /source --rm -i -t -v $(shell pwd):/source ada-gnat rm -rf ./tags

.PHONY: all clean native
