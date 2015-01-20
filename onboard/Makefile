all:
	gprbuild -p -P ravn.gpr

ctags:
	ctags -R --exclude=./src/i2c/i2c_interface/ .

clean:
	gprclean ravn.gpr
	-rm -rf ./tags

.PHONY: all clean
