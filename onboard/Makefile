all: i2c_interface force
	gprbuild -p -P ravn.gpr

i2c_interface: src/i2c/i2c_interface.c
	cd src/i2c; gnatgcc -c -fdump-ada-spec -C i2c_interface.c


clean:
	gprclean ravn.gpr

.PHONY: all force clean
