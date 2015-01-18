all: i2c_interface
	gprbuild -p -P ravn.gpr

i2c_interface: src/i2c/i2c_interface/i2c_interface.c
	cd src/i2c/i2c_interface; gnatgcc -c -fdump-ada-spec -C i2c_interface.c

clean:
	gprclean ravn.gpr
	-rm -f src/i2c/i2c_interface/*.o
	-rm -f src/i2c/i2c_interface/*_h.ads
	-rm -f src/i2c/i2c_interface/*_c.ads

.PHONY: all clean
