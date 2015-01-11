all: i2c_interface
	gprbuild -p -P ravn.gpr

i2c_interface: src/i2c/i2c_interface.c
	cd src/i2c; gnatgcc -c -fdump-ada-spec -C i2c_interface.c


clean:
	gprclean ravn.gpr
	-rm -f src/i2c/*.o
	-rm -f src/i2c/*_h.ads
	-rm -f src/i2c/*_c.ads

.PHONY: all clean
