all: i2c_interface force
	gprbuild -p -P ravn.gpr

i2c_interface: src/i2c/i2c_interface.c
	cd src/i2c; gnatgcc -c -fdump-ada-spec -C i2c_interface.c


clean:
	gprclean ravn.gpr
	rm src/i2c/asm_generic_int_ll64_h.ads
	rm src/i2c/asm_generic_types_h.ads
	rm src/i2c/i2c_interface.o
	rm src/i2c/linux_i2c_dev_h.ads
	rm src/i2c/linux_posix_types_h.ads
	rm src/i2c/linux_types_h.ads
	rm src/i2c/x86_64_linux_gnu_asm_posix_types_64_h.ads
	rm src/i2c/x86_64_linux_gnu_bits_ioctl_types_h.ads
	rm src/i2c/x86_64_linux_gnu_sys_ioctl_h.ads

.PHONY: all force clean
