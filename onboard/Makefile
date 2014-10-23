all: i2c-ada
	gprbuild -p -P ravn.gpr

i2c-ada:
	cd ./raspi-i2c-ada-code/ && $(MAKE)

clean:
	gprclean ravn.gpr
	cd ./raspi-i2c-ada-code/ && gprclean build.gpr
	cd ./raspi-i2c-ada-code/ && gprclean raspi_i2c_ada.gpr

.phony: all clean
