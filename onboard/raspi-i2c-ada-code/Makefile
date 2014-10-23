#  This package is free software; you can redistribute it and/or
#  modify it under terms of the GNU General Public License as
#  published by the Free Software Foundation; either version 3, or
#  (at your option) any later version.  It is distributed in the
#  hope that it will be useful, but WITHOUT ANY WARRANTY; without
#  even the implied warranty of MERCHANTABILITY or FITNESS FOR A
#  PARTICULAR PURPOSE.
#
#  Copyright Simon Wright <simon@pushface.org>

all::

all:: lib

lib: src/i2c_interface_c.ads force
	gprbuild -p -P build.gpr -XLIBRARY_TYPE=static
	gprbuild -p -P build.gpr -XLIBRARY_TYPE=relocatable

src/i2c_interface_c.ads: src/i2c_interface.c
	cd src; \
	gnatgcc -c -fdump-ada-spec -C i2c_interface.c

# Documentation upload to SF

SFUSER ?= simonjwright

upload-docs:: doc/simonjwright.css $(wildcard doc/*.html)
	rsync \
	  --compress \
	  --copy-unsafe-links \
	  --cvs-exclude \
	  --delete \
	  --perms \
	  --recursive \
	  --rsh=ssh \
	  --times \
	  --update \
	  --verbose \
	  doc/simonjwright.css doc/*.html \
	  $(SFUSER),raspi-i2c-ada@web.sourceforge.net:htdocs/

.PHONY: all force upload-docs
