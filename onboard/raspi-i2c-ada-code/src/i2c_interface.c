/*
 *  This package is free software; you can redistribute it and/or
 *  modify it under terms of the GNU General Public License as
 *  published by the Free Software Foundation; either version 3, or
 *  (at your option) any later version.  It is distributed in the
 *  hope that it will be useful, but WITHOUT ANY WARRANTY; without
 *  even the implied warranty of MERCHANTABILITY or FITNESS FOR A
 *  PARTICULAR PURPOSE.
 *
 *  As a special exception under Section 7 of GPL version 3, you are
 *  granted additional permissions described in the GCC Runtime
 *  Library Exception, version 3.1, as published by the Free Software
 *  Foundation.
 *
 *  You should have received a copy of the GNU General Public License
 *  and a copy of the GCC Runtime Library Exception along with this
 *  program; see the files COPYING3 and COPYING.RUNTIME respectively.
 *  If not, see <http://www.gnu.org/licenses/>.
 *
 *  Copyright Simon Wright <simon@pushface.org>
 */

/*
 * The Linux I2C header <linux/i2c-dev.h> doesn't correspond to any
 * library; instead, all the functions are declared inline.
 *
 * This file provides a callable interface to each declared function,
 * so that it can be called from Ada using pragma Import.
 */

#include <linux/i2c-dev.h>

__s32 write_quick(int file, __u8 value)
{
  return i2c_smbus_write_quick(file, value);
}

__s32 read_byte(int file) {
  return i2c_smbus_read_byte(file);
}

__s32 write_byte(int file, __u8 value)
{
  return i2c_smbus_write_byte(file, value);
}

__s32 read_byte_data(int file, __u8 command)
{
  return i2c_smbus_read_byte_data(file, command);
}

__s32 write_byte_data(int file, __u8 command, __u8 value)
{
  return i2c_smbus_write_byte_data(file, command, value);
}

__s32 read_word_data(int file, __u8 command)
{
  return i2c_smbus_read_word_data(file, command);
}

__s32 write_word_data(int file, __u8 command, __u16 value)
{
  return i2c_smbus_write_word_data(file, command, value);
}

__s32 process_call(int file, __u8 command, __u16 value)
{
  return i2c_smbus_process_call(file, command, value);
}

/* Returns the number of read bytes */
__s32 read_block_data(int file, __u8 command, __u8 *values)
{
  return i2c_smbus_read_block_data(file, command, values);
}

__s32 write_block_data(int file,
                       __u8 command,
                       __u8 length,
                       const __u8 *values)
{
  return i2c_smbus_write_block_data(file, command, length, values);
}

/* Returns the number of read bytes */
/* Until kernel 2.6.22, the length is hardcoded to 32 bytes. If you
   ask for less than 32 bytes, your code will only work with kernels
   2.6.23 and later. */
__s32 read_i2c_block_data(int file,
                          __u8 command,
                          __u8 length,
                          __u8 *values)
{
  return i2c_smbus_read_i2c_block_data(file, command, length, values);
}

__s32 write_i2c_block_data(int file,
                           __u8 command,
                           __u8 length,
                           const __u8 *values)
{
  return i2c_smbus_write_i2c_block_data(file, command, length, values);
}

/* Returns the number of read bytes */
__s32 block_process_call(int file,
                         __u8 command,
                         __u8 length,
                         __u8 *values)
{
  return i2c_smbus_block_process_call(file, command, length, values);
}
