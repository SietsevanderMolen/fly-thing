with Interfaces.C; use Interfaces.C;
with asm_generic_int_ll64_h;

package i2c_interface_c is

  -- *  This package is free software; you can redistribute it and/or
  -- *  modify it under terms of the GNU General Public License as
  -- *  published by the Free Software Foundation; either version 3, or
  -- *  (at your option) any later version.  It is distributed in the
  -- *  hope that it will be useful, but WITHOUT ANY WARRANTY; without
  -- *  even the implied warranty of MERCHANTABILITY or FITNESS FOR A
  -- *  PARTICULAR PURPOSE.
  -- *
  -- *  As a special exception under Section 7 of GPL version 3, you are
  -- *  granted additional permissions described in the GCC Runtime
  -- *  Library Exception, version 3.1, as published by the Free Software
  -- *  Foundation.
  -- *
  -- *  You should have received a copy of the GNU General Public License
  -- *  and a copy of the GCC Runtime Library Exception along with this
  -- *  program; see the files COPYING3 and COPYING.RUNTIME respectively.
  -- *  If not, see <http://www.gnu.org/licenses/>.
  -- *
  -- *  Copyright Simon Wright <simon@pushface.org>
  --  

  -- * The Linux I2C header <linux/i2c-dev.h> doesn't correspond to any
  -- * library; instead, all the functions are declared inline.
  -- *
  -- * This file provides a callable interface to each declared function,
  -- * so that it can be called from Ada using pragma Import.
  --  

   function write_quick (file : int; value : asm_generic_int_ll64_h.uu_u8) return asm_generic_int_ll64_h.uu_s32;  -- i2c_interface.c:33
   pragma Import (C, write_quick, "write_quick");

   function read_byte (file : int) return asm_generic_int_ll64_h.uu_s32;  -- i2c_interface.c:38
   pragma Import (C, read_byte, "read_byte");

   function write_byte (file : int; value : asm_generic_int_ll64_h.uu_u8) return asm_generic_int_ll64_h.uu_s32;  -- i2c_interface.c:42
   pragma Import (C, write_byte, "write_byte");

   function read_byte_data (file : int; command : asm_generic_int_ll64_h.uu_u8) return asm_generic_int_ll64_h.uu_s32;  -- i2c_interface.c:47
   pragma Import (C, read_byte_data, "read_byte_data");

   function write_byte_data
     (file : int;
      command : asm_generic_int_ll64_h.uu_u8;
      value : asm_generic_int_ll64_h.uu_u8) return asm_generic_int_ll64_h.uu_s32;  -- i2c_interface.c:52
   pragma Import (C, write_byte_data, "write_byte_data");

   function read_word_data (file : int; command : asm_generic_int_ll64_h.uu_u8) return asm_generic_int_ll64_h.uu_s32;  -- i2c_interface.c:57
   pragma Import (C, read_word_data, "read_word_data");

   function write_word_data
     (file : int;
      command : asm_generic_int_ll64_h.uu_u8;
      value : asm_generic_int_ll64_h.uu_u16) return asm_generic_int_ll64_h.uu_s32;  -- i2c_interface.c:62
   pragma Import (C, write_word_data, "write_word_data");

   function process_call
     (file : int;
      command : asm_generic_int_ll64_h.uu_u8;
      value : asm_generic_int_ll64_h.uu_u16) return asm_generic_int_ll64_h.uu_s32;  -- i2c_interface.c:67
   pragma Import (C, process_call, "process_call");

  -- Returns the number of read bytes  
   function read_block_data
     (file : int;
      command : asm_generic_int_ll64_h.uu_u8;
      values : access asm_generic_int_ll64_h.uu_u8) return asm_generic_int_ll64_h.uu_s32;  -- i2c_interface.c:73
   pragma Import (C, read_block_data, "read_block_data");

   function write_block_data
     (file : int;
      command : asm_generic_int_ll64_h.uu_u8;
      length : asm_generic_int_ll64_h.uu_u8;
      values : access asm_generic_int_ll64_h.uu_u8) return asm_generic_int_ll64_h.uu_s32;  -- i2c_interface.c:78
   pragma Import (C, write_block_data, "write_block_data");

  -- Returns the number of read bytes  
  -- Until kernel 2.6.22, the length is hardcoded to 32 bytes. If you
  --   ask for less than 32 bytes, your code will only work with kernels
  --   2.6.23 and later.  

   function read_i2c_block_data
     (file : int;
      command : asm_generic_int_ll64_h.uu_u8;
      length : asm_generic_int_ll64_h.uu_u8;
      values : access asm_generic_int_ll64_h.uu_u8) return asm_generic_int_ll64_h.uu_s32;  -- i2c_interface.c:90
   pragma Import (C, read_i2c_block_data, "read_i2c_block_data");

   function write_i2c_block_data
     (file : int;
      command : asm_generic_int_ll64_h.uu_u8;
      length : asm_generic_int_ll64_h.uu_u8;
      values : access asm_generic_int_ll64_h.uu_u8) return asm_generic_int_ll64_h.uu_s32;  -- i2c_interface.c:98
   pragma Import (C, write_i2c_block_data, "write_i2c_block_data");

  -- Returns the number of read bytes  
   function block_process_call
     (file : int;
      command : asm_generic_int_ll64_h.uu_u8;
      length : asm_generic_int_ll64_h.uu_u8;
      values : access asm_generic_int_ll64_h.uu_u8) return asm_generic_int_ll64_h.uu_s32;  -- i2c_interface.c:107
   pragma Import (C, block_process_call, "block_process_call");

end i2c_interface_c;
