pragma Ada_2005;
pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;
with asm_generic_int_ll64_h;
with Interfaces.C.Strings;

package i2c_dev_h is

   --  unsupported macro: I2C_M_TEN 0x10
   --  unsupported macro: I2C_M_RD 0x01
   --  unsupported macro: I2C_M_NOSTART 0x4000
   --  unsupported macro: I2C_M_REV_DIR_ADDR 0x2000
   --  unsupported macro: I2C_M_IGNORE_NAK 0x1000
   --  unsupported macro: I2C_M_NO_RD_ACK 0x0800
   --  unsupported macro: I2C_FUNC_I2C 0x00000001
   --  unsupported macro: I2C_FUNC_10BIT_ADDR 0x00000002
   --  unsupported macro: I2C_FUNC_PROTOCOL_MANGLING 0x00000004
   --  unsupported macro: I2C_FUNC_SMBUS_PEC 0x00000008
   --  unsupported macro: I2C_FUNC_SMBUS_BLOCK_PROC_CALL 0x00008000
   --  unsupported macro: I2C_FUNC_SMBUS_QUICK 0x00010000
   --  unsupported macro: I2C_FUNC_SMBUS_READ_BYTE 0x00020000
   --  unsupported macro: I2C_FUNC_SMBUS_WRITE_BYTE 0x00040000
   --  unsupported macro: I2C_FUNC_SMBUS_READ_BYTE_DATA 0x00080000
   --  unsupported macro: I2C_FUNC_SMBUS_WRITE_BYTE_DATA 0x00100000
   --  unsupported macro: I2C_FUNC_SMBUS_READ_WORD_DATA 0x00200000
   --  unsupported macro: I2C_FUNC_SMBUS_WRITE_WORD_DATA 0x00400000
   --  unsupported macro: I2C_FUNC_SMBUS_PROC_CALL 0x00800000
   --  unsupported macro: I2C_FUNC_SMBUS_READ_BLOCK_DATA 0x01000000
   --  unsupported macro: I2C_FUNC_SMBUS_WRITE_BLOCK_DATA 0x02000000
   --  unsupported macro: I2C_FUNC_SMBUS_READ_I2C_BLOCK 0x04000000
   --  unsupported macro: I2C_FUNC_SMBUS_WRITE_I2C_BLOCK 0x08000000
   --  unsupported macro: I2C_FUNC_SMBUS_BYTE (I2C_FUNC_SMBUS_READ_BYTE | I2C_FUNC_SMBUS_WRITE_BYTE)
   --  unsupported macro: I2C_FUNC_SMBUS_BYTE_DATA (I2C_FUNC_SMBUS_READ_BYTE_DATA | I2C_FUNC_SMBUS_WRITE_BYTE_DATA)
   --  unsupported macro: I2C_FUNC_SMBUS_WORD_DATA (I2C_FUNC_SMBUS_READ_WORD_DATA | I2C_FUNC_SMBUS_WRITE_WORD_DATA)
   --  unsupported macro: I2C_FUNC_SMBUS_BLOCK_DATA (I2C_FUNC_SMBUS_READ_BLOCK_DATA | I2C_FUNC_SMBUS_WRITE_BLOCK_DATA)
   --  unsupported macro: I2C_FUNC_SMBUS_I2C_BLOCK (I2C_FUNC_SMBUS_READ_I2C_BLOCK | I2C_FUNC_SMBUS_WRITE_I2C_BLOCK)
   --  unsupported macro: I2C_FUNC_SMBUS_HWPEC_CALC I2C_FUNC_SMBUS_PEC
   --  unsupported macro: I2C_SMBUS_BLOCK_MAX 32
   --  unsupported macro: I2C_SMBUS_I2C_BLOCK_MAX 32
   --  unsupported macro: I2C_SMBUS_READ 1
   --  unsupported macro: I2C_SMBUS_WRITE 0
   --  unsupported macro: I2C_SMBUS_QUICK 0
   --  unsupported macro: I2C_SMBUS_BYTE 1
   --  unsupported macro: I2C_SMBUS_BYTE_DATA 2
   --  unsupported macro: I2C_SMBUS_WORD_DATA 3
   --  unsupported macro: I2C_SMBUS_PROC_CALL 4
   --  unsupported macro: I2C_SMBUS_BLOCK_DATA 5
   --  unsupported macro: I2C_SMBUS_I2C_BLOCK_BROKEN 6
   --  unsupported macro: I2C_SMBUS_BLOCK_PROC_CALL 7
   --  unsupported macro: I2C_SMBUS_I2C_BLOCK_DATA 8
   --  unsupported macro: I2C_RETRIES 0x0701
   --  unsupported macro: I2C_TIMEOUT 0x0702
   --  unsupported macro: I2C_SLAVE 0x0703
   --  unsupported macro: I2C_SLAVE_FORCE 0x0706
   --  unsupported macro: I2C_TENBIT 0x0704
   --  unsupported macro: I2C_FUNCS 0x0705
   --  unsupported macro: I2C_RDWR 0x0707
   --  unsupported macro: I2C_PEC 0x0708
   --  unsupported macro: I2C_SMBUS 0x0720
  --    i2c-dev.h - i2c-bus driver, char device interface
  --    Copyright (C) 1995-97 Simon G. Vogl
  --    Copyright (C) 1998-99 Frodo Looijaard <frodol@dds.nl>
  --    This program is free software; you can redistribute it and/or modify
  --    it under the terms of the GNU General Public License as published by
  --    the Free Software Foundation; either version 2 of the License, or
  --    (at your option) any later version.
  --    This program is distributed in the hope that it will be useful,
  --    but WITHOUT ANY WARRANTY; without even the implied warranty of
  --    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  --    GNU General Public License for more details.
  --    You should have received a copy of the GNU General Public License
  --    along with this program; if not, write to the Free Software
  --    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
  --    MA 02110-1301 USA.
  -- 

  -- $Id$  
  -- -- i2c.h --  
  -- * I2C Message - used for pure i2c transaction, also from /dev interface
  --  

  -- slave address			 
   type i2c_msg is record
      addr : aliased asm_generic_int_ll64_h.uu_u16;  -- i2c-dev.h:40
      flags : aliased unsigned_short;  -- i2c-dev.h:41
      len : aliased short;  -- i2c-dev.h:48
      buf : Interfaces.C.Strings.chars_ptr;  -- i2c-dev.h:49
   end record;
   pragma Convention (C_Pass_By_Copy, i2c_msg);  -- i2c-dev.h:39

  -- msg length				 
  -- pointer to msg data			 
  -- To determine what functionality is present  
  -- Old name, for compatibility  
  -- 
  -- * Data for SMBus Messages 
  --  

   type i2c_smbus_data_block_array is array (0 .. 33) of aliased asm_generic_int_ll64_h.uu_u8;
   type i2c_smbus_data (discr : unsigned := 0) is record
      case discr is
         when 0 =>
            byte : aliased asm_generic_int_ll64_h.uu_u8;  -- i2c-dev.h:92
         when 1 =>
            word : aliased asm_generic_int_ll64_h.uu_u16;  -- i2c-dev.h:93
         when others =>
            block : aliased i2c_smbus_data_block_array;  -- i2c-dev.h:94
      end case;
   end record;
   pragma Convention (C_Pass_By_Copy, i2c_smbus_data);
   pragma Unchecked_Union (i2c_smbus_data);  -- i2c-dev.h:91

  -- block[0] is used for length  
  -- and one more for PEC  
  -- smbus_access read or write markers  
  -- SMBus transaction types (size parameter in the above functions) 
  --   Note: these no longer correspond to the (arbitrary) PIIX4 internal codes!  

  -- ----- commands for the ioctl like i2c_command call:
  -- * note that additional calls are defined in the algorithm and hw 
  -- *	dependent layers - these can be listed here, or see the 
  -- *	corresponding header files.
  --  

  -- -> bit-adapter specific ioctls	 
  -- should be polled when not             
  -- acknowledging 			 
  -- this is for i2c-dev.c	 
  -- Attn.: Slave address is 7 or 10 bits  
  -- Attn.: Slave address is 7 or 10 bits  
  -- This changes the address, even if it  
  -- is already taken!			 
  -- -- i2c.h --  
  -- Note: 10-bit addresses are NOT supported!  
  -- This is the structure as used in the I2C_SMBUS ioctl call  
   type i2c_smbus_ioctl_data is record
      read_write : aliased char;  -- i2c-dev.h:149
      command : aliased asm_generic_int_ll64_h.uu_u8;  -- i2c-dev.h:150
      size : aliased int;  -- i2c-dev.h:151
      data : access i2c_smbus_data;  -- i2c-dev.h:152
   end record;
   pragma Convention (C_Pass_By_Copy, i2c_smbus_ioctl_data);  -- i2c-dev.h:148

  -- This is the structure as used in the I2C_RDWR ioctl call  
  -- pointers to i2c_msgs  
   type i2c_rdwr_ioctl_data is record
      msgs : access i2c_msg;  -- i2c-dev.h:157
      nmsgs : aliased int;  -- i2c-dev.h:158
   end record;
   pragma Convention (C_Pass_By_Copy, i2c_rdwr_ioctl_data);  -- i2c-dev.h:156

  -- number of i2c_msgs  
   function i2c_smbus_access
     (file : int;
      read_write : char;
      command : asm_generic_int_ll64_h.uu_u8;
      size : int;
      data : access i2c_smbus_data) return asm_generic_int_ll64_h.uu_s32;  -- i2c-dev.h:162
   pragma Import (CPP, i2c_smbus_access, "_ZL16i2c_smbus_accessichiP14i2c_smbus_data");

   function i2c_smbus_write_quick (file : int; value : asm_generic_int_ll64_h.uu_u8) return asm_generic_int_ll64_h.uu_s32;  -- i2c-dev.h:175
   pragma Import (CPP, i2c_smbus_write_quick, "_ZL21i2c_smbus_write_quickih");

   function i2c_smbus_read_byte (file : int) return asm_generic_int_ll64_h.uu_s32;  -- i2c-dev.h:180
   pragma Import (CPP, i2c_smbus_read_byte, "_ZL19i2c_smbus_read_bytei");

   function i2c_smbus_write_byte (file : int; value : asm_generic_int_ll64_h.uu_u8) return asm_generic_int_ll64_h.uu_s32;  -- i2c-dev.h:189
   pragma Import (CPP, i2c_smbus_write_byte, "_ZL20i2c_smbus_write_byteih");

   function i2c_smbus_read_byte_data (file : int; command : asm_generic_int_ll64_h.uu_u8) return asm_generic_int_ll64_h.uu_s32;  -- i2c-dev.h:195
   pragma Import (CPP, i2c_smbus_read_byte_data, "_ZL24i2c_smbus_read_byte_dataih");

   function i2c_smbus_write_byte_data
     (file : int;
      command : asm_generic_int_ll64_h.uu_u8;
      value : asm_generic_int_ll64_h.uu_u8) return asm_generic_int_ll64_h.uu_s32;  -- i2c-dev.h:205
   pragma Import (CPP, i2c_smbus_write_byte_data, "_ZL25i2c_smbus_write_byte_dataihh");

   function i2c_smbus_read_word_data (file : int; command : asm_generic_int_ll64_h.uu_u8) return asm_generic_int_ll64_h.uu_s32;  -- i2c-dev.h:214
   pragma Import (CPP, i2c_smbus_read_word_data, "_ZL24i2c_smbus_read_word_dataih");

   function i2c_smbus_write_word_data
     (file : int;
      command : asm_generic_int_ll64_h.uu_u8;
      value : asm_generic_int_ll64_h.uu_u16) return asm_generic_int_ll64_h.uu_s32;  -- i2c-dev.h:224
   pragma Import (CPP, i2c_smbus_write_word_data, "_ZL25i2c_smbus_write_word_dataiht");

   function i2c_smbus_process_call
     (file : int;
      command : asm_generic_int_ll64_h.uu_u8;
      value : asm_generic_int_ll64_h.uu_u16) return asm_generic_int_ll64_h.uu_s32;  -- i2c-dev.h:233
   pragma Import (CPP, i2c_smbus_process_call, "_ZL22i2c_smbus_process_calliht");

  -- Returns the number of read bytes  
   function i2c_smbus_read_block_data
     (file : int;
      command : asm_generic_int_ll64_h.uu_u8;
      values : access asm_generic_int_ll64_h.uu_u8) return asm_generic_int_ll64_h.uu_s32;  -- i2c-dev.h:246
   pragma Import (CPP, i2c_smbus_read_block_data, "_ZL25i2c_smbus_read_block_dataihPh");

   function i2c_smbus_write_block_data
     (file : int;
      command : asm_generic_int_ll64_h.uu_u8;
      length : asm_generic_int_ll64_h.uu_u8;
      values : access asm_generic_int_ll64_h.uu_u8) return asm_generic_int_ll64_h.uu_s32;  -- i2c-dev.h:261
   pragma Import (CPP, i2c_smbus_write_block_data, "_ZL26i2c_smbus_write_block_dataihhPKh");

  -- Returns the number of read bytes  
  -- Until kernel 2.6.22, the length is hardcoded to 32 bytes. If you
  --   ask for less than 32 bytes, your code will only work with kernels
  --   2.6.23 and later.  

   function i2c_smbus_read_i2c_block_data
     (file : int;
      command : asm_generic_int_ll64_h.uu_u8;
      length : asm_generic_int_ll64_h.uu_u8;
      values : access asm_generic_int_ll64_h.uu_u8) return asm_generic_int_ll64_h.uu_s32;  -- i2c-dev.h:279
   pragma Import (CPP, i2c_smbus_read_i2c_block_data, "_ZL29i2c_smbus_read_i2c_block_dataihhPh");

   function i2c_smbus_write_i2c_block_data
     (file : int;
      command : asm_generic_int_ll64_h.uu_u8;
      length : asm_generic_int_ll64_h.uu_u8;
      values : access asm_generic_int_ll64_h.uu_u8) return asm_generic_int_ll64_h.uu_s32;  -- i2c-dev.h:299
   pragma Import (CPP, i2c_smbus_write_i2c_block_data, "_ZL30i2c_smbus_write_i2c_block_dataihhPKh");

  -- Returns the number of read bytes  
   function i2c_smbus_block_process_call
     (file : int;
      command : asm_generic_int_ll64_h.uu_u8;
      length : asm_generic_int_ll64_h.uu_u8;
      values : access asm_generic_int_ll64_h.uu_u8) return asm_generic_int_ll64_h.uu_s32;  -- i2c-dev.h:314
   pragma Import (CPP, i2c_smbus_block_process_call, "_ZL28i2c_smbus_block_process_callihhPh");

end i2c_dev_h;
