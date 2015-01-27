with Ada.IO_Exceptions;
with Ada.Strings.Fixed;
with Interfaces; use Interfaces;
with Interfaces.C;
with asm_generic_int_ll64_h;

package body I2C is
   function Read_Bit (C : Chip'class;
                      R : Register;
                      Bit_Num : Integer) return Byte
   is
      Value : constant Integer
        := i2c_interface_c.read_byte_data
              (Integer (C.On_Bus.FD),
               Byte (R));
      use type asm_generic_int_ll64_h.uu_s32;
      --  Data : Byte :=
         --  Byte (Value) and
         --  Byte (
            --  Shift_Right (1, Bit_Num)
         --  );
   begin
      if Value < 0 then
         raise Ada.IO_Exceptions.Device_Error with "reading from chip"
            & Chip_Address'Image (C.Address);
      else
         return Byte (Value);
      end if;
   end Read_Bit;

   function Read_Byte (C : Chip'class) return Byte
   is
      Value : constant Integer
        := i2c_interface_c.read_byte (Interfaces.C.int (C.On_Bus.FD));
   begin
      if Value < 0 then
         raise Ada.IO_Exceptions.Device_Error with "reading from chip"
            & Chip_Address'Image (C.Address);
      else
         return Byte (Value);
      end if;
   end Read_Byte;

   procedure Write_Byte (C : Chip'class; Data : Byte)
   is
      Status : Integer;
   begin
      Status := i2c_interface_c.write_byte
        (Integer (C.On_Bus.FD),
         Data);
      if Status < 0 then
         raise Ada.IO_Exceptions.Device_Error
           with "writing"
           & "to chip"
           & Chip_Address'Image (C.Address);
      end if;
   end Write_Byte;

   function Read_Byte_Data (C : Chip'class; R : Register) return Byte
   is
      Value : constant Integer
        := i2c_interface_c.read_byte_data
        (Integer (C.On_Bus.FD), Byte (R));
      use type asm_generic_int_ll64_h.uu_s32;
   begin
      if Value < 0 then
         raise Ada.IO_Exceptions.Device_Error
           with "reading from chip"
           & Chip_Address'Image (C.Address)
           & " register"
           & Register'Image (R);
      else
         return Byte (Value);
      end if;
   end Read_Byte_Data;

   procedure Write_Byte_Data (C : Chip'class; R : Register; To : Byte)
   is
      Status : Integer;
   begin
      Status := i2c_interface_c.write_byte_data
        (Integer (C.On_Bus.FD),
         Byte (R),
         To);
      if Status < 0 then
         raise Ada.IO_Exceptions.Device_Error
           with "writing to chip"
           & Chip_Address'Image (C.Address)
           & " register"
           & Register'Image (R);
      end if;
   end Write_Byte_Data;

   function Read_Word_Data (C : Chip'class; R : Register) return Word
   is
      Value : constant Integer
        := i2c_interface_c.read_word_data
        (Integer (C.On_Bus.FD), Byte (R));
   begin
      if Value < 0 then
         raise Ada.IO_Exceptions.Device_Error
           with "reading from chip"
           & Chip_Address'Image (C.Address)
           & " register"
           & Register'Image (R);
      else
         return Word (Value);
      end if;
   end Read_Word_Data;

   procedure Write_Word_Data (C : Chip'class; R : Register; To : Word)
   is
      Status : Integer;
   begin
      Status := i2c_interface_c.write_word_data
        (Integer (C.On_Bus.FD),
         Byte (R),
         Interfaces.Unsigned_16 (To));
      if Status < 0 then
         raise Ada.IO_Exceptions.Device_Error
           with "writing to chip"
           & Chip_Address'Image (C.Address)
           & " register"
           & Register'Image (R);
      end if;
   end Write_Word_Data;

   procedure Write_Array_Data (C : Chip'class;
                               R : Register;
                               Values : Byte_Array) is
      Status : Integer;
   begin
      Status := i2c_interface_c.write_i2c_block_data
        (Integer (C.On_Bus.FD),
         Byte (R),
         Values'Length,
         Values);
      if Status < 0 then
         raise Ada.IO_Exceptions.Device_Error
           with "writing to register "
           & Register'Image (R)
           & " on chip "
           & Chip_Address'Image (C.Address);
      end if;
   end Write_Array_Data;

   overriding
   procedure Initialize (B : in out Bus)
   is
      use type GNAT.OS_Lib.File_Descriptor;
      Which_Bus : constant String
        := Ada.Strings.Fixed.Trim (Adapter_Number_T'Image (B.Adapter_Number),
                                   Side => Ada.Strings.Both);
   begin
      if B.FD /= GNAT.OS_Lib.Invalid_FD then
         raise Ada.IO_Exceptions.Use_Error
           with "I2C bus " & Which_Bus & " already open";
      end if;
      B.FD := GNAT.OS_Lib.Open_Read_Write
        (Name => "/dev/i2c/" & Which_Bus,
         Fmode => GNAT.OS_Lib.Binary);
      if B.FD = GNAT.OS_Lib.Invalid_FD then
         B.FD := GNAT.OS_Lib.Open_Read_Write
           (Name => "/dev/i2c-" & Which_Bus,
            Fmode => GNAT.OS_Lib.Binary);
      end if;
      if B.FD = GNAT.OS_Lib.Invalid_FD then
         raise Ada.IO_Exceptions.Name_Error
           with "unable to open either /dev/i2c/" & Which_Bus
           & " or /dev/i2c-" & Which_Bus;
      end if;
   end Initialize;

   overriding
   procedure Finalize (B : in out Bus)
   is
      use type GNAT.OS_Lib.File_Descriptor;
   begin
      if B.FD /= GNAT.OS_Lib.Invalid_FD then
         GNAT.OS_Lib.Close (B.FD);
         B.FD := GNAT.OS_Lib.Invalid_FD;
      end if;
   end Finalize;

   overriding
   procedure Initialize (C : in out Chip)
   is
      --  See i2cbusses::set_slave_addr()
      I2C_SLAVE : constant := 16#0703#;
      function ioctl (FD : Interfaces.C.int;
                      Request : Interfaces.C.unsigned_long;
                      Address : Interfaces.C.int)
                     return Interfaces.C.int;
      pragma Import (C, ioctl, "ioctl");
      use type Interfaces.C.int;
   begin
      if ioctl (Interfaces.C.int (C.On_Bus.FD),
                I2C_SLAVE,
                Interfaces.C.int (C.Address)) < 0
      then
         raise Ada.IO_Exceptions.Use_Error
           with "unable to set slave address to"
           & Chip_Address'Image (C.Address);
      end if;
   end Initialize;
end I2C;
