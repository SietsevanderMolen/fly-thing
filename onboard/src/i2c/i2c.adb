with Ada.IO_Exceptions;
with Ada.Strings.Fixed;
with Interfaces; use Interfaces;
with Interfaces.C;

package body I2C is
   function Read_Bit_Data (C : Chip'class;
                           R : Register;
                           Bit_Num : Integer) return Byte
   is
      Value : constant Integer := i2c_interface_c.read_byte_data
         (Integer (C.On_Bus.FD), C.Address, R);
   begin
      if Value < 0 then
         raise Ada.IO_Exceptions.Device_Error with "reading bit from chip"
            & Chip_Address'Image (C.Address) & " at register "
            & Register'Image (R) & " and bit number "
            & Integer'Image (Bit_Num);
      else
         declare
            Data : constant Byte := Byte (Value) and Shift_Left (1, Bit_Num);
         begin
            return Data;
         end;
      end if;
   end Read_Bit_Data;

   function Read_Bits_Data (C : Chip'class;
                            R : Register;
                            Start_Bit : Integer;
                            Length : Integer) return Byte
   is
      Value : constant Integer := i2c_interface_c.read_byte_data
         (Integer (C.On_Bus.FD), C.Address, R);
   begin
      if Value < 0 then
         raise Ada.IO_Exceptions.Device_Error with "reading from chip"
            & Chip_Address'Image (C.Address);
      else
         --  01101001 read byte
         --  76543210 bit numbers
         --  xxx args: bitStart=4, length=3
         --  010 masked
         --  -> 010 shifted
         declare
            Mask : constant Byte := Shift_Left (((Shift_Left (1, Length)) - 1),
                                                 (Start_Bit - Length + 1));
            Masked_Data : constant Byte := Byte (Value) and Mask;
            Shifted_Data : constant Byte :=
               Shift_Right (Masked_Data, Start_Bit - Length + 1);
         begin
            return Shifted_Data;
         end;
      end if;
   end Read_Bits_Data;

   function Read_Byte (C : Chip'class) return Byte
   is
      Value : constant Integer := i2c_interface_c.read_byte
         (Integer (C.On_Bus.FD), C.Address);
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
         (Integer (C.On_Bus.FD), C.Address, Data);
      if Status < 0 then
         raise Ada.IO_Exceptions.Device_Error
            with "writing " & Byte'Image (Data)
            & " to chip " & Chip_Address'Image (C.Address);
      end if;
   end Write_Byte;

   function Read_Byte_Data (C : Chip'class; R : Register) return Byte
   is
      Value : constant Integer := i2c_interface_c.read_byte_data
         (Integer (C.On_Bus.FD), C.Address, R);
   begin
      if Value < 0 then
         raise Ada.IO_Exceptions.Device_Error
            with "reading from chip " & Chip_Address'Image (C.Address)
            & " register" & Register'Image (R);
      else
         return Byte (Value);
      end if;
   end Read_Byte_Data;

   procedure Write_Byte_Data (C : Chip'class; R : Register; D : Byte)
   is
      Status : Integer;
   begin
      Status :=
         i2c_interface_c.write_byte_data
            (Integer (C.On_Bus.FD), C.Address, R, D);
      if Status < 0 then
         raise Ada.IO_Exceptions.Device_Error
            with "writing to chip " & Chip_Address'Image (C.Address)
            & " register" & Register'Image (R);
      end if;
   end Write_Byte_Data;

   function Read_Word_Data (C : Chip'class; R : Register) return Word
   is
      Value : constant Integer := i2c_interface_c.read_word_data
         (Integer (C.On_Bus.FD), C.Address, R);
   begin
      if Value < 0 then
         raise Ada.IO_Exceptions.Device_Error
            with "reading from chip" & Chip_Address'Image (C.Address)
            & " register" & Register'Image (R);
      else
         return Word (Value);
      end if;
   end Read_Word_Data;

   procedure Write_Word_Data (C : Chip'class; R : Register; D : Word)
   is
      Status : Integer;
   begin
      Status := i2c_interface_c.write_word_data
        (Integer (C.On_Bus.FD), C.Address, R, D);
      if Status < 0 then
         raise Ada.IO_Exceptions.Device_Error
            with "writing to chip" & Chip_Address'Image (C.Address)
            & " register" & Register'Image (R);
      end if;
   end Write_Word_Data;

   procedure Write_Array_Data (C : Chip'class;
                               R : Register;
                               Values : Byte_Array) is
      Status : Integer;
   begin
      Status := i2c_interface_c.write_i2c_block_data
        (Integer (C.On_Bus.FD), C.Address, R, Values'Length, Values);
      if Status < 0 then
         raise Ada.IO_Exceptions.Device_Error
            with "writing to register " & Register'Image (R)
            & " on chip " & Chip_Address'Image (C.Address);
      end if;
   end Write_Array_Data;

   function Read_Array_Data (C : Chip'class;
                             R : Register;
                             L : Integer) return Byte_Array is
      Status : Integer;
      Values : Byte_Array (0 .. L - 1);
   begin
      Status := i2c_interface_c.read_i2c_block_data
        (Integer (C.On_Bus.FD), C.Address, R, Byte (L), Values);
      if Status < 0 then
         raise Ada.IO_Exceptions.Device_Error
            with "reading from register " & Register'Image (R)
            & " on chip " & Chip_Address'Image (C.Address)
            & " status was " & Integer'Image (Status);
      else
         return Values;
      end if;
   end Read_Array_Data;

   procedure Set_Slave_Address_To (C : in Chip) is
      I2C_SLAVE : constant := 16#0703#;
      function ioctl (FD : Interfaces.C.int;
                      Request : Interfaces.C.unsigned_long;
                      Address : Interfaces.C.int) return Interfaces.C.int;
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
   end Set_Slave_Address_To;

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
      I2C_SLAVE : constant := 16#0703#;
      function ioctl (FD : Interfaces.C.int;
                      Request : Interfaces.C.unsigned_long;
                      Address : Interfaces.C.int) return Interfaces.C.int;
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
