package body PCA9685 is
   not overriding
   procedure Reset (C : in out Chip)
   is
   begin
      Set (C => C, R => MODE1, To => 16#0#);
   end Reset;

   not overriding
   procedure Set (C : Chip; R : Register; To : Byte)
   is
      Status : asm_generic_int_ll64_h.uu_s32;
      use type asm_generic_int_ll64_h.uu_s32;
   begin
      Status := i2c_interface_c.write_byte_data
        (Interfaces.C.int (C.On_Bus.FD),
         asm_generic_int_ll64_h.uu_u8 (R),
         asm_generic_int_ll64_h.uu_u8 (To));
      if Status < 0 then
         raise Ada.IO_Exceptions.Device_Error
           with "writing to chip"
           & Chip_Address'Image (C.Address)
           & " register"
           & Register'Image (R);
      end if;
   end Set;

   not overriding
   function Get (C : Chip; R : Register) return Byte
   is
      Value : constant asm_generic_int_ll64_h.uu_s32
        := i2c_interface_c.read_byte_data
        (Interfaces.C.int (C.On_Bus.FD), asm_generic_int_ll64_h.uu_u8 (R));
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
   end Get;

   overriding
   procedure Initialize (B : in out Bus)
   is
      use type GNAT.OS_Lib.File_Descriptor;
      Which_Bus : constant String
        := Ada.Strings.Fixed.Trim (Bus_Address'Image (B.Address),
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
                Interfaces.C.int (C.Address)) < 0 then
         raise Ada.IO_Exceptions.Use_Error
           with "unable to set slave address to"
           & Chip_Address'Image (C.Address);
      end if;
   end Initialize;

end PCA9685;
