with Ada.Finalization;
with GNAT.OS_Lib;

with i2c_interface_c;

package I2C is
   subtype Byte is i2c_interface_c.Byte;
   subtype Byte_Array is i2c_interface_c.Byte_Array;
   subtype Word is i2c_interface_c.Word;
   subtype Register is Byte range 16#00# .. 16#ff#;

   type Adapter_Number_T is range 16#00000# .. 16#fffff#;

   type Bus (Adapter_Number : Adapter_Number_T)
   is tagged limited private;

   subtype Chip_Address is Byte range 16#03# .. 16#77#;

   type Chip (Address : Chip_Address; On_Bus : not null access Bus)
   is abstract new Ada.Finalization.Limited_Controlled with null record;

   function Read_Bit_Data (C : Chip'Class;
                           R : Register;
                           Bit_Num : Integer) return Byte;
   pragma Precondition (Bit_Num >= 0 and Bit_Num <= 7);
   --  Read multiple bits from 8 bit register
   function Read_Bits_Data (C : Chip'Class;
                            R : Register;
                            Start_Bit : Integer;
                            Length : Integer) return Byte;
   pragma Precondition (Start_Bit >= 0 and Start_Bit <= 7 and
                        Length >= 0 and Length <= 7);
   --  Read a byte
   function Read_Byte (C : Chip'Class) return Byte;
   --  Write a byte
   procedure Write_Byte (C : Chip'class; Data : Byte);
   --  Read a byte from a specific register
   function Read_Byte_Data (C : Chip'class; R : Register) return Byte;
   --  Write a byte to a specific register
   procedure Write_Byte_Data (C : Chip'class; R : Register; D : Byte);
   --  Read a word from a specific register
   function Read_Word_Data (C : Chip'class; R : Register) return Word;
   --  Write a word to a specific register
   procedure Write_Word_Data (C : Chip'class; R : Register; D : Word);
   --  Read an array of bytes from a specific register
   function Read_Array_Data (C : Chip'class;
                             R : Register;
                             L : Integer) return Byte_Array;
   pragma Precondition (L < 32); --  max according to i2c-dev.h
   --  Write an array of bytes to a specific register
   procedure Write_Array_Data (C : Chip'class;
                               R : Register;
                               Values : Byte_Array);
   pragma Precondition (Values'Length < 32); --  max according to i2c-dev.h
private
   type Bus (Adapter_Number : Adapter_Number_T)
   is new Ada.Finalization.Limited_Controlled with record
      FD : GNAT.OS_Lib.File_Descriptor := GNAT.OS_Lib.Invalid_FD;
   end record;

   overriding
   procedure Initialize (B : in out Bus);

   overriding
   procedure Finalize (B : in out Bus);

   overriding
   procedure Initialize (C : in out Chip);

   --  Set the IOCTL slave address
   procedure Set_Slave_Address_To (C : in Chip);
end I2C;
