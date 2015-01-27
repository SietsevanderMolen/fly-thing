with Ada.Finalization;
with GNAT.OS_Lib;

with i2c_interface_c;

package I2C is
   type Adapter_Number_T is range 16#00000# .. 16#fffff#;

   type Bus (Adapter_Number : Adapter_Number_T)
   is tagged limited private;

   type Chip_Address is range 16#03# .. 16#77#;

   type Chip (Address : Chip_Address; On_Bus : not null access Bus)
   is abstract new Ada.Finalization.Limited_Controlled with null record;

   subtype Byte is i2c_interface_c.Byte;
   subtype Byte_Array is i2c_interface_c.Byte_Array;
   subtype Word is i2c_interface_c.Word;
   subtype Register is Byte range 16#00# .. 16#ff#;

   function Read_Bit (C : Chip'Class;
                      R : Register;
                      Bit_Num : Integer) return Byte;
   pragma Precondition (Bit_Num >= 0 and Bit_Num <= 7);
   --  Read multiple bits from 8 bit register
   --  function Read_Bits (C : Chip'Class;
                       --  R : Register;
                       --  Start_Bit : Integer;
                       --  Length : Integer) return Byte;
   --  pragma Precondition (Start_Bit >= 0 and Start_Bit <= 7 and
                        --  Length >= 0 and Length <= 7);
   --  Read a byte
   function Read_Byte (C : Chip'Class) return Byte;
   --  Write a byte
   procedure Write_Byte (C : Chip'class; Data : Byte);
   --  Read a byte from a specific register
   function Read_Byte_Data (C : Chip'class; R : Register) return Byte;
   --  Write a byte to a specific register
   procedure Write_Byte_Data (C : Chip'class; R : Register; To : Byte);
   --  Read a word from a specific register
   function Read_Word_Data (C : Chip'class; R : Register) return Word;
   --  Write a word to a specific register
   procedure Write_Word_Data (C : Chip'class; R : Register; To : Word);
   --  Write an array of bytes to a specific register
   procedure Write_Array_Data (C : Chip'class;
                               R : Register;
                               Values : Byte_Array);
   pragma Precondition (Values'Length <= 31); --  Max is 32-Register
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
end I2C;
