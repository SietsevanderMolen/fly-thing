with Ada.Finalization;
with GNAT.OS_Lib;

with asm_generic_int_ll64_h;
with i2c_interface_c;

package I2C is
   type Adapter_Number_T is range 16#00000# .. 16#fffff#;

   type Bus (Adapter_Number : Adapter_Number_T)
   is tagged limited private;

   type Chip_Address is range 16#03# .. 16#77#;

   type Chip (Address : Chip_Address; On_Bus : not null access Bus)
   is abstract new Ada.Finalization.Limited_Controlled with null record;

   type Register is range 16#00# .. 16#ff#;
   subtype Byte is asm_generic_int_ll64_h.uu_u8;
   subtype Word is asm_generic_int_ll64_h.uu_s32;


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

   procedure Write_Array (C : Chip'class;
                          R : Register;
                          Values : i2c_interface_c.Byte_Array);
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
