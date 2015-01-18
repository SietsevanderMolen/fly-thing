with Ada.Finalization;
with GNAT.OS_Lib;
with Interfaces;
with Interfaces.C;

with i2c_interface_c;

package I2C is
   type Adapter_Number_T is range 16#00000# .. 16#fffff#;

   type Bus (Adapter_Number : Adapter_Number_T)
   is tagged limited private;

   type Chip_Address is range 16#03# .. 16#77#;

   type Chip (Address : Chip_Address; On_Bus : not null access Bus)
   is abstract new Ada.Finalization.Limited_Controlled with null record;

   type Register is range 16#00# .. 16#ff#;
   subtype Byte is Interfaces.C.unsigned_char;

   procedure Set (C : Chip'class; R : Register; To : Byte);
   function Get (C : Chip'class; R : Register) return Byte;
   procedure Write_Byte (C : Chip'class; Data : Byte);
   procedure Write_Array (C : Chip'class;
                          R : Register;
                          L : Byte;
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
