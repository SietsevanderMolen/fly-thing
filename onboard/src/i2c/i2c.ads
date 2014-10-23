with Interfaces;
with Ada.Finalization;
with GNAT.OS_Lib;

package I2C is
   type Bus_Address is range 16#00000# .. 16#fffff#;

   type Bus (Address : Bus_Address)
   is tagged limited private;

   type Chip_Address is range 16#03# .. 16#77#;

   type Chip (Address : Chip_Address; On_Bus : not null access Bus)
   is abstract new Ada.Finalization.Limited_Controlled with null record;

   type Register is range 16#00# .. 16#ff#;
   type Byte is new Interfaces.Unsigned_8;

   procedure Set (C : Chip'class; R : Register; To : Byte);
   function Get (C : Chip'class; R : Register) return Byte;
private
   type Bus (Address : Bus_Address)
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
