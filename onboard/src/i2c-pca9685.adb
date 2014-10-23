with Ada.Text_IO; use Ada.Text_IO;
pragma Warnings (Off, Ada.Text_IO);

package body I2C.PCA9685 is

   -------------------------------
   --  Implementations of spec  --
   -------------------------------
   not overriding
   procedure Reset (C : in out Chip)
   is
   begin
       I2C.Set (Chip=>C, Register=>MODE1, To=>#16#0#);
   end Reset;

   overriding
   procedure Initialize (C : in out Chip)
   is
   begin
      I2C.Chip (C).Initialize;
      C.Reset;
   end Initialize;

   overriding
   procedure Finalize (C : in out Chip)
   is
      use type GNAT.OS_Lib.File_Descriptor;
   begin
      if C.On_Bus.FD /= GNAT.OS_Lib.Invalid_FD then
         C.Reset;
      end if;
      I2C.Chip (C).Finalize;
   end Finalize;
end I2C.PCA9685;
