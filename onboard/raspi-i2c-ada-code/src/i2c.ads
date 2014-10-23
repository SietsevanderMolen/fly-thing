--  This package is free software; you can redistribute it and/or
--  modify it under terms of the GNU General Public License as
--  published by the Free Software Foundation; either version 3, or
--  (at your option) any later version.  It is distributed in the
--  hope that it will be useful, but WITHOUT ANY WARRANTY; without
--  even the implied warranty of MERCHANTABILITY or FITNESS FOR A
--  PARTICULAR PURPOSE.
--
--  As a special exception under Section 7 of GPL version 3, you are
--  granted additional permissions described in the GCC Runtime
--  Library Exception, version 3.1, as published by the Free Software
--  Foundation.
--
--  You should have received a copy of the GNU General Public License
--  and a copy of the GCC Runtime Library Exception along with this
--  program; see the files COPYING3 and COPYING.RUNTIME respectively.
--  If not, see <http://www.gnu.org/licenses/>.
--
--  Copyright Simon Wright <simon@pushface.org>

pragma License (Modified_GPL);

--  This package interfaces to the Raspberry Pi's I2C bus.
--
--  Child packages support different chips, eg the MCP23017 I/O
--  multiplexer.
--
--  Much detail is read across from the I2C Tools package.

with Ada.Finalization;
with GNAT.OS_Lib;
with Interfaces;

package I2C is

   type Bus_Address is range 16#00000# .. 16#fffff#;

   type Bus (Address : Bus_Address)
   is tagged limited private;

   type Chip_Address is range 16#03# .. 16#77#;

private

   type Bus (Address : Bus_Address)
   is new Ada.Finalization.Limited_Controlled with record
      FD : GNAT.OS_Lib.File_Descriptor := GNAT.OS_Lib.Invalid_FD;
   end record;

   overriding
   procedure Initialize (B : in out Bus);

   overriding
   procedure Finalize (B : in out Bus);

   type Chip (Address : Chip_Address; On_Bus : not null access Bus)
   is abstract new Ada.Finalization.Limited_Controlled with null record;

   overriding
   procedure Initialize (C : in out Chip);

   --  Support for child packages.

   type Register is range 16#00# .. 16#ff#;
   type Byte is new Interfaces.Unsigned_8;

   --  To start with, we only need byte read/write.

   not overriding
   procedure Set (C : Chip; R : Register; To : Byte);

   not overriding
   function Get (C : Chip; R : Register) return Byte;

end I2C;
