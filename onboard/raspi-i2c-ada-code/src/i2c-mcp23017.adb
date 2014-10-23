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

with Ada.Text_IO; use Ada.Text_IO;
pragma Warnings (Off, Ada.Text_IO);

--  This package interfaces to the MCP23017 I/O multiplexer on the
--  Raspberry Pi's I2C bus.
--
--  The data sheet is at:
--  http://ww1.microchip.com/downloads/en/devicedoc/21952a.pdf.

package body I2C.MCP23017 is

   -----------------
   --  Utilities  --
   -----------------

   function Get_Bit (C    : Chip;
                     Base : Register;
                     Pin  : GPIO_Pin) return Boolean;

   procedure Set_Bit (C    : Chip;
                      Base : Register;
                      Pin  : GPIO_Pin;
                      To   : Boolean);

   -------------------------------
   --  Implementations of spec  --
   -------------------------------

   not overriding
   procedure Reset (C : in out Chip)
   is
      --  I'm going to assume that IOCON.BANK is left at 0, because
      --  the register mapping for IOCON depends on IOCON.BANK!
      --
      --  With IOCON.BANK = 0, the registers are in pairs, R for A,
      --  R+1 for B.
      procedure Set (R : Register; To : Byte);
      procedure Set (R : Register; To : Byte)
      is
         pragma Assert (R mod 2 = 0,
                        "Reset.Set: register must be even");
      begin
         C.Set (R, To => To);
         C.Set (R + 1, To => To);
      end Set;
   begin
      --  Clear [BANK], MIRROR, SEQOP, DISSLW, HAEN, ODR, INTPOL.
      --
      --  XXX can you have different values for the two banks, or is
      --  there in fact only one register?
      Set (R => IOCON, To => 2#0000_0000#);

      --  GPIO, clear all GPIOs
      Set (R => GPIO, To => 0);

      --  OLAT, clear output latches
      Set (R => OLAT, To => 0);

      --  IODIR, all lines inputs
      Set (R => IODIR, To => 16#ff#);

      --  IPOL, normal input polarity
      Set (R => IPOL, To => 16#00#);

      --  GPINTEN, interrupt disabled
      Set (R => GPINTEN, To => 16#00#);

      --  DEFVAL, default value for interrupt comparison
      Set (R => DEFVAL, To => 16#00#);

      --  INTCON, comparisons against previous value
      Set (R => INTCON, To => 16#00#);

      --  GPPU, pullup disabled
      Set (R => GPPU, To => 16#00#);

      --  INTF is read-only
      --  INTCAP is read-only
   end Reset;

   not overriding
   procedure Configure
     (C                          : in out Chip;
      Connect_Interrupt_Pins     :        Boolean := False;
      Interrupt_Pins_Open_Drain  :        Boolean := False;
      Interrupt_Pins_Active_High :        Boolean := False)
   is
      Setting : Byte := C.Get (R => IOCON);
   begin
      if Connect_Interrupt_Pins then
         Setting := Setting or MIRROR;
      end if;
      if Interrupt_Pins_Open_Drain then
         Setting := Setting or ODR;
      end if;
      if Interrupt_Pins_Active_High then
         Setting := Setting or INTPOL;
      end if;
      C.Set (R => IOCON, To => Setting);
   end Configure;

   not overriding
   procedure Configure
     (C                       : in out Chip;
      Pin                     :        GPIO_Pin;
      As_Input                :        Boolean  := True;
      Normal_Input_Polarity   :        Boolean  := True;
      Interrupt_Disabled      :        Boolean  := True;
      Default_Comparison      :        Boolean  := False;
      Interrupt_On_Any_Change :        Boolean  := True;
      Pullup_Enabled          :        Boolean  := False)
   is
   begin
      --  IODIR
      Set_Bit (C, Base => IODIR, Pin => Pin, To => As_Input);
      --  IPOL
      Set_Bit (C, Base => IPOL, Pin => Pin, To => not Normal_Input_Polarity);
      --  GPINTEN
      Set_Bit (C, Base => GPINTEN, Pin => Pin, To => not Interrupt_Disabled);
      --  DEFVAL
      Set_Bit (C, Base => DEFVAL, Pin => Pin, To => Default_Comparison);
      --  INTCON
      Set_Bit
        (C, Base => INTCON, Pin => Pin, To => not Interrupt_On_Any_Change);
      --  GPPU
      Set_Bit (C, Base => GPPU, Pin => Pin, To => Pullup_Enabled);
   end Configure;

   not overriding
   function Read (C : Chip; Pin : GPIO_Pin) return Boolean
   is
   begin
      return Get_Bit (C, Base => GPIO, Pin => Pin);
   end Read;

   not overriding
   procedure Write (C : Chip; Pin : GPIO_Pin; Item : Boolean)
   is
   begin
      Set_Bit (C, Base => GPIO, Pin => Pin, To => Item);
   end Write;

   not overriding
   procedure Register_Callback (C        : in out Chip;
                                Callback :        Callback_Procedure)
   is
   begin
      C.Callback := Callback;
   end Register_Callback;

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

   -------------------------------
   --  Utility implementations  --
   -------------------------------

   function Get_Bit (C    : Chip;
                     Base : Register;
                     Pin  : GPIO_Pin) return Boolean
   is
      Bank_A : constant Boolean := Pin in A0 .. A7;
      Reg : constant Register := Base + Boolean'Pos (not Bank_A);
      Bit : constant Natural := GPIO_Pin'Pos (Pin) mod 8;
      Mask : constant Byte := Shift_Left (1, Bit);
      Register_Value : constant Byte := C.Get (Reg);
   begin
      return (Register_Value and Mask) /= 0;
   end Get_Bit;

   procedure Set_Bit (C    : Chip;
                      Base : Register;
                      Pin  : GPIO_Pin;
                      To   : Boolean)
   is
      Bank_A : constant Boolean := Pin in A0 .. A7;
      Reg : constant Register := Base + Boolean'Pos (not Bank_A);
      Bit : constant Natural := GPIO_Pin'Pos (Pin) mod 8;
      Mask : constant Byte := Shift_Left (1, Bit);
      Value : Byte;
   begin
      Value := C.Get (Reg);
      if To then
         Value := Value or Mask;
      else
         Value := Value and (not Mask);
      end if;
      C.Set (R => Reg, To => Value);
   end Set_Bit;

end I2C.MCP23017;
