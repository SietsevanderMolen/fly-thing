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

--  This package interfaces to the MCP23017 I/O multiplexer on the
--  Raspberry Pi's I2C bus.

package I2C.MCP23017 is

   --  A Chip is at an Address on the I2C bus On_Bus.
   --
   --  This type is abstract because there are different styles of
   --  detecting pin change and making the required callback (if any);
   --  for example, polling or interrupt-driven.
   type Chip (Address : Chip_Address; On_Bus : not null access Bus)
   is abstract tagged limited private;

   type GPIO_Pin is (A0, A1, A2, A3, A4, A5, A6, A7,
                     B0, B1, B2, B3, B4, B5, B6, B7);

   --  Reset the chip to the power-on reset state.
   not overriding
   procedure Reset (C : in out Chip);

   not overriding
   procedure Configure
     (C                          : in out Chip;
      Connect_Interrupt_Pins     :        Boolean := False;
      Interrupt_Pins_Open_Drain  :        Boolean := False;
      Interrupt_Pins_Active_High :        Boolean := False);
   --  Configure the chip as a whole.
   --  The default values indicated are the reset values.

   not overriding
   procedure Configure
     (C                       : in out Chip;
      Pin                     :        GPIO_Pin;
      As_Input                :        Boolean  := True;
      Normal_Input_Polarity   :        Boolean  := True;
      Interrupt_Disabled      :        Boolean  := True;
      Default_Comparison      :        Boolean  := False;
      Interrupt_On_Any_Change :        Boolean  := True;
      Pullup_Enabled          :        Boolean  := False);
   --  Configure a Pin.
   --  The default values indicated are the reset values.

   not overriding
   function Read (C : Chip; Pin : GPIO_Pin) return Boolean;

   not overriding
   procedure Write (C : Chip; Pin : GPIO_Pin; Item : Boolean);

   type Callback_Procedure is access procedure (Pin_Changed : GPIO_Pin;
                                                To          : Boolean);

   not overriding
   procedure Register_Callback (C        : in out Chip;
                                Callback :        Callback_Procedure);
   --  if Callback is null, any existing callback is deregistered.

private

   --  Name the chip's registers. Note, this naming assumes that
   --  IOCON.BANK is clear, so that (with the exception of IOCON
   --  itself) the B registers are 1 higher than the A registers; the
   --  name given is that of the A register.

   --  Read/write
   IOCON   : constant Register := 16#0a#;
   GPIO    : constant Register := 16#12#;
   OLAT    : constant Register := 16#14#;
   IODIR   : constant Register := 16#00#;
   IPOL    : constant Register := 16#02#;
   GPINTEN : constant Register := 16#04#;
   DEFVAL  : constant Register := 16#06#;
   INTCON  : constant Register := 16#08#;
   GPPU    : constant Register := 16#0c#;

   --  Read-only
   INTF    : constant Register := 16#0e#;
   INTCAP  : constant Register := 16#10#;

   --  Name the IOCON bits.
   BANK   : constant Byte := 2 ** 7; -- LEAVE THIS UNSET!
   MIRROR : constant Byte := 2 ** 6;
   SEQOP  : constant Byte := 2 ** 5;
   DISSLW : constant Byte := 2 ** 4;
   HAEN   : constant Byte := 2 ** 3;
   ODR    : constant Byte := 2 ** 2;
   INTPOL : constant Byte := 2 ** 1;
   --  Bit 0 is unused.

   type Chip (Address : Chip_Address; On_Bus : not null access Bus)
      is abstract new I2C.Chip (Address => Address, On_Bus => On_Bus)
   with record
      Callback : Callback_Procedure;
   end record;

   overriding
   procedure Initialize (C : in out Chip);

   overriding
   procedure Finalize (C : in out Chip);

end I2C.MCP23017;
