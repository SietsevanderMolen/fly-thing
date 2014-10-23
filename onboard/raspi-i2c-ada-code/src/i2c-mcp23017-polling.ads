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
--  Raspberry Pi's I2C bus, providing polling support for pin change
--  detection.

package I2C.MCP23017.Polling is

   --  A Chip is at an Address on the I2C bus On_Bus. It is polled at
   --  regular intervals (default 0.1 second, can be changed by
   --  Set_Polling_Interval).
   --
   --  When a pin is determined to have changed, the registered
   --  callback procedure (see package I2C.MCP23017) is called.
   type Chip (Address : Chip_Address; On_Bus : not null access Bus)
   is new MCP23017.Chip with private;

   procedure Set_Polling_Interval (For_Chip : in out Chip; To : Duration);

private

   task type Polling_Task (For_Chip : not null access Chip) is
      entry Stop;
   end Polling_Task;
   type Polling_Task_P is access Polling_Task;

   type Chip (Address : Chip_Address; On_Bus : not null access Bus)
   is new MCP23017.Chip (Address => Address, On_Bus => On_Bus)
   with record
      Polling_Interval : Duration := 0.1;
      Poller           : Polling_Task_P;
   end record;

   overriding
   procedure Initialize (C : in out Chip);

   overriding
   procedure Finalize (C : in out Chip);

end I2C.MCP23017.Polling;
