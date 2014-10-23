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
