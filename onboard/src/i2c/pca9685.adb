package body PCA9685 is
   procedure Reset (C : in out Chip)
   is
   begin
      I2C.Set (C => C, R => MODE1, To => 16#0#);
   end Reset;
end PCA9685;
