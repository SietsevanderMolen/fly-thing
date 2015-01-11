package body PCA9685 is
   procedure Reset (C : in out Chip)
   is
   begin
      I2C.Set (C => C, R => MODE1, To => 16#0#);
   end Reset;

   procedure SetPWMFreq (C : in out Chip; Frequency : Float) is
   begin
      null;
   end SetPWMFreq;

   procedure SetPWM (C : in out Chip;
                     Number : I2C.Byte;
                     On : Integer;
                     Off : Integer) is
   begin
      null;
   end SetPWM;

   procedure SetPin (C : in out Chip;
                     Number : I2C.Byte;
                     Value : Integer;
                     Invert : Boolean := False) is
   begin
      null;
   end SetPin;

   procedure Write (C : in out Chip;
                    Address : I2C.Byte;
                    Value : I2C.Byte) is
   begin
      null;
   end Write;
end PCA9685;
