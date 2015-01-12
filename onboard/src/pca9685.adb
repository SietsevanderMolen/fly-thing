package body PCA9685 is
   procedure Reset (C : in out Chip)
   is
   begin
      I2C.Set (C => C, R => MODE1, To => 16#0#);
   end Reset;

   --  Set the PWM Frequency to use for all outputs
   procedure SetPWMFreq (C : in out Chip; Frequency : Float) is
      Freq : constant Float := Frequency * 0.9;
      --  Estimated prescale
      Prescale_Value : constant Float :=
         (PCA9685.Clock / PCA9685.Resolution / Freq) - 1.0;
      --  Used prescale
      Prescale : constant Unsigned_8 :=
         Unsigned_8 (Float'Floor (Prescale_Value + 0.5));
      Old_Mode : constant Unsigned_8 := C.Get (MODE1);
      New_Mode : constant Unsigned_8 := (Old_Mode and 16#7F#) or 16#10#;
   begin
      C.Set (R => PCA9685.MODE1, To => New_Mode); --  Sleep
      C.Set (R => PCA9685.PRESCALE, To => Prescale);
      C.Set (R => PCA9685.MODE1, To => Old_Mode);
      --  Possible add a delay here TODO
      C.Set (R => PCA9685.MODE1, To => Old_Mode or 16#A1#); --  Auto inc mode1
   end SetPWMFreq;

   --  Set PWM for given pin
   procedure SetPWM (C : in out Chip;
                     Pin : Unsigned_8;
                     On : Unsigned_16;
                     Off : Unsigned_16) is
   begin
      C.Write_Byte (Data => Unsigned_8 (LED0_ON_L) + 4 * Pin);
      C.Write_Byte (Data => Unsigned_8 (On and 255));
      C.Write_Byte (Data => Unsigned_8 (Shift_Right (On, 8) and 255));
      C.Write_Byte (Data => Unsigned_8 (Off and 255));
      C.Write_Byte (Data => Unsigned_8 (Shift_Right (Off, 8) and 255));
   end SetPWM;

   --  Sets pin without having to deal with on/off tick placement and properly
   --  handles a zero value as completely off. Optional invert parameter
   --  supports inverting the pulse for sinking to ground. Val should be a
   --  value from 0 to 4095 inclusive.
   procedure SetPin (C : in out Chip;
                     Pin : Unsigned_8;
                     Value : Unsigned_16;
                     Invert : Boolean := False) is
      Val : constant Unsigned_16 := Unsigned_16'Min (Value, 4095);
   begin
      if Invert then
         if Val = 0 then --  Fully on
            C.SetPWM (Pin => Pin, On => 4096, Off => 0);
         elsif Val = 4095 then --  Fully off
            C.SetPWM (Pin => Pin, On => 0, Off => 4096);
         else
            C.SetPWM (Pin => Pin, On => 0, Off => 4095-Val);
         end if;
      else
         if Val = 4095 then --  Fully on
            C.SetPWM (Pin => Pin, On => 4096, Off => 0);
         elsif Val = 0 then --  Fully off
            C.SetPWM (Pin => Pin, On => 0, Off => 4096);
         else
            C.SetPWM (Pin => Pin, On => 0, Off => Val);
         end if;
      end if;
   end SetPin;
end PCA9685;
