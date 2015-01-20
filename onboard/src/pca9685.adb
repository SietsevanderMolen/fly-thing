with Interfaces.C;
with i2c_interface_c;

package body PCA9685 is
   procedure Reset (C : in out Chip)
   is
   begin
      I2C.Write_Byte_Data (C => C, R => MODE1, To => 16#0#);
   end Reset;

   procedure SetPWMFreq (C : in out Chip; Frequency : Float) is
      Freq : constant Float := Frequency * 0.9;
      --  Estimated prescale
      Prescale_Value : constant Float :=
         (PCA9685.Clock / Resolution / Freq) - 1.0;
      --  Used prescale
      Prescale : constant Unsigned_8 :=
         Unsigned_8 (Float'Floor (Prescale_Value + 0.5));
      Old_Mode : constant Unsigned_8 := Unsigned_8 (C.Read_Byte_Data (MODE1));
      New_Mode : constant Unsigned_8 := (Old_Mode and 16#7F#) or 16#10#;
   begin
      C.Write_Byte_Data (R => PCA9685.MODE1, To => Byte (New_Mode)); --  Sleep
      C.Write_Byte_Data (R => PCA9685.PRESCALE, To => Byte (Prescale));
      C.Write_Byte_Data (R => PCA9685.MODE1, To => Byte (Old_Mode));
      --  Possible add a delay here TODO
      --  Auto inc mode1
      C.Write_Byte_Data (R => PCA9685.MODE1, To => Byte (Old_Mode or 16#A1#));
   end SetPWMFreq;

   procedure SetPWM (C : in out Chip;
                     Pin : Unsigned_8;
                     On : Unsigned_16;
                     Off : Unsigned_16) is
      bytes : i2c_interface_c.Byte_Array (1 .. 4);
   begin
      bytes (1) := Interfaces.C.unsigned_char (On and 255);
      bytes (2) := Interfaces.C.unsigned_char (Shift_Right (On, 8) and 255);
      bytes (3) := Interfaces.C.unsigned_char (Off and 255);
      bytes (4) := Interfaces.C.unsigned_char (Shift_Right (Off, 8) and 255);
      I2C.Write_Array (C => C,
                       R => Register (Unsigned_8 (LED0_ON_L) + (4 * Pin)),
                       Values => bytes);
   end SetPWM;

   procedure SetPin (C : in out Chip;
                     Pin : Unsigned_8;
                     Value : Unsigned_16;
                     Invert : Boolean := False) is
   begin
      if Invert then
         if Value = 0 then --  Fully on
            C.SetPWM (Pin => Pin, On => 4096, Off => 0);
         elsif Value = 4095 then --  Fully off
            C.SetPWM (Pin => Pin, On => 0, Off => 4096);
         else
            C.SetPWM (Pin => Pin, On => 0, Off => 4095-Value);
         end if;
      else
         if Value = 4095 then --  Fully on
            C.SetPWM (Pin => Pin, On => 4096, Off => 0);
         elsif Value = 0 then --  Fully off
            C.SetPWM (Pin => Pin, On => 0, Off => 4096);
         else
            C.SetPWM (Pin => Pin, On => 0, Off => Value);
         end if;
      end if;
   end SetPin;
end PCA9685;
