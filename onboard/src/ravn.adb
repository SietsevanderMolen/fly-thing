with Ada.Text_IO;
with Ada.Real_Time; use Ada.Real_Time;
with Ada.Float_Text_IO; use Ada.Float_Text_IO;

with PCA9685;
with HMC5883L;
with I2C;

procedure Ravn is
   I2C_Bus : aliased I2C.Bus (Adapter_Number => 1); --  /dev/i2c-1
   PWM_Driver : PCA9685.Chip (On_Bus => I2C_Bus'Access,
                              Address => 16#40#);  --  default address
   Compass : HMC5883L.Chip (On_Bus => I2C_Bus'Access,
                            Address => 16#1E#);  --  default address
   Passed_Selftest : Boolean := False;
begin
   PWM_Driver.Reset;
   PWM_Driver.SetPWMFreq (1000.0); --  Max frequency as per datasheet
   PWM_Driver.SetPin (61, 0); --  Initialize with all off
   Compass.Reset;
   Compass.Set_Declination (Degrees => 72, Minutes => 44); --  Oslo/NO
   if Compass.Self_Test then
      declare
         bytes : constant I2C.Byte_Array (1 .. 16) := (others => I2C.Byte (0));
      begin
         Ada.Text_IO.Put_Line ("Setting 4 I2C outputs to 0 afap, op/s:");
         loop
            declare
               Finish_Time : Time;
               Start_Time : constant Time := Clock;
               Compass_Output : Float;
            begin

               for j in Integer range 0 .. 1000 loop
                  I2C.Write_Array_Data (C => PWM_Driver,
                                        R => I2C.Register (50), --  Pin 11-15
                                        Values => bytes);
                  Compass_Output := Compass.Get_Heading;
               end loop;

               Finish_Time := Clock;
               Ada.Float_Text_IO.Put (
                  1000.0 / Float (To_Duration (Finish_Time - Start_Time)),
                  Fore => 4, Aft => 2, Exp => 0
               );
               Ada.Text_IO.New_Line;
               Ada.Float_Text_IO.Put (
                  Compass_Output,
                  Fore => 4, Aft => 2, Exp => 0
               );
               Ada.Text_IO.New_Line;
            end;
         end loop;
      end;
   else
      Ada.Text_IO.Put_Line ("HMC5883L didn't pass self test");
   end if;
end Ravn;
