pragma Profile (Ravenscar);
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Real_Time; use Ada.Real_Time;
with Ada.Float_Text_IO; use Ada.Float_Text_IO;

with PCA9685;
with HMC5883L;
with MPU6050;
with I2C;

procedure Ravn is
   I2C_Bus : aliased I2C.Bus (Adapter_Number => 1); --  /dev/i2c-1
   PWM_Driver : PCA9685.Chip (On_Bus => I2C_Bus'Access,
                              Address => 16#40#);  --  default address
   Compass : HMC5883L.Chip (On_Bus => I2C_Bus'Access,
                            Address => 16#1E#);  --  default address
   IMU : MPU6050.Chip (On_Bus => I2C_Bus'Access,
                       Address => 16#68#); --  default address
begin
   PWM_Driver.Reset;
   PWM_Driver.SetPWMFreq (1000.0); --  Max frequency as per datasheet
   PWM_Driver.SetPin (61, 0); --  Initialize with all off
   Compass.Reset;
   IMU.Reset;

   if not Compass.Self_Test then
      Ada.Text_IO.Put_Line ("HMC5883L didn't pass self test");
   end if;
      declare
         bytes : constant I2C.Byte_Array (1 .. 16) := (others => I2C.Byte (0));
      begin
         Compass.Set_Declination (Degrees => 72, Minutes => 44); --  Oslo/NO
         Ada.Text_IO.Put_Line ("Setting 4 I2C outputs to 0 afap, op/s:");
         loop
            declare
               Finish_Time : Time;
               Start_Time : constant Time := Clock;
               Compass_Output : Float;
               IMU_Output : MPU6050.MPU6050_Output;
            begin

               for j in Integer range 0 .. 100 loop
                  I2C.Write_Array_Data (C => PWM_Driver,
                                        R => I2C.Register (50), --  Pin 11-15
                                        Values => bytes);
                  Compass_Output := Compass.Get_Heading;
                  IMU_Output := IMU.Get_Motion_6;
               end loop;

               Finish_Time := Clock;
               Ada.Float_Text_IO.Put (
                  100.0 / Float (To_Duration (Finish_Time - Start_Time)),
                  Fore => 4, Aft => 2, Exp => 0
               );
               Ada.Text_IO.New_Line;
               Ada.Float_Text_IO.Put (
                  Compass_Output,
                  Fore => 4, Aft => 2, Exp => 0
               );
               Ada.Text_IO.New_Line;
               Put_Line ("GX " & Integer'Image (
                  IMU_Output.Gyroscope_Output.X));
               Put_Line ("GY " & Integer'Image (
                  IMU_Output.Gyroscope_Output.Y));
               Put_Line ("GZ " & Integer'Image (
                  IMU_Output.Gyroscope_Output.Z));
               Put_Line ("Tmp " & Integer'Image (
                  IMU_Output.Thermometer_Output));
               Put_Line ("AX " & Integer'Image (
                  IMU_Output.Accelerometer_Output.X));
               Put_Line ("AY " & Integer'Image (
                  IMU_Output.Accelerometer_Output.Y));
               Put_Line ("AZ " & Integer'Image (
                  IMU_Output.Accelerometer_Output.Z));
            end;
         end loop;
      end;
end Ravn;
