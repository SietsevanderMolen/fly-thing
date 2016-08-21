pragma Profile (Ravenscar);

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Real_Time; use Ada.Real_Time;
with Ada.Float_Text_IO; use Ada.Float_Text_IO;

with Vector_Math; use Vector_Math;
with Quaternions;

with PCA9685;
with HMC5883L;
with MPU6050;
with I2C;

with AHRS; use AHRS;
with PIDs; use PIDs;

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

   Compass.Set_Declination (Degrees => 72, Minutes => 44); --  Oslo/NO

   declare
      Compass_Output : Vector_Math.Float3;
      IMU_Output : MPU6050.MPU6050_Output;
      Algorithm : AHRS.Mahony := AHRS.Make (Sample_Period => 1.0/200.0,
                                            Proportional_Gain => 2.0,
                                            Integral_Gain => 0.005);
      Pid : PIDs.PID := PIDs.Make (Kp => 4.0,
                                   Ki => 0.02,
                                   Kd => 15.0,
                                   Output_Min => 0.0,
                                   Output_Max => 100.0,
                                   Setpoint => 50.0,
                                   Sample_Rate => 200);
   begin
      declare
         Next : Ada.Real_Time.Time;
      begin
         Next := Clock;
         loop
            Compass_Output := Compass.Get_Axes;
            IMU_Output := IMU.Get_Motion_6;

            AHRS.Update (M => Algorithm,
                         Gyroscope => IMU_Output.Gyroscope_Output,
                         Accelerometer => IMU_Output.Accelerometer_Output,
                         Magnetometer => Compass_Output);

            Put ("raw: ");
            Ada.Float_Text_IO.Put (Item => IMU_Output.Accelerometer_Output.x,
                                   Fore => 4,
                                   Aft  => 0,
                                   Exp  => 0);
            Put (", ");
            Ada.Float_Text_IO.Put (Item => IMU_Output.Accelerometer_Output.y,
                                   Fore => 4,
                                   Aft  => 0,
                                   Exp  => 0);
            Put (", ");
            Ada.Float_Text_IO.Put (Item => IMU_Output.Accelerometer_Output.z,
                                   Fore => 4,
                                   Aft  => 0,
                                   Exp  => 0);
            Put (", ");
            Ada.Float_Text_IO.Put (Item => Compass_Output.x,
                                   Fore => 4,
                                   Aft  => 2,
                                   Exp  => 0);

            Ada.Text_IO.Put_Line (". AHRS: "
               & Float_Quaternion.Image (Algorithm.Output));

            delay until Next;
            Next := Next + Ada.Real_Time.To_Time_Span (1.0/200.0);
         end loop;
      end;
   end;
end Ravn;
