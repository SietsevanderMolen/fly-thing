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

with AHRS;

procedure Ravn is
   package Float_Quaternion is new Quaternions (Float);
   use Float_Quaternion;

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
      Result : Quaternion;
      Algorithm : AHRS.Mahony := AHRS.Make (Sample_Period => 1.0,
                                            Proportional_Gain => 1.0,
                                            Integral_Gain => 1.0);
   begin
      loop
         Compass_Output := Compass.Get_Axes;
         IMU_Output := IMU.Get_Motion_6;
         AHRS.Update (M => Algorithm,
                      Gyroscope => IMU_Output.Gyroscope_Output,
                      Accelerometer => IMU_Output.Accelerometer_Output);
         Ada.Text_IO.Put_Line ("Q: " & Image (Result));
      end loop;
   end;
end Ravn;
