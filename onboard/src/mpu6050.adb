with Interfaces; use Interfaces;

package body MPU6050 is

   --  Reset the chip to the power-on reset state.
   procedure Reset (C : in out Chip) is
   begin
      null;
   end Reset;

   procedure Initialize (C : in out Chip) is
   begin
      null;
   end Initialize;

   function Test_Connection (C : in out Chip) return Boolean is
      Device_ID : constant Byte := C.Get_Device_Id;
   begin
      return Integer (Device_ID) = 16#34#;
   end Test_Connection;

   function Get_Motion_6 (C : in out Chip) return MPU6050_6DOF_Output is
      Output : MPU6050_Output;
:     Data : constant Byte_Array :=
         C.Read_Array_Data (MPU6050_RA_ACCEL_XOUT_H, 12);
   begin
      Output.Accelerometer_Output.X.H := Integer (To_2_Complement (Data (0)));
      Output.Accelerometer_Output.X.L := Integer (To_2_Complement (Data (1)));
      Output.Accelerometer_Output.Y.H := Integer (To_2_Complement (Data (2)));
      Output.Accelerometer_Output.Y.L := Integer (To_2_Complement (Data (3)));
      Output.Accelerometer_Output.Z.H := Integer (To_2_Complement (Data (4)));
      Output.Accelerometer_Output.Z.L := Integer (To_2_Complement (Data (5)));
      Output.Gyroscope_Output.X.H := Integer (To_2_Complement (Data (6)));
      Output.Gyroscope_Output.X.L := Integer (To_2_Complement (Data (7)));
      Output.Gyroscope_Output.Y.H := Integer (To_2_Complement (Data (8)));
      Output.Gyroscope_Output.Y.L := Integer (To_2_Complement (Data (9)));
      Output.Gyroscope_Output.Z.H := Integer (To_2_Complement (Data (10)));
      Output.Gyroscope_Output.Z.L := Integer (To_2_Complement (Data (11)));
      return Output;
   end Get_Motion_6;

   --  Get Device ID should be 0b110100, 0x34.
   function Get_Device_Id (C : in out Chip) return I2C.Byte is
      Device_ID : constant Byte :=
         C.Read_Bits_Data (MPU6050_RA_WHO_AM_I,
                           MPU6050_WHO_AM_I_BIT,
                           MPU6050_WHO_AM_I_LENGTH);
   begin
      return Device_ID;
   end Get_Device_Id;
end MPU6050;
