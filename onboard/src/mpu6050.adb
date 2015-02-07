with Interfaces; use Interfaces;
with Ada.Text_IO;

package body MPU6050 is

   --  Reset the chip to the power-on reset state.
   procedure Reset (C : in out Chip) is
   begin
      C.Set_Clock_Source (S => MPU6050_CLOCK_PLL_XGYRO);
      C.Set_Full_Scale_Gyro_Range (R => MPU6050_GYRO_FS_250);
      C.Set_Full_Scale_Accel_Range (R => MPU6050_ACCEL_FS_2);
      C.Set_Sleep (S => False);
      C.Initialize_DMP;
   end Reset;

   --  Reset the chip to the power-on reset state.
   procedure Initialize_DMP (C : in out Chip) is
      HW_Revision : Byte := 0;
   begin
      C.Set_Memory_Bank (Bank => 16#00#, Prefetch => True, User_Bank => True);
      C.Set_Memory_Start_Address (Address => 16#06#);
      HW_Revision := C.Read_Memory_Byte;
      Ada.Text_IO.Put_Line ("HW Revision: " & Byte'Image (HW_Revision));
   end Initialize_DMP;

   function Test_Connection (C : in Chip) return Boolean is
      Device_ID : constant Byte := C.Get_Device_Id;
   begin
      return Integer (Device_ID) = 16#34#;
   end Test_Connection;

   function Get_Motion_6 (C : in Chip) return MPU6050_Output is
      Output : MPU6050_Output;
      Data : constant Byte_Array :=
         C.Read_Array_Data (MPU6050_RA_ACCEL_XOUT_H, 14);
      A_X, A_Y, A_Z : Axis_Reading;
      G_X, G_Y, G_Z : Axis_Reading;
      Tmp           : Axis_Reading;
   begin
      A_X.H := Data (0);
      A_X.L := Data (1);
      A_Y.H := Data (2);
      A_Y.L := Data (3);
      A_Z.H := Data (4);
      A_Z.L := Data (5);
      Tmp.H := Data (6);
      Tmp.L := Data (7);
      G_X.H := Data (8);
      G_X.L := Data (9);
      G_Y.H := Data (10);
      G_Y.L := Data (11);
      G_Z.H := Data (12);
      G_Z.L := Data (13);

      Output.Accelerometer_Output.X := Integer (Pack (A_X));
      Output.Accelerometer_Output.Y := Integer (Pack (A_Y));
      Output.Accelerometer_Output.Z := Integer (Pack (A_Z));
      Output.Thermometer_Output := Integer (Pack (Tmp));
      Output.Gyroscope_Output.X := Integer (Pack (G_X));
      Output.Gyroscope_Output.Y := Integer (Pack (G_Y));
      Output.Gyroscope_Output.Z := Integer (Pack (G_Z));
      return Output;
   end Get_Motion_6;

   --  Get Device ID should be 0b110100, 0x34.
   function Get_Device_Id (C : in Chip) return I2C.Byte is
      Device_ID : constant Byte :=
         C.Read_Bits_Data (MPU6050_RA_WHO_AM_I,
                           MPU6050_WHO_AM_I_BIT,
                           MPU6050_WHO_AM_I_LENGTH);
   begin
      return Device_ID;
   end Get_Device_Id;

   procedure Set_Clock_Source (C : in out Chip; S : Clock_Source) is
      Power_Management : PWR_MGMT_1 :=
         Unpack (C.Read_Byte_Data (PWR_MGMT_1_Address));
   begin
      Power_Management.Clock_Sel := S;
      C.Write_Byte_Data (R => PWR_MGMT_1_Address,
                         D => Pack (Power_Management));
   end Set_Clock_Source;

   procedure Set_Full_Scale_Gyro_Range (C : in out Chip; R : Gyro_Scale_Range)
   is
      Gyro_Setting : GYRO_CONFIG :=
         Unpack (C.Read_Byte_Data (GYRO_CONFIG_Address));
   begin
      Gyro_Setting.FS_SEL := R;
      C.Write_Byte_Data (R => GYRO_CONFIG_Address,
                         D => Pack (Gyro_Setting));
   end Set_Full_Scale_Gyro_Range;

   procedure Set_Full_Scale_Accel_Range (C : in out Chip;
                                         R : Accel_Scale_Range) is
      Accel_Setting : ACCEL_CONFIG :=
         Unpack (C.Read_Byte_Data (ACCEL_CONFIG_Address));
   begin
      Accel_Setting.AFS_SEL := R;
      C.Write_Byte_Data (R => ACCEL_CONFIG_Address,
                         D => Pack (Accel_Setting));
   end Set_Full_Scale_Accel_Range;

   procedure Set_Memory_Bank (C : in Chip;
                              Bank : Natural;
                              Prefetch : Boolean;
                              User_Bank : Boolean) is
      B : BANK_SEL;
   begin
      B.MEM_SEL := Bank;
      B.PRFTCH_EN := 0; --  bank &= 0x1F
      B.CFG_USER_BANK := Boolean'Pos (User_Bank);
      B.PRFTCH_EN := Boolean'Pos (Prefetch);
      C.Write_Byte_Data (R => MPU6050_RA_BANK_SEL, D => Pack (B));
   end Set_Memory_Bank;

   procedure Set_Memory_Start_Address (C : in Chip;
                                       Address : Natural) is
   begin
      C.Write_Byte_Data (R => MPU6050_RA_MEM_START_ADDR, D => Byte (Address));
   end Set_Memory_Start_Address;

   function Read_Memory_Byte (C : in Chip) return Byte is
   begin
      return C.Read_Byte_Data (MPU6050_RA_MEM_R_W);
   end Read_Memory_Byte;

   procedure Set_Sleep (C : in out Chip;
                        S : Boolean) is
      Power_Management : PWR_MGMT_1 :=
         Unpack (C.Read_Byte_Data (PWR_MGMT_1_Address));
   begin
      if S then
         Power_Management.Sleep := 1;
      else
         Power_Management.Sleep := 0;
      end if;
      C.Write_Byte_Data (R => PWR_MGMT_1_Address,
                         D => Pack (Power_Management));
   end Set_Sleep;
end MPU6050;
