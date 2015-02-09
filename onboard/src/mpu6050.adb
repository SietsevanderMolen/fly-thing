with Interfaces; use Interfaces;
with Ada.Text_IO;

with I2C; use type I2C.Byte_Array;

package body MPU6050 is

   --  Reset the chip to the power-on reset state.
   procedure Reset (C : in out Chip) is
   begin
      C.Set_Full_Scale_Gyro_Range (R => MPU6050_GYRO_FS_250);
      C.Set_Full_Scale_Accel_Range (R => MPU6050_ACCEL_FS_2);
      C.Set_Sleep (S => False);
      C.Initialize_DMP;
   end Reset;

   --  Reset the chip to the power-on reset state.
   procedure Initialize_DMP (C : in out Chip) is
      HW_Revision : Byte := 0;
   begin
      C.Set_Sleep (S => False);
      C.Set_Memory_Bank (Bank => 16#10#, Prefetch => True, User_Bank => True);
      C.Set_Memory_Start_Address (Address => 16#06#);
      HW_Revision := C.Read_Memory_Byte;
      Ada.Text_IO.Put_Line ("HW Revision: " & Byte'Image (HW_Revision));
      C.Set_Memory_Bank (Bank => 0, Prefetch => False, User_Bank => False);

      --  write dmp code
      C.Write_Memory_Block (Data => MPU_Progmem);
      --  write dmp config
      C.Write_DMP_Configuration (Data => MPU_Config);
      C.Set_Clock_Source (S => MPU6050_CLOCK_PLL_ZGYRO);
      C.Set_DMP_Interrupt (True);
      C.Set_Fifo_Overflow_Interrupt (True);
      C.Set_Sample_Rate_Divider (4); --  1khz / (1+4) = 200hz
      C.Set_External_Frame_Sync (MPU6050_EXT_SYNC_TEMP_OUT_L);
      C.Set_DLPF_Mode (MPU6050_DLPF_BW_42); --  42hz
      C.Set_Full_Scale_Gyro_Range (R => MPU6050_GYRO_FS_2000);
      C.Write_Byte_Data (R => MPU6050_RA_DMP_CFG_1, D => 16#3#); --  funct unk
      C.Write_Byte_Data (R => MPU6050_RA_DMP_CFG_2, D => 16#0#); --  funct unk
      C.Set_OTP_Bank_Valid (False);
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
                              Bank : Memory_Bank;
                              Prefetch : Boolean := False;
                              User_Bank : Boolean := False) is
      B : BANK_SEL;
   begin
      B.MEM_SEL := Bank;
      B.PRFTCH_EN := 0; --  bank &= 0x1F
      B.CFG_USER_BANK := Boolean'Pos (User_Bank);
      B.PRFTCH_EN := Boolean'Pos (Prefetch);
      C.Write_Byte_Data (R => MPU6050_RA_BANK_SEL, D => Pack (B));
   end Set_Memory_Bank;

   procedure Set_Memory_Start_Address (C : in Chip;
                                       Address : Memory_Address) is
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
      Power_Management.Sleep := Boolean'Pos (S);
      C.Write_Byte_Data (R => PWR_MGMT_1_Address,
                         D => Pack (Power_Management));
   end Set_Sleep;

   procedure Set_Motion_Detect_Interrupt (C : in out Chip;
                                          S : Boolean) is
      Interrupts : INT_ENABLE :=
         Unpack (C.Read_Byte_Data (INT_ENABLE_Address));
   begin
      Interrupts.MOT_EN := Boolean'Pos (S);
      C.Write_Byte_Data (R => INT_ENABLE_Address,
                         D => Pack (Interrupts));
   end Set_Motion_Detect_Interrupt;

   procedure Set_Fifo_Overflow_Interrupt (C : in out Chip;
                                          S : Boolean) is
      Interrupts : INT_ENABLE :=
         Unpack (C.Read_Byte_Data (INT_ENABLE_Address));
   begin
      Interrupts.FIFO_OFLOW_EN := Boolean'Pos (S);
      C.Write_Byte_Data (R => INT_ENABLE_Address,
                         D => Pack (Interrupts));
   end Set_Fifo_Overflow_Interrupt;

   procedure Set_I2C_Master_Interrupt (C : in out Chip;
                                       S : Boolean) is
      Interrupts : INT_ENABLE :=
         Unpack (C.Read_Byte_Data (INT_ENABLE_Address));
   begin
      Interrupts.I2C_MST_EN := Boolean'Pos (S);
      C.Write_Byte_Data (R => INT_ENABLE_Address,
                         D => Pack (Interrupts));
   end Set_I2C_Master_Interrupt;

   procedure Set_DMP_Interrupt (C : in out Chip;
                                S : Boolean) is
      Interrupts : INT_ENABLE :=
         Unpack (C.Read_Byte_Data (INT_ENABLE_Address));
   begin
      Interrupts.DMP_EN := Boolean'Pos (S);
      C.Write_Byte_Data (R => INT_ENABLE_Address,
                         D => Pack (Interrupts));
   end Set_DMP_Interrupt;

   procedure Set_Data_Ready_Interrupt (C : in out Chip;
                                       S : Boolean) is
      Interrupts : INT_ENABLE :=
         Unpack (C.Read_Byte_Data (INT_ENABLE_Address));
   begin
      Interrupts.DATA_RDY_EN := Boolean'Pos (S);
      C.Write_Byte_Data (R => INT_ENABLE_Address,
                         D => Pack (Interrupts));
   end Set_Data_Ready_Interrupt;

   procedure Set_Sample_Rate_Divider (C : in out Chip;
                                      D : in Byte) is
   begin
      C.Write_Byte_Data (R => SMPLRT_DIV_Address,
                         D => D);
   end Set_Sample_Rate_Divider;

   procedure Set_External_Frame_Sync (C : in out Chip;
                                      P : in External_Frame_Sync_Pin) is
      Conf : CONFIG := Unpack (C.Read_Byte_Data (CONFIG_Address));
   begin
      Conf.Ext_Sync_Set := P;
      C.Write_Byte_Data (R => CONFIG_Address,
                         D => Pack (Conf));
   end Set_External_Frame_Sync;

   procedure Set_DLPF_Mode (C : in out Chip;
                            S : in Digital_Low_Pass_Filter_Setting) is
      Conf : CONFIG := Unpack (C.Read_Byte_Data (CONFIG_Address));
   begin
      Conf.Dlpf_Cfg := S;
      C.Write_Byte_Data (R => CONFIG_Address,
                         D => Pack (Conf));
   end Set_DLPF_Mode;

   procedure Set_OTP_Bank_Valid (C : in out Chip;
                                 V : in Boolean) is
      Conf : XG_OFFS_TC := Unpack (C.Read_Byte_Data (XG_OFFS_TC_Address));
   begin
      Conf.OTP_Valid := Boolean'Pos (V);
      C.Write_Byte_Data (R => XG_OFFS_TC_Address,
                         D => Pack (Conf));
   end Set_OTP_Bank_Valid;

   procedure Write_Memory_Block (C : in Chip;
                                 Data : in Byte_Array;
                                 Bank : in Memory_Bank := 0;
                                 Address : in Memory_Address := 0;
                                 Verify : in Boolean := True) is
      Chunk_Size : constant Natural := MPU6050_DMP_MEMORY_CHUNK_SIZE;
      Chunks : constant Natural := Data'Length / Chunk_Size;
      Rem_Bytes : constant Natural := Data'Length mod Chunk_Size;
      Current_Bank : Memory_Bank := Bank;
      Current_Address : Memory_Address := Address;
      Verify_Buffer : Byte_Array (0 .. Chunk_Size - 1);
      Verification_Failed : exception;
   begin
      C.Set_Memory_Bank (Current_Bank);
      C.Set_Memory_Start_Address (Current_Address);

      for I in 0 .. Chunks - 1 loop
         --  Write current chunk
         C.Write_Array_Data (R => MPU6050_RA_MEM_R_W,
                             Values => Data ((I * Chunk_Size) + Data'First ..
                                             (I * Chunk_Size) + Data'First
                                                + Chunk_Size));
         if Verify then
            Verify_Buffer := C.Read_Array_Data (R => MPU6050_RA_MEM_R_W,
                                                L => Chunk_Size);
            if Verify_Buffer /= Data ((I * Chunk_Size) + Data'First ..
                                      (I * Chunk_Size) + Data'First
                                         + Chunk_Size)
            then
               raise Verification_Failed;
            end if;
         end if;

         --  Update address and bank
         if Current_Address + Chunk_Size < 256 then
            Current_Address := Current_Address + Chunk_Size;
            C.Set_Memory_Start_Address (Current_Address);
         else --  Handle overflow to next bank
            Current_Address := 0;
            C.Set_Memory_Start_Address (Current_Address);
            Current_Bank := Current_Bank + 1;
            C.Set_Memory_Bank (Current_Bank);
         end if;
      end loop;
      --  Write remainder
      if Rem_Bytes > 0 then
         C.Write_Array_Data (R => MPU6050_RA_MEM_R_W,
            Values => Data (Data'First + (Chunks * Chunk_Size) ..
                            Data'Last));
      end if;
   end Write_Memory_Block;

   procedure Write_DMP_Configuration (C : in Chip;
                                      Data : in Byte_Array) is
      I : Integer := Data'First; --  Data loop counter
   begin
      while I < Data'Length loop
         declare
            Bank : constant Memory_Bank := Integer (Data (I));
            Offset : constant Memory_Address :=
               Integer (Data (I + 1));
            Length : Natural := Integer (Data (I + 2));
         begin
            I := I + 3; --  Update loop counter to after bank, offset and len
            if Length > 0 then
               declare
                  Prog_Buffer : constant Byte_Array (0 .. Length - 1) :=
                     Data (I .. I + Length - 1);
               begin
                  C.Write_Memory_Block (Data => Prog_Buffer,
                                        Bank => Bank,
                                        Address => Offset);
               end;
            else --  Special case according to Rowberg et al
               Length := 1; --  Length is actually 1 here
               declare
                  Magic_Byte : constant Byte := Data (I);
               begin
                  case Magic_Byte is
                     when 16#01# => --  Enable DMP related interrupts
                        C.Write_Byte_Data (R => MPU6050_RA_INT_ENABLE,
                                           D => 50);
                     when others => raise Not_Implemented;
                  end case;
               end;
            end if;
            I := I + Length; -- Set pointer to next block
         end;
      end loop;
   end Write_DMP_Configuration;
end MPU6050;
