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
   end Reset;

   --  Reset the chip to the power-on reset state.
   procedure Initialize_DMP (C : in out Chip) is
      HW_Revision : Byte := 0;
      XG_Offset : constant Integer := C.Get_XGyro_Offset_TC;
      YG_Offset : constant Integer := C.Get_YGyro_Offset_TC;
      ZG_Offset : constant Integer := C.Get_ZGyro_Offset_TC;
      Fifo_Cnt : Integer;
      Interrupt_Status : INT_STATUS;
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
      C.Write_DMP_Update (Data => MPU_Config);

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
      --  Reset offsets
      C.Set_XGyro_Offset_TC (XG_Offset);
      C.Set_YGyro_Offset_TC (YG_Offset);
      C.Set_ZGyro_Offset_TC (ZG_Offset);

      C.Write_DMP_Update (Data => MPU_Updates (0 .. 11)); --  1 & 2

      C.Reset_Fifo;
      Fifo_Cnt := C.Get_Fifo_Count;
      C.Get_Fifo_Bytes (Length => Fifo_Cnt);
      C.Set_Motion_Detect_Threshold (2);
      C.Set_Zero_Motion_Detect_Threshold (156);
      C.Set_Motion_Detect_Duration (80);
      C.Set_Zero_Motion_Detect_Duration (0);
      C.Reset_Fifo;
      C.Set_Fifo_Enable (True);
      C.Set_DMP_Enable (True);
      C.Reset_DMP;

      C.Write_DMP_Update (Data => MPU_Updates (12 .. 34)); --  3, 4 & 5

      while C.Get_Fifo_Count < 3 loop
         null; --  Wait until fifo count is at least 3
      end loop;
      C.Get_Fifo_Bytes (Length => Fifo_Cnt);
      Interrupt_Status := C.Get_Interrupt_Status;

      C.Write_DMP_Update (Data => MPU_Updates (35 .. 39)); --  6

      while C.Get_Fifo_Count < 3 loop
         null; --  Wait until fifo count is at least 3
      end loop;
      C.Get_Fifo_Bytes (Length => Fifo_Cnt);
      Interrupt_Status := C.Get_Interrupt_Status;

      C.Write_DMP_Update (Data => MPU_Updates (40 .. 46)); --  7

      C.Set_DMP_Enable (False);
      C.Reset_Fifo;
      Interrupt_Status := C.Get_Interrupt_Status;
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

      Output.Accelerometer_Output.x := Float (Pack (A_X));
      Output.Accelerometer_Output.y := Float (Pack (A_Y));
      Output.Accelerometer_Output.z := Float (Pack (A_Z));
      Output.Thermometer_Output := Integer (Pack (Tmp));
      Output.Gyroscope_Output.x := Float (Pack (G_X));
      Output.Gyroscope_Output.y := Float (Pack (G_Y));
      Output.Gyroscope_Output.z := Float (Pack (G_Z));
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
                                      D : in Integer) is
   begin
      C.Write_Byte_Data (R => SMPLRT_DIV_Address,
                         D => Byte (D));
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

   procedure Set_XGyro_Offset_TC (C : in out Chip;
                                  O : in Integer) is
      Conf : XG_OFFS_TC := Unpack (C.Read_Byte_Data (XG_OFFS_TC_Address));
   begin
      Conf.Offset := O;
      C.Write_Byte_Data (R => XG_OFFS_TC_Address,
                         D => Pack (Conf));
   end Set_XGyro_Offset_TC;

   procedure Set_YGyro_Offset_TC (C : in out Chip;
                                  O : in Integer) is
      Conf : YG_OFFS_TC := Unpack (C.Read_Byte_Data (YG_OFFS_TC_Address));
   begin
      Conf.Offset := O;
      C.Write_Byte_Data (R => YG_OFFS_TC_Address,
                         D => Pack (Conf));
   end Set_YGyro_Offset_TC;

   procedure Set_ZGyro_Offset_TC (C : in out Chip;
                                  O : in Integer) is
      Conf : ZG_OFFS_TC := Unpack (C.Read_Byte_Data (ZG_OFFS_TC_Address));
   begin
      Conf.Offset := O;
      C.Write_Byte_Data (R => ZG_OFFS_TC_Address,
                         D => Pack (Conf));
   end Set_ZGyro_Offset_TC;

   function Get_XGyro_Offset_TC (C : in out Chip) return Integer is
      Conf : constant XG_OFFS_TC :=
         Unpack (C.Read_Byte_Data (XG_OFFS_TC_Address));
   begin
      return Conf.Offset;
   end Get_XGyro_Offset_TC;

   function Get_YGyro_Offset_TC (C : in out Chip) return Integer is
      Conf : constant YG_OFFS_TC :=
         Unpack (C.Read_Byte_Data (YG_OFFS_TC_Address));
   begin
      return Conf.Offset;
   end Get_YGyro_Offset_TC;

   function Get_ZGyro_Offset_TC (C : in out Chip) return Integer is
      Conf : constant ZG_OFFS_TC :=
         Unpack (C.Read_Byte_Data (ZG_OFFS_TC_Address));
   begin
      return Conf.Offset;
   end Get_ZGyro_Offset_TC;

   procedure Reset_Fifo (C : in out Chip) is
      Conf : USER_CTRL := Unpack (C.Read_Byte_Data (USER_CTRL_Address));
   begin
      Conf.Fifo_Reset := 1;
      C.Write_Byte_Data (R => USER_CTRL_Address,
                         D => Pack (Conf));
   end Reset_Fifo;

   function Get_Fifo_Count (C : in out Chip) return Integer is
      H : constant Byte := C.Read_Byte_Data (FIFO_COUNT_H_Address);
      L : constant Byte := C.Read_Byte_Data (FIFO_COUNT_L_Address);
      Conf : FIFO_COUNT;
   begin
      Conf.H := Integer (H);
      Conf.L := Integer (L);
      return Integer (Pack (Conf));
   end Get_Fifo_Count;

   procedure Set_Motion_Detect_Threshold (C : in out Chip;
                                          T : in Integer) is
   begin
      C.Write_Byte_Data (R => MOT_THR_Address,
                         D => Byte (T));
   end Set_Motion_Detect_Threshold;

   function Get_Motion_Detect_Threshold (C : in out Chip) return Integer is
      T : constant Byte := C.Read_Byte_Data (MOT_THR_Address);
   begin
      return Integer (T);
   end Get_Motion_Detect_Threshold;

   procedure Set_Zero_Motion_Detect_Threshold (C : in out Chip;
                                               T : in Integer) is
   begin
      C.Write_Byte_Data (R => ZRMOT_THR_Address,
                         D => Byte (T));
   end Set_Zero_Motion_Detect_Threshold;

   function Get_Zero_Motion_Detect_Threshold (C : in out Chip)
      return Integer
   is
      T : constant Byte := C.Read_Byte_Data (ZRMOT_THR_Address);
   begin
      return Integer (T);
   end Get_Zero_Motion_Detect_Threshold;

   procedure Set_Motion_Detect_Duration (C : in out Chip;
                                          T : in Integer) is
   begin
      C.Write_Byte_Data (R => MOT_DUR_Address,
                         D => Byte (T));
   end Set_Motion_Detect_Duration;

   function Get_Zero_Motion_Detect_Duration (C : in out Chip) return Integer is
      T : constant Byte := C.Read_Byte_Data (MOT_DUR_Address);
   begin
      return Integer (T);
   end Get_Zero_Motion_Detect_Duration;

   procedure Set_Zero_Motion_Detect_Duration (C : in out Chip;
                                               T : in Integer) is
   begin
      C.Write_Byte_Data (R => ZRMOT_DUR_Address,
                         D => Byte (T));
   end Set_Zero_Motion_Detect_Duration;

   function Get_Motion_Detect_Duration (C : in out Chip) return Integer is
      T : constant Byte := C.Read_Byte_Data (ZRMOT_DUR_Address);
   begin
      return Integer (T);
   end Get_Motion_Detect_Duration;

   procedure Set_Fifo_Enable (C : in out Chip;
                              E : in Boolean) is
      Conf : USER_CTRL := Unpack (C.Read_Byte_Data (USER_CTRL_Address));
   begin
      Conf.Fifo_En := Boolean'Pos (E);
      C.Write_Byte_Data (R => USER_CTRL_Address,
                         D => Pack (Conf));
   end Set_Fifo_Enable;

   procedure Set_DMP_Enable (C : in out Chip;
                             E : in Boolean) is
      Conf : USER_CTRL := Unpack (C.Read_Byte_Data (USER_CTRL_Address));
   begin
      Conf.DMP_En := Boolean'Pos (E);
      C.Write_Byte_Data (R => USER_CTRL_Address,
                         D => Pack (Conf));
   end Set_DMP_Enable;

   procedure Reset_DMP (C : in out Chip) is
      Conf : USER_CTRL := Unpack (C.Read_Byte_Data (USER_CTRL_Address));
   begin
      Conf.DMP_Reset := 1;
      C.Write_Byte_Data (R => USER_CTRL_Address,
                         D => Pack (Conf));
   end Reset_DMP;

   function Get_Fifo_Bytes (C : in Chip;
                            Length : in Integer) return Byte_Array is
      Bytes : Byte_Array (0 .. Length) := (others => 0);
   begin
      if Length > 0 then
         Bytes := C.Read_Array_Data (R => FIFO_R_W_Address,
         L => Length);
      end if;
      return Bytes;
   end Get_Fifo_Bytes;

   procedure Get_Fifo_Bytes (C : in Chip;
                            Length : in Integer) is
      Bytes : Byte_Array (0 .. Length) := (others => 0);
   begin
      if Length > 0 then
         Bytes := C.Read_Array_Data (R => FIFO_R_W_Address,
         L => Length);
      end if;
   end Get_Fifo_Bytes;

   function Get_Interrupt_Status (C : in Chip) return INT_STATUS is
      Status : constant INT_STATUS :=
         Unpack (C.Read_Byte_Data (R => INT_STATUS_Address));
   begin
      return Status;
   end Get_Interrupt_Status;

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
                                                + Chunk_Size - 1));
         if Verify then
            C.Set_Memory_Bank (Current_Bank);
            C.Set_Memory_Start_Address (Current_Address);
            Verify_Buffer := C.Read_Array_Data (R => MPU6050_RA_MEM_R_W,
                                                L => Chunk_Size);
            if Verify_Buffer /= Data ((I * Chunk_Size) + Data'First ..
                                      (I * Chunk_Size) + Data'First
                                         + Chunk_Size - 1)
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

   procedure Write_DMP_Update (C : in Chip;
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
   end Write_DMP_Update;
end MPU6050;
