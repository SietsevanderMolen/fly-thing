with Ada.Unchecked_Conversion;
with Interfaces;

with I2C; use I2C;

package MPU6050 is
   type Chip is new I2C.Chip with null record;

   subtype Clock_Source is Integer range 0 .. 7;
   subtype Gyro_Scale_Range is Integer range 0 .. 3;
   subtype Accel_Scale_Range is Integer range 0 .. 3;
   subtype External_Frame_Sync_Pin is Integer range 0 .. 7;
   subtype Digital_Low_Pass_Filter_Setting is Integer range 0 .. 6;

   Not_Implemented : exception;

   type Axis_Reading is
      record
         L : Byte;
         H : Byte;
      end record;
   for Axis_Reading use
      record
         L at 0 range 0 .. 7;
         H at 0 range 8 .. 15;
      end record;
   function Pack is new
      Ada.Unchecked_Conversion (Source => Axis_Reading,
                                Target => Interfaces.Integer_16);

   type Triple_Axis_Reading is record
      X : Integer;
      Y : Integer;
      Z : Integer;
   end record;

   type MPU6050_Output is record
      Accelerometer_Output : Triple_Axis_Reading;
      Gyroscope_Output : Triple_Axis_Reading;
      Thermometer_Output : Integer;
   end record;

   --  Reset the chip to the power-on reset state.
   procedure Reset (C : in out Chip);
   procedure Initialize_DMP (C : in out Chip);
   function Test_Connection (C : in Chip) return Boolean;

   function Get_Motion_6 (C : in Chip) return MPU6050_Output;
   function Get_Device_Id (C : in Chip) return I2C.Byte;

   procedure Set_Clock_Source (C : in out Chip; S : Clock_Source);
   procedure Set_Full_Scale_Gyro_Range (C : in out Chip; R : Gyro_Scale_Range);
   procedure Set_Full_Scale_Accel_Range
      (C : in out Chip; R : Accel_Scale_Range);
   procedure Set_Sleep (C : in out Chip; S : Boolean);
   procedure Set_Motion_Detect_Interrupt (C : in out Chip;
                                          S : Boolean);
   procedure Set_Fifo_Overflow_Interrupt (C : in out Chip;
                                          S : Boolean);
   procedure Set_I2C_Master_Interrupt (C : in out Chip;
                                       S : Boolean);
   procedure Set_DMP_Interrupt (C : in out Chip;
                                       S : Boolean);
   procedure Set_Data_Ready_Interrupt (C : in out Chip;
                                       S : Boolean);
   procedure Set_Sample_Rate_Divider (C : in out Chip;
                                      D : in Integer);
   procedure Set_External_Frame_Sync (C : in out Chip;
                                      P : in External_Frame_Sync_Pin);
   procedure Set_DLPF_Mode (C : in out Chip;
                            S : in Digital_Low_Pass_Filter_Setting);
   procedure Set_OTP_Bank_Valid (C : in out Chip;
                                 V : in Boolean);
   procedure Set_XGyro_Offset_TC (C : in out Chip;
                                  O : in Integer);
   procedure Set_YGyro_Offset_TC (C : in out Chip;
                                  O : in Integer);
   procedure Set_ZGyro_Offset_TC (C : in out Chip;
                                  O : in Integer);
   function Get_XGyro_Offset_TC (C : in out Chip) return Integer;
   function Get_YGyro_Offset_TC (C : in out Chip) return Integer;
   function Get_ZGyro_Offset_TC (C : in out Chip) return Integer;
   procedure Reset_Fifo (C : in out Chip);
   function Get_Fifo_Count (C : in out Chip) return Integer;
   procedure Set_Motion_Detect_Threshold (C : in out Chip;
                                          T : in Integer);
   function Get_Motion_Detect_Threshold (C : in out Chip) return Integer;

   procedure Set_Zero_Motion_Detect_Threshold (C : in out Chip;
                                          T : in Integer);
   function Get_Zero_Motion_Detect_Threshold (C : in out Chip) return Integer;

   procedure Set_Motion_Detect_Duration (C : in out Chip;
                                          T : in Integer);
   function Get_Motion_Detect_Duration (C : in out Chip) return Integer;

   procedure Set_Zero_Motion_Detect_Duration (C : in out Chip;
                                          T : in Integer);
   function Get_Zero_Motion_Detect_Duration (C : in out Chip) return Integer;
   procedure Set_Fifo_Enable (C : in out Chip;
                              E : in Boolean);
   procedure Set_DMP_Enable (C : in out Chip;
                             E : in Boolean);
   procedure Reset_DMP (C : in out Chip);
   function Get_Fifo_Bytes (C : in Chip;
                            Length : in Integer) return Byte_Array;

   MPU6050_CLOCK_INTERNAL : constant Clock_Source := 16#00#;
   MPU6050_CLOCK_PLL_XGYRO : constant Clock_Source := 16#01#;
   MPU6050_CLOCK_PLL_YGYRO : constant Clock_Source := 16#02#;
   MPU6050_CLOCK_PLL_ZGYRO : constant Clock_Source := 16#03#;
   MPU6050_CLOCK_PLL_EXT32K : constant Clock_Source := 16#04#;
   MPU6050_CLOCK_PLL_EXT19M : constant Clock_Source := 16#05#;
   MPU6050_CLOCK_KEEP_RESET : constant Clock_Source := 16#07#;
   MPU6050_GYRO_FS_250 : constant Gyro_Scale_Range := 16#00#;
   MPU6050_GYRO_FS_500 : constant Gyro_Scale_Range := 16#01#;
   MPU6050_GYRO_FS_1000 : constant Gyro_Scale_Range := 16#02#;
   MPU6050_GYRO_FS_2000 : constant Gyro_Scale_Range := 16#03#;
   MPU6050_ACCEL_FS_2 : constant Accel_Scale_Range := 16#00#;
   MPU6050_ACCEL_FS_4 : constant Accel_Scale_Range := 16#01#;
   MPU6050_ACCEL_FS_8 : constant Accel_Scale_Range := 16#02#;
   MPU6050_ACCEL_FS_16 : constant Accel_Scale_Range := 16#03#;

   MPU6050_EXT_SYNC_DISABLED : constant External_Frame_Sync_Pin := 16#0#;
   MPU6050_EXT_SYNC_TEMP_OUT_L : constant External_Frame_Sync_Pin := 16#1#;
   MPU6050_EXT_SYNC_GYRO_XOUT_L : constant External_Frame_Sync_Pin := 16#2#;
   MPU6050_EXT_SYNC_GYRO_YOUT_L : constant External_Frame_Sync_Pin := 16#3#;
   MPU6050_EXT_SYNC_GYRO_ZOUT_L : constant External_Frame_Sync_Pin := 16#4#;
   MPU6050_EXT_SYNC_ACCEL_XOUT_L : constant External_Frame_Sync_Pin := 16#5#;
   MPU6050_EXT_SYNC_ACCEL_YOUT_L : constant External_Frame_Sync_Pin := 16#6#;
   MPU6050_EXT_SYNC_ACCEL_ZOUT_L : constant External_Frame_Sync_Pin := 16#7#;

   MPU6050_DLPF_BW_256 : constant Digital_Low_Pass_Filter_Setting := 16#00#;
   MPU6050_DLPF_BW_188 : constant Digital_Low_Pass_Filter_Setting := 16#01#;
   MPU6050_DLPF_BW_98 : constant Digital_Low_Pass_Filter_Setting := 16#02#;
   MPU6050_DLPF_BW_42 : constant Digital_Low_Pass_Filter_Setting := 16#03#;
   MPU6050_DLPF_BW_20 : constant Digital_Low_Pass_Filter_Setting := 16#04#;
   MPU6050_DLPF_BW_10 : constant Digital_Low_Pass_Filter_Setting := 16#05#;
   MPU6050_DLPF_BW_5 : constant Digital_Low_Pass_Filter_Setting := 16#06#;
private
   type CONFIG is
      record
         Pad          : Integer range 0 .. 0;
         Pad1         : Integer range 0 .. 0;
         Ext_Sync_Set : External_Frame_Sync_Pin;
         Dlpf_Cfg     : Digital_Low_Pass_Filter_Setting range 0 .. 6;
      end record;
   for CONFIG use
      record
         Pad at 0 range 7 .. 7;
         Pad1 at 0 range 6 .. 6;
         Ext_Sync_Set at 0 range 3 .. 5;
         Dlpf_Cfg at 0 range 0 .. 2;
      end record;
   CONFIG_Address : constant Register := 16#1A#;
   function Pack is new Ada.Unchecked_Conversion (Source => CONFIG,
                                                  Target => Byte);
   function Unpack is new Ada.Unchecked_Conversion (Source => Byte,
                                                    Target => CONFIG);

   type PWR_MGMT_1 is
      record
         Device_Reset : Integer range 0 .. 1;
         Sleep        : Integer range 0 .. 1;
         Cycle        : Integer range 0 .. 1;
         Pad          : Integer range 0 .. 0;
         Temp_Dis     : Integer range 0 .. 1;
         Clock_Sel    : Clock_Source range 0 .. 7;
      end record;
   for PWR_MGMT_1 use
      record
         Device_Reset at 0 range 7 .. 7;
         Sleep at 0 range 6 .. 6;
         Cycle at 0 range 5 .. 5;
         Pad at 0 range 4 .. 4;
         Temp_Dis at 0 range 3 .. 3;
         Clock_Sel at 0 range 0 .. 2;
      end record;
   PWR_MGMT_1_Address : constant Register := 16#6B#;
   function Pack is new Ada.Unchecked_Conversion (Source => PWR_MGMT_1,
                                                  Target => Byte);
   function Unpack is new Ada.Unchecked_Conversion (Source => Byte,
                                                    Target => PWR_MGMT_1);

   type GYRO_CONFIG is
      record
         XG_ST  : Gyro_Scale_Range range 0 .. 1;
         YG_ST  : Gyro_Scale_Range range 0 .. 1;
         ZG_ST  : Gyro_Scale_Range range 0 .. 1;
         FS_SEL : Integer range 0 .. 3;
         Pad : Integer range 0 .. 0;
      end record;
   for GYRO_CONFIG use
      record
         XG_ST  at 0 range 7 .. 7;
         YG_ST  at 0 range 6 .. 6;
         ZG_ST  at 0 range 5 .. 5;
         FS_SEL at 0 range 3 .. 4;
         Pad at 0 range 0 .. 2;
      end record;
   GYRO_CONFIG_Address : constant Register := 16#1B#;
   function Pack is new Ada.Unchecked_Conversion (Source => GYRO_CONFIG,
                                                  Target => Byte);
   function Unpack is new Ada.Unchecked_Conversion (Source => Byte,
                                                    Target => GYRO_CONFIG);

   type ACCEL_CONFIG is
      record
         XA_ST   : Accel_Scale_Range range 0 .. 1;
         YA_ST   : Accel_Scale_Range range 0 .. 1;
         ZA_ST   : Accel_Scale_Range range 0 .. 1;
         AFS_SEL : Accel_Scale_Range range 0 .. 3;
         Pad : Integer range 0 .. 0;
      end record;
   for ACCEL_CONFIG use
      record
         XA_ST   at 0 range 7 .. 7;
         YA_ST   at 0 range 6 .. 6;
         ZA_ST   at 0 range 5 .. 5;
         AFS_SEL at 0 range 3 .. 4;
         Pad at 0 range 0 .. 2;
      end record;
   ACCEL_CONFIG_Address : constant Register := 16#1C#;
   function Pack is new Ada.Unchecked_Conversion (Source => ACCEL_CONFIG,
                                                  Target => Byte);
   function Unpack is new Ada.Unchecked_Conversion (Source => Byte,
                                                    Target => ACCEL_CONFIG);

   type BANK_SEL is
      record
         Pad : Integer range 0 .. 0;
         PRFTCH_EN : Integer range 0 .. 1;
         CFG_USER_BANK : Integer range 0 .. 1;
         MEM_SEL : Integer range 0 .. 31;
      end record;
   for BANK_SEL use
      record
         Pad at 0 range 7 .. 7;
         PRFTCH_EN at 0 range 6 .. 6;
         CFG_USER_BANK at 0 range 5 .. 5;
         MEM_SEL at 0 range 0 .. 4;
      end record;
   BANK_SEL_Address : constant Register := 16#6D#;
   function Pack is new Ada.Unchecked_Conversion (Source => BANK_SEL,
                                                  Target => Byte);
   function Unpack is new Ada.Unchecked_Conversion (Source => Byte,
                                                    Target => BANK_SEL);

   type INT_ENABLE is
      record
         Pad : Integer range 0 .. 0;
         MOT_EN : Integer range 0 .. 1;
         Pad1 : Integer range 0 .. 0;
         FIFO_OFLOW_EN : Integer range 0 .. 1;
         I2C_MST_EN : Integer range 0 .. 1;
         Pad2 : Integer range 0 .. 0;
         DMP_EN : Integer range 0 .. 1; --  Sure? Rowberg's code sets it...
         DATA_RDY_EN : Integer range 0 .. 1;
      end record;
   for INT_ENABLE use
      record
         Pad at 0 range 7 .. 7;
         MOT_EN at 0 range 6 .. 6;
         Pad1 at 0 range 5 .. 5;
         FIFO_OFLOW_EN at 0 range 4 .. 4;
         I2C_MST_EN at 0 range 3 .. 3;
         Pad2 at 0 range 2 .. 2;
         DMP_EN at 0 range 1 .. 1;
         DATA_RDY_EN at 0 range 0 .. 0;
      end record;
   INT_ENABLE_Address : constant Register := 16#38#;
   function Pack is new Ada.Unchecked_Conversion (Source => INT_ENABLE,
                                                  Target => Byte);
   function Unpack is new Ada.Unchecked_Conversion (Source => Byte,
                                                    Target => INT_ENABLE);

   type XG_OFFS_TC is --  Undocumented
      record
         Power_Mode : Integer range 0 .. 1;
         Offset : Integer range 0 .. 63;
         OTP_Valid : Integer range 0 .. 1;
      end record;
   for XG_OFFS_TC use
      record
         Power_Mode at 0 range 7 .. 7;
         Offset at 0 range 1 .. 6;
         OTP_Valid at 0 range 0 .. 0;
      end record;
   XG_OFFS_TC_Address : constant Register := 16#00#;
   function Pack is new Ada.Unchecked_Conversion (Source => XG_OFFS_TC,
                                                  Target => Byte);
   function Unpack is new Ada.Unchecked_Conversion (Source => Byte,
                                                    Target => XG_OFFS_TC);

   type YG_OFFS_TC is --  Undocumented
      record
         Power_Mode : Integer range 0 .. 1;
         Offset : Integer range 0 .. 63;
         OTP_Valid : Integer range 0 .. 1;
      end record;
   for YG_OFFS_TC use
      record
         Power_Mode at 0 range 7 .. 7;
         Offset at 0 range 1 .. 6;
         OTP_Valid at 0 range 0 .. 0;
      end record;
   YG_OFFS_TC_Address : constant Register := 16#01#;
   function Pack is new Ada.Unchecked_Conversion (Source => YG_OFFS_TC,
                                                  Target => Byte);
   function Unpack is new Ada.Unchecked_Conversion (Source => Byte,
                                                    Target => YG_OFFS_TC);

   type ZG_OFFS_TC is --  Undocumented
      record
         Power_Mode : Integer range 0 .. 1;
         Offset : Integer range 0 .. 63;
         OTP_Valid : Integer range 0 .. 1;
      end record;
   for ZG_OFFS_TC use
      record
         Power_Mode at 0 range 7 .. 7;
         Offset at 0 range 1 .. 6;
         OTP_Valid at 0 range 0 .. 0;
      end record;
   ZG_OFFS_TC_Address : constant Register := 16#02#;
   function Pack is new Ada.Unchecked_Conversion (Source => ZG_OFFS_TC,
                                                  Target => Byte);
   function Unpack is new Ada.Unchecked_Conversion (Source => Byte,
                                                    Target => ZG_OFFS_TC);

   type USER_CTRL is
      record
         DMP_En : Integer range 0 .. 1;
         Fifo_En : Integer range 0 .. 1;
         I2C_Master_En : Integer range 0 .. 1;
         I2C_Iface_Disable : Integer range 0 .. 1;
         DMP_Reset : Integer range 0 .. 1;
         Fifo_Reset : Integer range 0 .. 1;
         I2C_Master_Reset : Integer range 0 .. 1;
         Sig_Cond_Reset : Integer range 0 .. 1;
      end record;
   for USER_CTRL use
      record
         DMP_En at 0 range 7 .. 7;
         Fifo_En at 0 range 6 .. 6;
         I2C_Master_En at 0 range 5 .. 5;
         I2C_Iface_Disable at 0 range 4 .. 4;
         DMP_Reset at 0 range 3 .. 3;
         Fifo_Reset at 0 range 2 .. 2;
         I2C_Master_Reset at 0 range 1 .. 1;
         Sig_Cond_Reset at 0 range 0 .. 0;
      end record;
   USER_CTRL_Address : constant Register := 16#6A#;
   function Pack is new Ada.Unchecked_Conversion (Source => USER_CTRL,
                                                  Target => Byte);
   function Unpack is new Ada.Unchecked_Conversion (Source => Byte,
                                                    Target => USER_CTRL);

   type FIFO_COUNT is
      record
         H : Integer range 0 .. 7;
         L : Integer range 0 .. 7;
      end record;
   for FIFO_COUNT use
      record
         H at 0 range 0 .. 7;
         L at 0 range 8 .. 15;
      end record;
   FIFO_COUNT_H_Address : constant Register := 16#72#;
   FIFO_COUNT_L_Address : constant Register := 16#73#;
   function Pack is new Ada.Unchecked_Conversion (Source => FIFO_COUNT,
                                                  Target => Word);
   function Unpack is new Ada.Unchecked_Conversion (Source => Word,
                                                    Target => FIFO_COUNT);

   type INT_STATUS is
      record
         Free_Fall_Int : Integer range 0 .. 1;
         Mot_Int : Integer range 0 .. 1;
         Z_Mot_Int : Integer range 0 .. 1;
         Fifo_Oflow_Int : Integer range 0 .. 1;
         I2C_Master_Int : Integer range 0 .. 1;
         PLL_Ready_Int : Integer range 0 .. 1;
         DMP_Int : Integer range 0 .. 1;
         Data_Ready : Integer range 0 .. 1;
      end record;
   for INT_STATUS use
      record
         Free_Fall_Int at 0 range 7 .. 7;
         Mot_Int at 0 range 6 .. 6;
         Z_Mot_Int at 0 range 5 .. 5;
         Fifo_Oflow_Int at 0 range 4 .. 4;
         I2C_Master_Int at 0 range 3 .. 3;
         PLL_Ready_Int at 0 range 2 .. 2;
         DMP_Int at 0 range 1 .. 1;
         Data_Ready at 0 range 0 .. 0;
      end record;
   INT_STATUS_Address : constant Register := 16#3A#;
   function Pack is new Ada.Unchecked_Conversion (Source => INT_STATUS,
                                                  Target => Byte);
   function Unpack is new Ada.Unchecked_Conversion (Source => Byte,
                                                    Target => INT_STATUS);

   FIFO_R_W_Address : constant Register := 16#74#;

   subtype Memory_Bank is Integer range 0 .. 31;
   subtype Memory_Address is Integer range 0 .. 255;

   procedure Set_Memory_Bank (C : in Chip;
                              Bank : Memory_Bank;
                              Prefetch : Boolean := False;
                              User_Bank : Boolean := False);
   procedure Set_Memory_Start_Address (C : in Chip;
                                       Address : Memory_Address);
   function Read_Memory_Byte (C : in Chip) return Byte;
   procedure Write_Memory_Block (C : in Chip;
                                 Data : in Byte_Array;
                                 Bank : in Memory_Bank := 0;
                                 Address : in Memory_Address := 0;
                                 Verify : in Boolean := True);
   procedure Write_DMP_Update (C : in Chip; Data : in Byte_Array);
   function Get_Interrupt_Status (C : in Chip) return INT_STATUS;

   MOT_THR_Address : constant Register := 16#1F#;
   MOT_DUR_Address : constant Register := 16#20#;
   ZRMOT_THR_Address : constant Register := 16#21#;
   ZRMOT_DUR_Address : constant Register := 16#22#;

   SMPLRT_DIV_Address : constant Register := 16#19#;

   --  Name the chip's registers
   MPU6050_ADDRESS_AD0_LOW : constant Register := 16#68#;
   MPU6050_ADDRESS_AD0_HIGH : constant Register := 16#69#;
   MPU6050_DEFAULT_ADDRESS : constant Register := MPU6050_ADDRESS_AD0_LOW;
   MPU6050_RA_XG_OFFS_TC : constant Register := 16#00#;
   MPU6050_RA_YG_OFFS_TC : constant Register := 16#01#;
   MPU6050_RA_ZG_OFFS_TC : constant Register := 16#02#;
   MPU6050_RA_X_FINE_GAIN : constant Register := 16#03#;
   MPU6050_RA_Y_FINE_GAIN : constant Register := 16#04#;
   MPU6050_RA_Z_FINE_GAIN : constant Register := 16#05#;
   MPU6050_RA_XA_OFFS_H : constant Register := 16#06#;
   MPU6050_RA_XA_OFFS_L_TC : constant Register := 16#07#;
   MPU6050_RA_YA_OFFS_H : constant Register := 16#08#;
   MPU6050_RA_YA_OFFS_L_TC : constant Register := 16#09#;
   MPU6050_RA_ZA_OFFS_H : constant Register := 16#0A#;
   MPU6050_RA_ZA_OFFS_L_TC : constant Register := 16#0B#;
   MPU6050_RA_XG_OFFS_USRH : constant Register := 16#13#;
   MPU6050_RA_XG_OFFS_USRL : constant Register := 16#14#;
   MPU6050_RA_YG_OFFS_USRH : constant Register := 16#15#;
   MPU6050_RA_YG_OFFS_USRL : constant Register := 16#16#;
   MPU6050_RA_ZG_OFFS_USRH : constant Register := 16#17#;
   MPU6050_RA_ZG_OFFS_USRL : constant Register := 16#18#;
   MPU6050_RA_CONFIG : constant Register := 16#1A#;
   MPU6050_RA_GYRO_CONFIG : constant Register := 16#1B#;
   MPU6050_RA_ACCEL_CONFIG : constant Register := 16#1C#;
   MPU6050_RA_FF_THR : constant Register := 16#1D#;
   MPU6050_RA_FF_DUR : constant Register := 16#1E#;
   MPU6050_RA_MOT_THR : constant Register := 16#1F#;
   MPU6050_RA_MOT_DUR : constant Register := 16#20#;
   MPU6050_RA_ZRMOT_THR : constant Register := 16#21#;
   MPU6050_RA_ZRMOT_DUR : constant Register := 16#22#;
   MPU6050_RA_FIFO_EN : constant Register := 16#23#;
   MPU6050_RA_I2C_MST_CTRL : constant Register := 16#24#;
   MPU6050_RA_I2C_SLV0_ADDR : constant Register := 16#25#;
   MPU6050_RA_I2C_SLV0_REG : constant Register := 16#26#;
   MPU6050_RA_I2C_SLV0_CTRL : constant Register := 16#27#;
   MPU6050_RA_I2C_SLV1_ADDR : constant Register := 16#28#;
   MPU6050_RA_I2C_SLV1_REG : constant Register := 16#29#;
   MPU6050_RA_I2C_SLV1_CTRL : constant Register := 16#2A#;
   MPU6050_RA_I2C_SLV2_ADDR : constant Register := 16#2B#;
   MPU6050_RA_I2C_SLV2_REG : constant Register := 16#2C#;
   MPU6050_RA_I2C_SLV2_CTRL : constant Register := 16#2D#;
   MPU6050_RA_I2C_SLV3_ADDR : constant Register := 16#2E#;
   MPU6050_RA_I2C_SLV3_REG : constant Register := 16#2F#;
   MPU6050_RA_I2C_SLV3_CTRL : constant Register := 16#30#;
   MPU6050_RA_I2C_SLV4_ADDR : constant Register := 16#31#;
   MPU6050_RA_I2C_SLV4_REG : constant Register := 16#32#;
   MPU6050_RA_I2C_SLV4_DO : constant Register := 16#33#;
   MPU6050_RA_I2C_SLV4_CTRL : constant Register := 16#34#;
   MPU6050_RA_I2C_SLV4_DI : constant Register := 16#35#;
   MPU6050_RA_I2C_MST_STATUS : constant Register := 16#36#;
   MPU6050_RA_INT_PIN_CFG : constant Register := 16#37#;
   MPU6050_RA_INT_ENABLE : constant Register := 16#38#;
   MPU6050_RA_DMP_INT_STATUS : constant Register := 16#39#;
   MPU6050_RA_INT_STATUS : constant Register := 16#3A#;
   MPU6050_RA_ACCEL_XOUT_H : constant Register := 16#3B#;
   MPU6050_RA_ACCEL_XOUT_L : constant Register := 16#3C#;
   MPU6050_RA_ACCEL_YOUT_H : constant Register := 16#3D#;
   MPU6050_RA_ACCEL_YOUT_L : constant Register := 16#3E#;
   MPU6050_RA_ACCEL_ZOUT_H : constant Register := 16#3F#;
   MPU6050_RA_ACCEL_ZOUT_L : constant Register := 16#40#;
   MPU6050_RA_TEMP_OUT_H : constant Register := 16#41#;
   MPU6050_RA_TEMP_OUT_L : constant Register := 16#42#;
   MPU6050_RA_GYRO_XOUT_H : constant Register := 16#43#;
   MPU6050_RA_GYRO_XOUT_L : constant Register := 16#44#;
   MPU6050_RA_GYRO_YOUT_H : constant Register := 16#45#;
   MPU6050_RA_GYRO_YOUT_L : constant Register := 16#46#;
   MPU6050_RA_GYRO_ZOUT_H : constant Register := 16#47#;
   MPU6050_RA_GYRO_ZOUT_L : constant Register := 16#48#;
   MPU6050_RA_EXT_SENS_DATA_00 : constant Register := 16#49#;
   MPU6050_RA_EXT_SENS_DATA_01 : constant Register := 16#4A#;
   MPU6050_RA_EXT_SENS_DATA_02 : constant Register := 16#4B#;
   MPU6050_RA_EXT_SENS_DATA_03 : constant Register := 16#4C#;
   MPU6050_RA_EXT_SENS_DATA_04 : constant Register := 16#4D#;
   MPU6050_RA_EXT_SENS_DATA_05 : constant Register := 16#4E#;
   MPU6050_RA_EXT_SENS_DATA_06 : constant Register := 16#4F#;
   MPU6050_RA_EXT_SENS_DATA_07 : constant Register := 16#50#;
   MPU6050_RA_EXT_SENS_DATA_08 : constant Register := 16#51#;
   MPU6050_RA_EXT_SENS_DATA_09 : constant Register := 16#52#;
   MPU6050_RA_EXT_SENS_DATA_10 : constant Register := 16#53#;
   MPU6050_RA_EXT_SENS_DATA_11 : constant Register := 16#54#;
   MPU6050_RA_EXT_SENS_DATA_12 : constant Register := 16#55#;
   MPU6050_RA_EXT_SENS_DATA_13 : constant Register := 16#56#;
   MPU6050_RA_EXT_SENS_DATA_14 : constant Register := 16#57#;
   MPU6050_RA_EXT_SENS_DATA_15 : constant Register := 16#58#;
   MPU6050_RA_EXT_SENS_DATA_16 : constant Register := 16#59#;
   MPU6050_RA_EXT_SENS_DATA_17 : constant Register := 16#5A#;
   MPU6050_RA_EXT_SENS_DATA_18 : constant Register := 16#5B#;
   MPU6050_RA_EXT_SENS_DATA_19 : constant Register := 16#5C#;
   MPU6050_RA_EXT_SENS_DATA_20 : constant Register := 16#5D#;
   MPU6050_RA_EXT_SENS_DATA_21 : constant Register := 16#5E#;
   MPU6050_RA_EXT_SENS_DATA_22 : constant Register := 16#5F#;
   MPU6050_RA_EXT_SENS_DATA_23 : constant Register := 16#60#;
   MPU6050_RA_MOT_DETECT_STATUS : constant Register := 16#61#;
   MPU6050_RA_I2C_SLV0_DO : constant Register := 16#63#;
   MPU6050_RA_I2C_SLV1_DO : constant Register := 16#64#;
   MPU6050_RA_I2C_SLV2_DO : constant Register := 16#65#;
   MPU6050_RA_I2C_SLV3_DO : constant Register := 16#66#;
   MPU6050_RA_I2C_MST_DELAY_CTRL : constant Register := 16#67#;
   MPU6050_RA_SIGNAL_PATH_RESET : constant Register := 16#68#;
   MPU6050_RA_MOT_DETECT_CTRL : constant Register := 16#69#;
   MPU6050_RA_USER_CTRL : constant Register := 16#6A#;
   MPU6050_RA_BANK_SEL : constant Register := 16#6D#;
   MPU6050_RA_MEM_START_ADDR : constant Register := 16#6E#;
   MPU6050_RA_MEM_R_W : constant Register := 16#6F#;
   MPU6050_RA_DMP_CFG_1 : constant Register := 16#70#;
   MPU6050_RA_DMP_CFG_2 : constant Register := 16#71#;
   MPU6050_RA_FIFO_COUNTH : constant Register := 16#72#;
   MPU6050_RA_FIFO_COUNTL : constant Register := 16#73#;
   MPU6050_RA_FIFO_R_W : constant Register := 16#74#;
   MPU6050_RA_WHO_AM_I : constant Register := 16#75#;

   MPU6050_TC_PWR_MODE_BIT : constant Register := 7;
   MPU6050_TC_OFFSET_BIT : constant Register := 6;
   MPU6050_TC_OFFSET_LENGTH : constant Register := 6;
   MPU6050_TC_OTP_BNK_VLD_BIT : constant Register := 0;

   MPU6050_VDDIO_LEVEL_VLOGIC : constant Register := 0;
   MPU6050_VDDIO_LEVEL_VDD : constant Register := 1;

   MPU6050_CFG_EXT_SYNC_SET_BIT : constant Register := 5;
   MPU6050_CFG_EXT_SYNC_SET_LENGTH : constant Register := 3;
   MPU6050_CFG_DLPF_CFG_BIT : constant Register := 2;
   MPU6050_CFG_DLPF_CFG_LENGTH : constant Register := 3;

   MPU6050_GCONFIG_FS_SEL_BIT : constant Register := 4;
   MPU6050_GCONFIG_FS_SEL_LENGTH : constant Register := 2;

   MPU6050_DHPF_RESET : constant Register := 16#00#;
   MPU6050_DHPF_5 : constant Register := 16#01#;
   MPU6050_DHPF_2P5 : constant Register := 16#02#;
   MPU6050_DHPF_1P25 : constant Register := 16#03#;
   MPU6050_DHPF_0P63 : constant Register := 16#04#;
   MPU6050_DHPF_HOLD : constant Register := 16#07#;

   MPU6050_TEMP_FIFO_EN_BIT : constant Register := 7;
   MPU6050_XG_FIFO_EN_BIT : constant Register := 6;
   MPU6050_YG_FIFO_EN_BIT : constant Register := 5;
   MPU6050_ZG_FIFO_EN_BIT : constant Register := 4;
   MPU6050_ACCEL_FIFO_EN_BIT : constant Register := 3;
   MPU6050_SLV2_FIFO_EN_BIT : constant Register := 2;
   MPU6050_SLV1_FIFO_EN_BIT : constant Register := 1;
   MPU6050_SLV0_FIFO_EN_BIT : constant Register := 0;

   MPU6050_MULT_MST_EN_BIT : constant Register := 7;
   MPU6050_WAIT_FOR_ES_BIT : constant Register := 6;
   MPU6050_SLV_3_FIFO_EN_BIT : constant Register := 5;
   MPU6050_I2C_MST_P_NSR_BIT : constant Register := 4;
   MPU6050_I2C_MST_CLK_BIT : constant Register := 3;
   MPU6050_I2C_MST_CLK_LENGTH : constant Register := 4;

   MPU6050_CLOCK_DIV_348 : constant Register := 16#0#;
   MPU6050_CLOCK_DIV_333 : constant Register := 16#1#;
   MPU6050_CLOCK_DIV_320 : constant Register := 16#2#;
   MPU6050_CLOCK_DIV_308 : constant Register := 16#3#;
   MPU6050_CLOCK_DIV_296 : constant Register := 16#4#;
   MPU6050_CLOCK_DIV_286 : constant Register := 16#5#;
   MPU6050_CLOCK_DIV_276 : constant Register := 16#6#;
   MPU6050_CLOCK_DIV_267 : constant Register := 16#7#;
   MPU6050_CLOCK_DIV_258 : constant Register := 16#8#;
   MPU6050_CLOCK_DIV_500 : constant Register := 16#9#;
   MPU6050_CLOCK_DIV_471 : constant Register := 16#A#;
   MPU6050_CLOCK_DIV_444 : constant Register := 16#B#;
   MPU6050_CLOCK_DIV_421 : constant Register := 16#C#;
   MPU6050_CLOCK_DIV_400 : constant Register := 16#D#;
   MPU6050_CLOCK_DIV_381 : constant Register := 16#E#;
   MPU6050_CLOCK_DIV_364 : constant Register := 16#F#;

   MPU6050_I2C_SLV_RW_BIT : constant Register := 7;
   MPU6050_I2C_SLV_ADDR_BIT : constant Register := 6;
   MPU6050_I2C_SLV_ADDR_LENGTH : constant Register := 7;
   MPU6050_I2C_SLV_EN_BIT : constant Register := 7;
   MPU6050_I2C_SLV_BYTE_SW_BIT : constant Register := 6;
   MPU6050_I2C_SLV_REG_DIS_BIT : constant Register := 5;
   MPU6050_I2C_SLV_GRP_BIT : constant Register := 4;
   MPU6050_I2C_SLV_LEN_BIT : constant Register := 3;
   MPU6050_I2C_SLV_LEN_LENGTH : constant Register := 4;
   MPU6050_I2C_SLV4_RW_BIT : constant Register := 7;
   MPU6050_I2C_SLV4_ADDR_BIT : constant Register := 6;
   MPU6050_I2C_SLV4_ADDR_LENGTH : constant Register := 7;
   MPU6050_I2C_SLV4_EN_BIT : constant Register := 7;
   MPU6050_I2C_SLV4_INT_EN_BIT : constant Register := 6;
   MPU6050_I2C_SLV4_REG_DIS_BIT : constant Register := 5;
   MPU6050_I2C_SLV4_MST_DLY_BIT : constant Register := 4;
   MPU6050_I2C_SLV4_MST_DLY_LENGTH : constant Register := 5;

   MPU6050_MST_PASS_THROUGH_BIT : constant Register := 7;
   MPU6050_MST_I2C_SLV4_DONE_BIT : constant Register := 6;
   MPU6050_MST_I2C_LOST_ARB_BIT : constant Register := 5;
   MPU6050_MST_I2C_SLV4_NACK_BIT : constant Register := 4;
   MPU6050_MST_I2C_SLV3_NACK_BIT : constant Register := 3;
   MPU6050_MST_I2C_SLV2_NACK_BIT : constant Register := 2;
   MPU6050_MST_I2C_SLV1_NACK_BIT : constant Register := 1;
   MPU6050_MST_I2C_SLV0_NACK_BIT : constant Register := 0;

   MPU6050_INTCFG_INT_LEVEL_BIT : constant Register := 7;
   MPU6050_INTCFG_INT_OPEN_BIT : constant Register := 6;
   MPU6050_INTCFG_LATCH_INT_EN_BIT : constant Register := 5;
   MPU6050_INTCFG_INT_RD_CLEAR_BIT : constant Register := 4;
   MPU6050_INTCFG_FSYNC_INT_LEVEL_BIT : constant Register := 3;
   MPU6050_INTCFG_FSYNC_INT_EN_BIT : constant Register := 2;
   MPU6050_INTCFG_I2C_BYPASS_EN_BIT : constant Register := 1;
   MPU6050_INTCFG_CLKOUT_EN_BIT : constant Register := 0;

   MPU6050_INTMODE_ACTIVEHIGH : constant Register := 16#00#;
   MPU6050_INTMODE_ACTIVELOW : constant Register := 16#01#;

   MPU6050_INTDRV_PUSHPULL : constant Register := 16#00#;
   MPU6050_INTDRV_OPENDRAIN : constant Register := 16#01#;

   MPU6050_INTLATCH_50USPULSE : constant Register := 16#00#;
   MPU6050_INTLATCH_WAITCLEAR : constant Register := 16#01#;

   MPU6050_INTCLEAR_STATUSREAD : constant Register := 16#00#;
   MPU6050_INTCLEAR_ANYREAD : constant Register := 16#01#;

   MPU6050_INTERRUPT_FF_BIT : constant Register := 7;
   MPU6050_INTERRUPT_MOT_BIT : constant Register := 6;
   MPU6050_INTERRUPT_ZMOT_BIT : constant Register := 5;
   MPU6050_INTERRUPT_FIFO_OFLOW_BIT : constant Register := 4;
   MPU6050_INTERRUPT_I2C_MST_INT_BIT : constant Register := 3;
   MPU6050_INTERRUPT_PLL_RDY_INT_BIT : constant Register := 2;
   MPU6050_INTERRUPT_DMP_INT_BIT : constant Register := 1;
   MPU6050_INTERRUPT_DATA_RDY_BIT : constant Register := 0;

   MPU6050_DMPINT_5_BIT : constant Register := 5;
   MPU6050_DMPINT_4_BIT : constant Register := 4;
   MPU6050_DMPINT_3_BIT : constant Register := 3;
   MPU6050_DMPINT_2_BIT : constant Register := 2;
   MPU6050_DMPINT_1_BIT : constant Register := 1;
   MPU6050_DMPINT_0_BIT : constant Register := 0;

   MPU6050_MOTION_MOT_XNEG_BIT : constant Register := 7;
   MPU6050_MOTION_MOT_XPOS_BIT : constant Register := 6;
   MPU6050_MOTION_MOT_YNEG_BIT : constant Register := 5;
   MPU6050_MOTION_MOT_YPOS_BIT : constant Register := 4;
   MPU6050_MOTION_MOT_ZNEG_BIT : constant Register := 3;
   MPU6050_MOTION_MOT_ZPOS_BIT : constant Register := 2;
   MPU6050_MOTION_MOT_ZRMOT_BIT : constant Register := 0;

   MPU6050_DELAYCTRL_DELAY_ES_SHADOW_BIT : constant Register := 7;
   MPU6050_DELAYCTRL_I2C_SLV4_DLY_EN_BIT : constant Register := 4;
   MPU6050_DELAYCTRL_I2C_SLV3_DLY_EN_BIT : constant Register := 3;
   MPU6050_DELAYCTRL_I2C_SLV2_DLY_EN_BIT : constant Register := 2;
   MPU6050_DELAYCTRL_I2C_SLV1_DLY_EN_BIT : constant Register := 1;
   MPU6050_DELAYCTRL_I2C_SLV0_DLY_EN_BIT : constant Register := 0;

   MPU6050_PATHRESET_GYRO_RESET_BIT : constant Register := 2;
   MPU6050_PATHRESET_ACCEL_RESET_BIT : constant Register := 1;
   MPU6050_PATHRESET_TEMP_RESET_BIT : constant Register := 0;

   MPU6050_DETECT_ACCEL_ON_DELAY_BIT : constant Register := 5;
   MPU6050_DETECT_ACCEL_ON_DELAY_LENGTH : constant Register := 2;
   MPU6050_DETECT_FF_COUNT_BIT : constant Register := 3;
   MPU6050_DETECT_FF_COUNT_LENGTH : constant Register := 2;
   MPU6050_DETECT_MOT_COUNT_BIT : constant Register := 1;
   MPU6050_DETECT_MOT_COUNT_LENGTH : constant Register := 2;
   MPU6050_DETECT_DECREMENT_RESET : constant Register := 16#0#;
   MPU6050_DETECT_DECREMENT_1 : constant Register := 16#1#;
   MPU6050_DETECT_DECREMENT_2 : constant Register := 16#2#;
   MPU6050_DETECT_DECREMENT_4 : constant Register := 16#3#;

   MPU6050_USERCTRL_DMP_EN_BIT : constant Register := 7;
   MPU6050_USERCTRL_FIFO_EN_BIT : constant Register := 6;
   MPU6050_USERCTRL_I2C_MST_EN_BIT : constant Register := 5;
   MPU6050_USERCTRL_I2C_IF_DIS_BIT : constant Register := 4;
   MPU6050_USERCTRL_DMP_RESET_BIT : constant Register := 3;
   MPU6050_USERCTRL_FIFO_RESET_BIT : constant Register := 2;
   MPU6050_USERCTRL_I2C_MST_RESET_BIT : constant Register := 1;
   MPU6050_USERCTRL_SIG_COND_RESET_BIT : constant Register := 0;

   MPU6050_PWR1_DEVICE_RESET_BIT : constant Register := 7;
   MPU6050_PWR1_SLEEP_BIT : constant Register := 6;
   MPU6050_PWR1_CYCLE_BIT : constant Register := 5;
   MPU6050_PWR1_TEMP_DIS_BIT : constant Register := 3;
   MPU6050_PWR1_CLKSEL_BIT : constant Register := 2;
   MPU6050_PWR1_CLKSEL_LENGTH : constant Register := 3;

   MPU6050_PWR2_LP_WAKE_CTRL_BIT : constant Register := 7;
   MPU6050_PWR2_LP_WAKE_CTRL_LENGTH : constant Register := 2;
   MPU6050_PWR2_STBY_XA_BIT : constant Register := 5;
   MPU6050_PWR2_STBY_YA_BIT : constant Register := 4;
   MPU6050_PWR2_STBY_ZA_BIT : constant Register := 3;
   MPU6050_PWR2_STBY_XG_BIT : constant Register := 2;
   MPU6050_PWR2_STBY_YG_BIT : constant Register := 1;
   MPU6050_PWR2_STBY_ZG_BIT : constant Register := 0;

   MPU6050_WAKE_FREQ_1P25 : constant Register := 16#0#;
   MPU6050_WAKE_FREQ_2P5 : constant Register := 16#1#;
   MPU6050_WAKE_FREQ_5 : constant Register := 16#2#;
   MPU6050_WAKE_FREQ_10 : constant Register := 16#3#;

   MPU6050_BANKSEL_PRFTCH_EN_BIT : constant Integer := 6;
   MPU6050_BANKSEL_CFG_USER_BANK_BIT : constant Integer := 5;
   MPU6050_BANKSEL_MEM_SEL_BIT : constant Integer := 4;
   MPU6050_BANKSEL_MEM_SEL_LENGTH : constant Integer := 5;

   MPU6050_WHO_AM_I_BIT : constant Integer := 6;
   MPU6050_WHO_AM_I_LENGTH : constant Integer := 6;

   MPU6050_DMP_MEMORY_BANKS : constant Integer := 8;
   MPU6050_DMP_MEMORY_BANK_SIZE : constant Integer := 256;
   MPU6050_DMP_MEMORY_CHUNK_SIZE : constant Integer := 16;

   MPU_Progmem : Byte_Array := (
      16#FB#, 16#00#, 16#00#, 16#3E#, 16#00#, 16#0B#, 16#00#, 16#36#, 16#00#,
      16#01#, 16#00#, 16#02#, 16#00#, 16#03#, 16#00#, 16#00#, 16#00#, 16#65#,
      16#00#, 16#54#, 16#FF#, 16#EF#, 16#00#, 16#00#, 16#FA#, 16#80#, 16#00#,
      16#0B#, 16#12#, 16#82#, 16#00#, 16#01#, 16#00#, 16#02#, 16#00#, 16#00#,
      16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
      16#00#, 16#00#, 16#00#, 16#00#, 16#28#, 16#00#, 16#00#, 16#FF#, 16#FF#,
      16#45#, 16#81#, 16#FF#, 16#FF#, 16#FA#, 16#72#, 16#00#, 16#00#, 16#00#,
      16#00#, 16#00#, 16#00#, 16#03#, 16#E8#, 16#00#, 16#00#, 16#00#, 16#01#,
      16#00#, 16#01#, 16#7F#, 16#FF#, 16#FF#, 16#FE#, 16#80#, 16#01#, 16#00#,
      16#1B#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
      16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#3E#, 16#03#,
      16#30#, 16#40#, 16#00#, 16#00#, 16#00#, 16#02#, 16#CA#, 16#E3#, 16#09#,
      16#3E#, 16#80#, 16#00#, 16#00#, 16#20#, 16#00#, 16#00#, 16#00#, 16#00#,
      16#00#, 16#00#, 16#00#, 16#40#, 16#00#, 16#00#, 16#00#, 16#60#, 16#00#,
      16#00#, 16#00#, 16#41#, 16#FF#, 16#00#, 16#00#, 16#00#, 16#00#, 16#0B#,
      16#2A#, 16#00#, 16#00#, 16#16#, 16#55#, 16#00#, 16#00#, 16#21#, 16#82#,
      16#FD#, 16#87#, 16#26#, 16#50#, 16#FD#, 16#80#, 16#00#, 16#00#, 16#00#,
      16#1F#, 16#00#, 16#00#, 16#00#, 16#05#, 16#80#, 16#00#, 16#00#, 16#00#,
      16#00#, 16#00#, 16#00#, 16#01#, 16#00#, 16#00#, 16#00#, 16#02#, 16#00#,
      16#00#, 16#00#, 16#03#, 16#00#, 16#00#, 16#40#, 16#00#, 16#00#, 16#00#,
      16#00#, 16#00#, 16#04#, 16#6F#, 16#00#, 16#02#, 16#65#, 16#32#, 16#00#,
      16#00#, 16#5E#, 16#C0#, 16#40#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
      16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
      16#00#, 16#FB#, 16#8C#, 16#6F#, 16#5D#, 16#FD#, 16#5D#, 16#08#, 16#D9#,
      16#00#, 16#7C#, 16#73#, 16#3B#, 16#00#, 16#6C#, 16#12#, 16#CC#, 16#32#,
      16#00#, 16#13#, 16#9D#, 16#32#, 16#00#, 16#D0#, 16#D6#, 16#32#, 16#00#,
      16#08#, 16#00#, 16#40#, 16#00#, 16#01#, 16#F4#, 16#FF#, 16#E6#, 16#80#,
      16#79#, 16#02#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#D0#, 16#D6#,
      16#00#, 16#00#, 16#27#, 16#10#, 16#FB#, 16#00#, 16#00#, 16#00#, 16#40#,
      16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
      16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#01#, 16#00#, 16#00#, 16#00#,
      16#00#, 16#00#, 16#00#, 16#01#, 16#00#, 16#01#, 16#00#, 16#00#, 16#00#,
      16#00#, 16#00#, 16#FA#, 16#36#, 16#FF#, 16#BC#, 16#30#, 16#8E#, 16#00#,
      16#05#, 16#FB#, 16#F0#, 16#FF#, 16#D9#, 16#5B#, 16#C8#, 16#FF#, 16#D0#,
      16#9A#, 16#BE#, 16#00#, 16#00#, 16#10#, 16#A9#, 16#FF#, 16#F4#, 16#1E#,
      16#B2#, 16#00#, 16#CE#, 16#BB#, 16#F7#, 16#00#, 16#00#, 16#00#, 16#01#,
      16#00#, 16#00#, 16#00#, 16#04#, 16#00#, 16#02#, 16#00#, 16#02#, 16#02#,
      16#00#, 16#00#, 16#0C#, 16#FF#, 16#C2#, 16#80#, 16#00#, 16#00#, 16#01#,
      16#80#, 16#00#, 16#00#, 16#CF#, 16#80#, 16#00#, 16#40#, 16#00#, 16#00#,
      16#00#, 16#00#, 16#00#, 16#00#, 16#01#, 16#00#, 16#00#, 16#00#, 16#00#,
      16#00#, 16#00#, 16#06#, 16#00#, 16#00#, 16#00#, 16#00#, 16#14#, 16#00#,
      16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
      16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
      16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
      16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
      16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
      16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#03#, 16#3F#, 16#68#,
      16#B6#, 16#79#, 16#35#, 16#28#, 16#BC#, 16#C6#, 16#7E#, 16#D1#, 16#6C#,
      16#80#, 16#00#, 16#00#, 16#00#, 16#40#, 16#00#, 16#00#, 16#00#, 16#00#,
      16#00#, 16#B2#, 16#6A#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
      16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#3F#,
      16#F0#, 16#00#, 16#00#, 16#00#, 16#30#, 16#00#, 16#00#, 16#00#, 16#00#,
      16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
      16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
      16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
      16#00#, 16#00#, 16#00#, 16#25#, 16#4D#, 16#00#, 16#2F#, 16#70#, 16#6D#,
      16#00#, 16#00#, 16#05#, 16#AE#, 16#00#, 16#0C#, 16#02#, 16#D0#, 16#00#,
      16#00#, 16#00#, 16#00#, 16#00#, 16#65#, 16#00#, 16#54#, 16#FF#, 16#EF#,
      16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#01#,
      16#00#, 16#00#, 16#44#, 16#00#, 16#00#, 16#00#, 16#00#, 16#0C#, 16#00#,
      16#00#, 16#00#, 16#01#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
      16#65#, 16#00#, 16#00#, 16#00#, 16#54#, 16#00#, 16#00#, 16#FF#, 16#EF#,
      16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
      16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
      16#40#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
      16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#40#, 16#00#,
      16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
      16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#01#,
      16#00#, 16#00#, 16#00#, 16#02#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
      16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
      16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
      16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
      16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
      16#1B#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
      16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
      16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
      16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
      16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#40#, 16#00#,
      16#00#, 16#00#, 16#00#, 16#1B#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
      16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
      16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
      16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
      16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
      16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
      16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
      16#00#, 16#00#, 16#00#, 16#D8#, 16#DC#, 16#BA#, 16#A2#, 16#F1#, 16#DE#,
      16#B2#, 16#B8#, 16#B4#, 16#A8#, 16#81#, 16#91#, 16#F7#, 16#4A#, 16#90#,
      16#7F#, 16#91#, 16#6A#, 16#F3#, 16#F9#, 16#DB#, 16#A8#, 16#F9#, 16#B0#,
      16#BA#, 16#A0#, 16#80#, 16#F2#, 16#CE#, 16#81#, 16#F3#, 16#C2#, 16#F1#,
      16#C1#, 16#F2#, 16#C3#, 16#F3#, 16#CC#, 16#A2#, 16#B2#, 16#80#, 16#F1#,
      16#C6#, 16#D8#, 16#80#, 16#BA#, 16#A7#, 16#DF#, 16#DF#, 16#DF#, 16#F2#,
      16#A7#, 16#C3#, 16#CB#, 16#C5#, 16#B6#, 16#F0#, 16#87#, 16#A2#, 16#94#,
      16#24#, 16#48#, 16#70#, 16#3C#, 16#95#, 16#40#, 16#68#, 16#34#, 16#58#,
      16#9B#, 16#78#, 16#A2#, 16#F1#, 16#83#, 16#92#, 16#2D#, 16#55#, 16#7D#,
      16#D8#, 16#B1#, 16#B4#, 16#B8#, 16#A1#, 16#D0#, 16#91#, 16#80#, 16#F2#,
      16#70#, 16#F3#, 16#70#, 16#F2#, 16#7C#, 16#80#, 16#A8#, 16#F1#, 16#01#,
      16#B0#, 16#98#, 16#87#, 16#D9#, 16#43#, 16#D8#, 16#86#, 16#C9#, 16#88#,
      16#BA#, 16#A1#, 16#F2#, 16#0E#, 16#B8#, 16#97#, 16#80#, 16#F1#, 16#A9#,
      16#DF#, 16#DF#, 16#DF#, 16#AA#, 16#DF#, 16#DF#, 16#DF#, 16#F2#, 16#AA#,
      16#C5#, 16#CD#, 16#C7#, 16#A9#, 16#0C#, 16#C9#, 16#2C#, 16#97#, 16#97#,
      16#97#, 16#97#, 16#F1#, 16#A9#, 16#89#, 16#26#, 16#46#, 16#66#, 16#B0#,
      16#B4#, 16#BA#, 16#80#, 16#AC#, 16#DE#, 16#F2#, 16#CA#, 16#F1#, 16#B2#,
      16#8C#, 16#02#, 16#A9#, 16#B6#, 16#98#, 16#00#, 16#89#, 16#0E#, 16#16#,
      16#1E#, 16#B8#, 16#A9#, 16#B4#, 16#99#, 16#2C#, 16#54#, 16#7C#, 16#B0#,
      16#8A#, 16#A8#, 16#96#, 16#36#, 16#56#, 16#76#, 16#F1#, 16#B9#, 16#AF#,
      16#B4#, 16#B0#, 16#83#, 16#C0#, 16#B8#, 16#A8#, 16#97#, 16#11#, 16#B1#,
      16#8F#, 16#98#, 16#B9#, 16#AF#, 16#F0#, 16#24#, 16#08#, 16#44#, 16#10#,
      16#64#, 16#18#, 16#F1#, 16#A3#, 16#29#, 16#55#, 16#7D#, 16#AF#, 16#83#,
      16#B5#, 16#93#, 16#AF#, 16#F0#, 16#00#, 16#28#, 16#50#, 16#F1#, 16#A3#,
      16#86#, 16#9F#, 16#61#, 16#A6#, 16#DA#, 16#DE#, 16#DF#, 16#D9#, 16#FA#,
      16#A3#, 16#86#, 16#96#, 16#DB#, 16#31#, 16#A6#, 16#D9#, 16#F8#, 16#DF#,
      16#BA#, 16#A6#, 16#8F#, 16#C2#, 16#C5#, 16#C7#, 16#B2#, 16#8C#, 16#C1#,
      16#B8#, 16#A2#, 16#DF#, 16#DF#, 16#DF#, 16#A3#, 16#DF#, 16#DF#, 16#DF#,
      16#D8#, 16#D8#, 16#F1#, 16#B8#, 16#A8#, 16#B2#, 16#86#, 16#B4#, 16#98#,
      16#0D#, 16#35#, 16#5D#, 16#B8#, 16#AA#, 16#98#, 16#B0#, 16#87#, 16#2D#,
      16#35#, 16#3D#, 16#B2#, 16#B6#, 16#BA#, 16#AF#, 16#8C#, 16#96#, 16#19#,
      16#8F#, 16#9F#, 16#A7#, 16#0E#, 16#16#, 16#1E#, 16#B4#, 16#9A#, 16#B8#,
      16#AA#, 16#87#, 16#2C#, 16#54#, 16#7C#, 16#B9#, 16#A3#, 16#DE#, 16#DF#,
      16#DF#, 16#A3#, 16#B1#, 16#80#, 16#F2#, 16#C4#, 16#CD#, 16#C9#, 16#F1#,
      16#B8#, 16#A9#, 16#B4#, 16#99#, 16#83#, 16#0D#, 16#35#, 16#5D#, 16#89#,
      16#B9#, 16#A3#, 16#2D#, 16#55#, 16#7D#, 16#B5#, 16#93#, 16#A3#, 16#0E#,
      16#16#, 16#1E#, 16#A9#, 16#2C#, 16#54#, 16#7C#, 16#B8#, 16#B4#, 16#B0#,
      16#F1#, 16#97#, 16#83#, 16#A8#, 16#11#, 16#84#, 16#A5#, 16#09#, 16#98#,
      16#A3#, 16#83#, 16#F0#, 16#DA#, 16#24#, 16#08#, 16#44#, 16#10#, 16#64#,
      16#18#, 16#D8#, 16#F1#, 16#A5#, 16#29#, 16#55#, 16#7D#, 16#A5#, 16#85#,
      16#95#, 16#02#, 16#1A#, 16#2E#, 16#3A#, 16#56#, 16#5A#, 16#40#, 16#48#,
      16#F9#, 16#F3#, 16#A3#, 16#D9#, 16#F8#, 16#F0#, 16#98#, 16#83#, 16#24#,
      16#08#, 16#44#, 16#10#, 16#64#, 16#18#, 16#97#, 16#82#, 16#A8#, 16#F1#,
      16#11#, 16#F0#, 16#98#, 16#A2#, 16#24#, 16#08#, 16#44#, 16#10#, 16#64#,
      16#18#, 16#DA#, 16#F3#, 16#DE#, 16#D8#, 16#83#, 16#A5#, 16#94#, 16#01#,
      16#D9#, 16#A3#, 16#02#, 16#F1#, 16#A2#, 16#C3#, 16#C5#, 16#C7#, 16#D8#,
      16#F1#, 16#84#, 16#92#, 16#A2#, 16#4D#, 16#DA#, 16#2A#, 16#D8#, 16#48#,
      16#69#, 16#D9#, 16#2A#, 16#D8#, 16#68#, 16#55#, 16#DA#, 16#32#, 16#D8#,
      16#50#, 16#71#, 16#D9#, 16#32#, 16#D8#, 16#70#, 16#5D#, 16#DA#, 16#3A#,
      16#D8#, 16#58#, 16#79#, 16#D9#, 16#3A#, 16#D8#, 16#78#, 16#93#, 16#A3#,
      16#4D#, 16#DA#, 16#2A#, 16#D8#, 16#48#, 16#69#, 16#D9#, 16#2A#, 16#D8#,
      16#68#, 16#55#, 16#DA#, 16#32#, 16#D8#, 16#50#, 16#71#, 16#D9#, 16#32#,
      16#D8#, 16#70#, 16#5D#, 16#DA#, 16#3A#, 16#D8#, 16#58#, 16#79#, 16#D9#,
      16#3A#, 16#D8#, 16#78#, 16#A8#, 16#8A#, 16#9A#, 16#F0#, 16#28#, 16#50#,
      16#78#, 16#9E#, 16#F3#, 16#88#, 16#18#, 16#F1#, 16#9F#, 16#1D#, 16#98#,
      16#A8#, 16#D9#, 16#08#, 16#D8#, 16#C8#, 16#9F#, 16#12#, 16#9E#, 16#F3#,
      16#15#, 16#A8#, 16#DA#, 16#12#, 16#10#, 16#D8#, 16#F1#, 16#AF#, 16#C8#,
      16#97#, 16#87#, 16#34#, 16#B5#, 16#B9#, 16#94#, 16#A4#, 16#21#, 16#F3#,
      16#D9#, 16#22#, 16#D8#, 16#F2#, 16#2D#, 16#F3#, 16#D9#, 16#2A#, 16#D8#,
      16#F2#, 16#35#, 16#F3#, 16#D9#, 16#32#, 16#D8#, 16#81#, 16#A4#, 16#60#,
      16#60#, 16#61#, 16#D9#, 16#61#, 16#D8#, 16#6C#, 16#68#, 16#69#, 16#D9#,
      16#69#, 16#D8#, 16#74#, 16#70#, 16#71#, 16#D9#, 16#71#, 16#D8#, 16#B1#,
      16#A3#, 16#84#, 16#19#, 16#3D#, 16#5D#, 16#A3#, 16#83#, 16#1A#, 16#3E#,
      16#5E#, 16#93#, 16#10#, 16#30#, 16#81#, 16#10#, 16#11#, 16#B8#, 16#B0#,
      16#AF#, 16#8F#, 16#94#, 16#F2#, 16#DA#, 16#3E#, 16#D8#, 16#B4#, 16#9A#,
      16#A8#, 16#87#, 16#29#, 16#DA#, 16#F8#, 16#D8#, 16#87#, 16#9A#, 16#35#,
      16#DA#, 16#F8#, 16#D8#, 16#87#, 16#9A#, 16#3D#, 16#DA#, 16#F8#, 16#D8#,
      16#B1#, 16#B9#, 16#A4#, 16#98#, 16#85#, 16#02#, 16#2E#, 16#56#, 16#A5#,
      16#81#, 16#00#, 16#0C#, 16#14#, 16#A3#, 16#97#, 16#B0#, 16#8A#, 16#F1#,
      16#2D#, 16#D9#, 16#28#, 16#D8#, 16#4D#, 16#D9#, 16#48#, 16#D8#, 16#6D#,
      16#D9#, 16#68#, 16#D8#, 16#B1#, 16#84#, 16#0D#, 16#DA#, 16#0E#, 16#D8#,
      16#A3#, 16#29#, 16#83#, 16#DA#, 16#2C#, 16#0E#, 16#D8#, 16#A3#, 16#84#,
      16#49#, 16#83#, 16#DA#, 16#2C#, 16#4C#, 16#0E#, 16#D8#, 16#B8#, 16#B0#,
      16#A8#, 16#8A#, 16#9A#, 16#F5#, 16#20#, 16#AA#, 16#DA#, 16#DF#, 16#D8#,
      16#A8#, 16#40#, 16#AA#, 16#D0#, 16#DA#, 16#DE#, 16#D8#, 16#A8#, 16#60#,
      16#AA#, 16#DA#, 16#D0#, 16#DF#, 16#D8#, 16#F1#, 16#97#, 16#86#, 16#A8#,
      16#31#, 16#9B#, 16#06#, 16#99#, 16#07#, 16#AB#, 16#97#, 16#28#, 16#88#,
      16#9B#, 16#F0#, 16#0C#, 16#20#, 16#14#, 16#40#, 16#B8#, 16#B0#, 16#B4#,
      16#A8#, 16#8C#, 16#9C#, 16#F0#, 16#04#, 16#28#, 16#51#, 16#79#, 16#1D#,
      16#30#, 16#14#, 16#38#, 16#B2#, 16#82#, 16#AB#, 16#D0#, 16#98#, 16#2C#,
      16#50#, 16#50#, 16#78#, 16#78#, 16#9B#, 16#F1#, 16#1A#, 16#B0#, 16#F0#,
      16#8A#, 16#9C#, 16#A8#, 16#29#, 16#51#, 16#79#, 16#8B#, 16#29#, 16#51#,
      16#79#, 16#8A#, 16#24#, 16#70#, 16#59#, 16#8B#, 16#20#, 16#58#, 16#71#,
      16#8A#, 16#44#, 16#69#, 16#38#, 16#8B#, 16#39#, 16#40#, 16#68#, 16#8A#,
      16#64#, 16#48#, 16#31#, 16#8B#, 16#30#, 16#49#, 16#60#, 16#A5#, 16#88#,
      16#20#, 16#09#, 16#71#, 16#58#, 16#44#, 16#68#, 16#11#, 16#39#, 16#64#,
      16#49#, 16#30#, 16#19#, 16#F1#, 16#AC#, 16#00#, 16#2C#, 16#54#, 16#7C#,
      16#F0#, 16#8C#, 16#A8#, 16#04#, 16#28#, 16#50#, 16#78#, 16#F1#, 16#88#,
      16#97#, 16#26#, 16#A8#, 16#59#, 16#98#, 16#AC#, 16#8C#, 16#02#, 16#26#,
      16#46#, 16#66#, 16#F0#, 16#89#, 16#9C#, 16#A8#, 16#29#, 16#51#, 16#79#,
      16#24#, 16#70#, 16#59#, 16#44#, 16#69#, 16#38#, 16#64#, 16#48#, 16#31#,
      16#A9#, 16#88#, 16#09#, 16#20#, 16#59#, 16#70#, 16#AB#, 16#11#, 16#38#,
      16#40#, 16#69#, 16#A8#, 16#19#, 16#31#, 16#48#, 16#60#, 16#8C#, 16#A8#,
      16#3C#, 16#41#, 16#5C#, 16#20#, 16#7C#, 16#00#, 16#F1#, 16#87#, 16#98#,
      16#19#, 16#86#, 16#A8#, 16#6E#, 16#76#, 16#7E#, 16#A9#, 16#99#, 16#88#,
      16#2D#, 16#55#, 16#7D#, 16#9E#, 16#B9#, 16#A3#, 16#8A#, 16#22#, 16#8A#,
      16#6E#, 16#8A#, 16#56#, 16#8A#, 16#5E#, 16#9F#, 16#B1#, 16#83#, 16#06#,
      16#26#, 16#46#, 16#66#, 16#0E#, 16#2E#, 16#4E#, 16#6E#, 16#9D#, 16#B8#,
      16#AD#, 16#00#, 16#2C#, 16#54#, 16#7C#, 16#F2#, 16#B1#, 16#8C#, 16#B4#,
      16#99#, 16#B9#, 16#A3#, 16#2D#, 16#55#, 16#7D#, 16#81#, 16#91#, 16#AC#,
      16#38#, 16#AD#, 16#3A#, 16#B5#, 16#83#, 16#91#, 16#AC#, 16#2D#, 16#D9#,
      16#28#, 16#D8#, 16#4D#, 16#D9#, 16#48#, 16#D8#, 16#6D#, 16#D9#, 16#68#,
      16#D8#, 16#8C#, 16#9D#, 16#AE#, 16#29#, 16#D9#, 16#04#, 16#AE#, 16#D8#,
      16#51#, 16#D9#, 16#04#, 16#AE#, 16#D8#, 16#79#, 16#D9#, 16#04#, 16#D8#,
      16#81#, 16#F3#, 16#9D#, 16#AD#, 16#00#, 16#8D#, 16#AE#, 16#19#, 16#81#,
      16#AD#, 16#D9#, 16#01#, 16#D8#, 16#F2#, 16#AE#, 16#DA#, 16#26#, 16#D8#,
      16#8E#, 16#91#, 16#29#, 16#83#, 16#A7#, 16#D9#, 16#AD#, 16#AD#, 16#AD#,
      16#AD#, 16#F3#, 16#2A#, 16#D8#, 16#D8#, 16#F1#, 16#B0#, 16#AC#, 16#89#,
      16#91#, 16#3E#, 16#5E#, 16#76#, 16#F3#, 16#AC#, 16#2E#, 16#2E#, 16#F1#,
      16#B1#, 16#8C#, 16#5A#, 16#9C#, 16#AC#, 16#2C#, 16#28#, 16#28#, 16#28#,
      16#9C#, 16#AC#, 16#30#, 16#18#, 16#A8#, 16#98#, 16#81#, 16#28#, 16#34#,
      16#3C#, 16#97#, 16#24#, 16#A7#, 16#28#, 16#34#, 16#3C#, 16#9C#, 16#24#,
      16#F2#, 16#B0#, 16#89#, 16#AC#, 16#91#, 16#2C#, 16#4C#, 16#6C#, 16#8A#,
      16#9B#, 16#2D#, 16#D9#, 16#D8#, 16#D8#, 16#51#, 16#D9#, 16#D8#, 16#D8#,
      16#79#, 16#D9#, 16#D8#, 16#D8#, 16#F1#, 16#9E#, 16#88#, 16#A3#, 16#31#,
      16#DA#, 16#D8#, 16#D8#, 16#91#, 16#2D#, 16#D9#, 16#28#, 16#D8#, 16#4D#,
      16#D9#, 16#48#, 16#D8#, 16#6D#, 16#D9#, 16#68#, 16#D8#, 16#B1#, 16#83#,
      16#93#, 16#35#, 16#3D#, 16#80#, 16#25#, 16#DA#, 16#D8#, 16#D8#, 16#85#,
      16#69#, 16#DA#, 16#D8#, 16#D8#, 16#B4#, 16#93#, 16#81#, 16#A3#, 16#28#,
      16#34#, 16#3C#, 16#F3#, 16#AB#, 16#8B#, 16#F8#, 16#A3#, 16#91#, 16#B6#,
      16#09#, 16#B4#, 16#D9#, 16#AB#, 16#DE#, 16#FA#, 16#B0#, 16#87#, 16#9C#,
      16#B9#, 16#A3#, 16#DD#, 16#F1#, 16#A3#, 16#A3#, 16#A3#, 16#A3#, 16#95#,
      16#F1#, 16#A3#, 16#A3#, 16#A3#, 16#9D#, 16#F1#, 16#A3#, 16#A3#, 16#A3#,
      16#A3#, 16#F2#, 16#A3#, 16#B4#, 16#90#, 16#80#, 16#F2#, 16#A3#, 16#A3#,
      16#A3#, 16#A3#, 16#A3#, 16#A3#, 16#A3#, 16#A3#, 16#A3#, 16#A3#, 16#B2#,
      16#A3#, 16#A3#, 16#A3#, 16#A3#, 16#A3#, 16#A3#, 16#B0#, 16#87#, 16#B5#,
      16#99#, 16#F1#, 16#A3#, 16#A3#, 16#A3#, 16#98#, 16#F1#, 16#A3#, 16#A3#,
      16#A3#, 16#A3#, 16#97#, 16#A3#, 16#A3#, 16#A3#, 16#A3#, 16#F3#, 16#9B#,
      16#A3#, 16#A3#, 16#DC#, 16#B9#, 16#A7#, 16#F1#, 16#26#, 16#26#, 16#26#,
      16#D8#, 16#D8#, 16#FF#);

   --  [Bank] [Offset] [Length] [n]
   MPU_Config : Byte_Array := (
      16#03#, 16#7B#, 16#03#, 16#4C#, 16#CD#, 16#6C#, 16#03#, 16#AB#, 16#03#,
      16#36#, 16#56#, 16#76#, 16#00#, 16#68#, 16#04#, 16#02#, 16#CB#, 16#47#,
      16#A2#, 16#02#, 16#18#, 16#04#, 16#00#, 16#05#, 16#8B#, 16#C1#, 16#01#,
      16#0C#, 16#04#, 16#00#, 16#00#, 16#00#, 16#00#, 16#03#, 16#7F#, 16#06#,
      16#0C#, 16#C9#, 16#2C#, 16#97#, 16#97#, 16#97#, 16#03#, 16#89#, 16#03#,
      16#26#, 16#46#, 16#66#, 16#00#, 16#6C#, 16#02#, 16#20#, 16#00#, 16#02#,
      16#40#, 16#04#, 16#00#, 16#00#, 16#00#, 16#00#, 16#02#, 16#44#, 16#04#,
      16#00#, 16#00#, 16#00#, 16#00#, 16#02#, 16#48#, 16#04#, 16#00#, 16#00#,
      16#00#, 16#00#, 16#02#, 16#4C#, 16#04#, 16#00#, 16#00#, 16#00#, 16#00#,
      16#02#, 16#50#, 16#04#, 16#00#, 16#00#, 16#00#, 16#00#, 16#02#, 16#54#,
      16#04#, 16#00#, 16#00#, 16#00#, 16#00#, 16#02#, 16#58#, 16#04#, 16#00#,
      16#00#, 16#00#, 16#00#, 16#02#, 16#5C#, 16#04#, 16#00#, 16#00#, 16#00#,
      16#00#, 16#02#, 16#BC#, 16#04#, 16#00#, 16#00#, 16#00#, 16#00#, 16#01#,
      16#EC#, 16#04#, 16#00#, 16#00#, 16#40#, 16#00#, 16#03#, 16#7F#, 16#06#,
      16#0C#, 16#C9#, 16#2C#, 16#97#, 16#97#, 16#97#, 16#04#, 16#02#, 16#03#,
      16#0D#, 16#35#, 16#5D#, 16#04#, 16#09#, 16#04#, 16#87#, 16#2D#, 16#35#,
      16#3D#, 16#00#, 16#A3#, 16#01#, 16#00#, 16#00#, 16#00#, 16#00#, 16#01#,
      16#07#, 16#86#, 16#01#, 16#FE#, 16#07#, 16#41#, 16#05#, 16#F1#, 16#20#,
      16#28#, 16#30#, 16#38#, 16#07#, 16#7E#, 16#01#, 16#30#, 16#07#, 16#46#,
      16#01#, 16#9A#, 16#07#, 16#47#, 16#04#, 16#F1#, 16#28#, 16#30#, 16#38#,
      16#07#, 16#6C#, 16#04#, 16#F1#, 16#28#, 16#30#, 16#38#, 16#02#, 16#16#,
      16#02#, 16#00#, 16#01#); --  Last 1 sets dmp fifo rate. 0=200hz 1=100hz

   --  [Bank] [Offset] [Length] [n]
   MPU_Updates : Byte_Array := (
      16#01#, 16#B2#, 16#02#, 16#FF#, 16#FF#, --  1
      16#01#, 16#90#, 16#04#, 16#09#, 16#23#, 16#A1#, 16#35#, --  2
      16#01#, 16#6A#, 16#02#, 16#06#, 16#00#, --  3
      16#01#, 16#60#, 16#08#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
      16#00#, 16#00#, --  4
      16#00#, 16#60#, 16#04#, 16#40#, 16#00#, 16#00#, 16#00#, --  5
      16#01#, 16#62#, 16#02#, 16#00#, 16#00#, --  6
      16#00#, 16#60#, 16#04#, 16#00#, 16#40#, 16#00#, 16#00#); --  7
end MPU6050;
