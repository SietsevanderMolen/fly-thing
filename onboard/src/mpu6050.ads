with Ada.Unchecked_Conversion;
with Interfaces;

with I2C; use I2C;
with Vector_Math; use Vector_Math;

package MPU6050 is
   type Chip is new I2C.Chip with null record;

   subtype Clock_Source is Integer range 0 .. 7;
   subtype Gyro_Scale_Range is Integer range 0 .. 3;
   subtype Accel_Scale_Range is Integer range 0 .. 3;

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

   function Test_Connection (C : in Chip) return Boolean;

   function Get_Motion_6 (C : in Chip) return MPU6050_Output;

   function Get_Device_Id (C : in Chip) return I2C.Byte;

   procedure Set_Clock_Source (C : in out Chip; S : Clock_Source);
   procedure Set_Full_Scale_Gyro_Range (C : in out Chip; R : Gyro_Scale_Range);
   procedure Set_Full_Scale_Accel_Range
      (C : in out Chip; R : Accel_Scale_Range);
   procedure Set_Sleep (C : in out Chip; S : Boolean);

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

private
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
   MPU6050_RA_SMPLRT_DIV : constant Register := 16#19#;
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

   MPU6050_EXT_SYNC_DISABLED : constant Register := 16#0#;
   MPU6050_EXT_SYNC_TEMP_OUT_L : constant Register := 16#1#;
   MPU6050_EXT_SYNC_GYRO_XOUT_L : constant Register := 16#2#;
   MPU6050_EXT_SYNC_GYRO_YOUT_L : constant Register := 16#3#;
   MPU6050_EXT_SYNC_GYRO_ZOUT_L : constant Register := 16#4#;
   MPU6050_EXT_SYNC_ACCEL_XOUT_L : constant Register := 16#5#;
   MPU6050_EXT_SYNC_ACCEL_YOUT_L : constant Register := 16#6#;
   MPU6050_EXT_SYNC_ACCEL_ZOUT_L : constant Register := 16#7#;

   MPU6050_DLPF_BW_256 : constant Register := 16#00#;
   MPU6050_DLPF_BW_188 : constant Register := 16#01#;
   MPU6050_DLPF_BW_98 : constant Register := 16#02#;
   MPU6050_DLPF_BW_42 : constant Register := 16#03#;
   MPU6050_DLPF_BW_20 : constant Register := 16#04#;
   MPU6050_DLPF_BW_10 : constant Register := 16#05#;
   MPU6050_DLPF_BW_5 : constant Register := 16#06#;

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
end MPU6050;
