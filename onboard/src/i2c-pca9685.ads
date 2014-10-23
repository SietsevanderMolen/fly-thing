--  This package interfaces to the PCA9685 I/O multiplexer on the
--  Raspberry Pi's I2C bus.

package I2C.PCA9685 is

   --  A Chip is at an Address on the I2C bus On_Bus.
   type Chip (Address : Chip_Address; On_Bus : not null access Bus)
      is new I2C.Chip (Address=>Chip_Address, On_Bus=>On_Bus) with null record;

   --  Reset the chip to the power-on reset state.
   not overriding
   procedure Reset (C : in out Chip);
private
   --  Name the chip's registers
   MODE1       : constant Register := 0;  -- Mode register 1
   MODE2       : constant Register := 1;  -- Mode register 2
   SUBADR1     : constant Register := 2;  -- I²C-bus subaddress 1
   SUBADR2     : constant Register := 3;  -- I²C-bus subaddress 2
   SUBADR3     : constant Register := 4;  -- I²C-bus subaddress 3
   ALLCALLADR  : constant Register := 5;  -- LED All Call I²C-bus address
   LED0_ON_L   : constant Register := 6;  -- LED0 output and brightness control byte 0
   LED0_ON_H   : constant Register := 7;  -- LED0 output and brightness control byte 1
   LED0_OFF_L  : constant Register := 8;  -- LED0 output and brightness control byte 2
   LED0_OFF_H  : constant Register := 9;  -- LED0 output and brightness control byte 3
   LED1_ON_L   : constant Register := 10; -- ""
   LED1_ON_H   : constant Register := 11;
   LED1_OFF_L  : constant Register := 12;
   LED1_OFF_H  : constant Register := 13;
   LED2_ON_L   : constant Register := 14;
   LED2_ON_H   : constant Register := 15;
   LED2_OFF_L  : constant Register := 16;
   LED2_OFF_H  : constant Register := 17;
   LED3_ON_L   : constant Register := 18;
   LED3_ON_H   : constant Register := 19;
   LED3_OFF_L  : constant Register := 20;
   LED3_OFF_H  : constant Register := 21;
   LED4_ON_L   : constant Register := 22;
   LED4_ON_H   : constant Register := 23;
   LED4_OFF_L  : constant Register := 24;
   LED4_OFF_H  : constant Register := 25;
   LED5_ON_L   : constant Register := 26;
   LED5_ON_H   : constant Register := 27;
   LED5_OFF_L  : constant Register := 28;
   LED5_OFF_H  : constant Register := 29;
   LED6_ON_L   : constant Register := 30;
   LED6_ON_H   : constant Register := 31;
   LED6_OFF_L  : constant Register := 32;
   LED6_OFF_H  : constant Register := 33;
   LED7_ON_L   : constant Register := 34;
   LED7_ON_H   : constant Register := 35;
   LED7_OFF_L  : constant Register := 36;
   LED7_OFF_H  : constant Register := 37;
   LED8_ON_L   : constant Register := 38;
   LED8_ON_H   : constant Register := 39;
   LED8_OFF_L  : constant Register := 40;
   LED8_OFF_H  : constant Register := 41;
   LED9_ON_L   : constant Register := 42;
   LED9_ON_H   : constant Register := 43;
   LED9_OFF_L  : constant Register := 44;
   LED9_OFF_H  : constant Register := 45;
   LED10_ON_L  : constant Register := 46;
   LED10_ON_H  : constant Register := 47;
   LED10_OFF_L : constant Register := 48;
   LED10_OFF_H : constant Register := 49;
   LED11_ON_L  : constant Register := 50;
   LED11_ON_H  : constant Register := 51;
   LED11_OFF_L : constant Register := 52;
   LED11_OFF_H : constant Register := 53;
   LED12_ON_L  : constant Register := 54;
   LED12_ON_H  : constant Register := 55;
   LED12_OFF_L : constant Register := 56;
   LED12_OFF_H : constant Register := 57;
   LED13_ON_L  : constant Register := 58;
   LED13_ON_H  : constant Register := 59;
   LED13_OFF_L : constant Register := 60;
   LED13_OFF_H : constant Register := 61;
   LED14_ON_L  : constant Register := 62;
   LED14_ON_H  : constant Register := 63;
   LED14_OFF_L : constant Register := 64;
   LED14_OFF_H : constant Register := 65;
   LED15_ON_L  : constant Register := 66;
   LED15_ON_H  : constant Register := 67;
   LED15_OFF_L : constant Register := 68;
   LED15_OFF_H : constant Register := 69;
   --  69-249 reserved for future use
   ALL_LED_ON_L  : constant Register := 250;  -- Load all the LEDn_ON registers, byte 0
   ALL_LED_ON_H  : constant Register := 251;  -- Load all the LEDn_ON registers, byte 1
   ALL_LED_OFF_L : constant Register := 252;  -- Load all the LEDn_OFF registers, byte 0
   ALL_LED_OFF_H : constant Register := 253;  -- Load all the LEDn_OFF registers, byte 1
   PRE_SCALE     : constant Register := 254;  -- Prescaler for output frequency
   TESTMODE      : constant Register := 255;  -- Defines the test mode to be entered

   --  Name the MODE1 bits.
   RESTART : constant Byte := 2 ** 7;
   EXTCLK  : constant Byte := 2 ** 6;
   AI      : constant Byte := 2 ** 5;
   SLEEP   : constant Byte := 2 ** 4;
   SUB1    : constant Byte := 2 ** 3;
   SUB2    : constant Byte := 2 ** 2;
   SUB3    : constant Byte := 2 ** 1;
   ALLCALL : constant Byte := 2 ** 0;

   --  Name the MODE2 bits.
   --  5-7 read only, reserved
   INVRT   : constant Byte := 2 ** 4;
   OCH     : constant Byte := 2 ** 3;
   OUTDRV  : constant Byte := 2 ** 2;
   OUTNE1  : constant Byte := 2 ** 1;
   OUTNE0  : constant Byte := 2 ** 0;

   procedure Initialize (C : in out Chip);
   procedure Finalize (C : in out Chip);
end I2C.PCA9685;
