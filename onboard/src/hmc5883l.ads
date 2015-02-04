with I2C; use I2C;
with Vector_Math;
with Interfaces;
with Ada.Unchecked_Conversion;

package HMC5883L is
   type Chip is new I2C.Chip with null record;

   subtype Degree is Integer range 0 .. 359;
   subtype Minute is Integer range 0 .. 59;

   type Gain is (Gain_0, Gain_1, Gain_2, Gain_3,
                 Gain_4, Gain_5, Gain_6, Gain_7);
   LSb_Per_Gauss_List : constant array (0 .. 7) of Float
      := (1370.0, 1090.0, 820.0, 660.0, 440.0, 390.0, 330.0, 230.0);

   --  Reset the HMC5883L to default settings
   procedure Reset (C : in Chip);
   --  Set the gain
   procedure Set_Gain (C : in Chip; G : in Gain);
   --  Run the self test procedure, return True if passed
   function Self_Test (C : in Chip) return Boolean;
   --  Return the current heading in deg as a Float
   function Get_Heading (C : in Chip) return Float;
   --  Set the current declination
   --  Magnetic declination or variation is the angle on the horizontal plane
   --  between magnetic north and true north . This angle varies depending on
   --  position on the Earth's surface, and changes over time.
   procedure Set_Declination (C : in Chip;
                              Degrees : in Degree;
                              Minutes : in Minute);
private
   --  One magnetometer axis reading, eg X, Y or Z
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
   function Pack is
      new Ada.Unchecked_Conversion (Source => Axis_Reading,
                                    Target => Interfaces.Integer_16);

   --  Configuration register a
   type ConfigA is
      record
         Pad : Integer range 0 .. 0;
         Averaged_Samples : Integer range 0 .. 3;
         Data_Output_Rate : Integer range 0 .. 6;
         Measurement_Mode : Integer range 0 .. 2;
      end record;
   for ConfigA use
      record
         Pad at 0 range 7 .. 7;
         Averaged_Samples at 0 range 5 .. 6;
         Data_Output_Rate at 0 range 2 .. 4;
         Measurement_Mode at 0 range 0 .. 1;
      end record;
   ConfigA_Address : constant Register := 16#00#;
   function Pack is new Ada.Unchecked_Conversion (Source => ConfigA,
                                                  Target => Byte);

   --  Configuration register b
   type ConfigB is
      record
         Gain : Integer range 0 .. 7;
         Pad : Integer range 0 .. 0;
      end record;
   for ConfigB use
      record
         Gain at 0 range 5 .. 7;
         Pad at 0 range 0 .. 4;
      end record;
   ConfigB_Address : constant Register := 16#01#;
   function Pack is new Ada.Unchecked_Conversion (Source => ConfigB,
                                                  Target => Byte);

   --  Mode register
   type Mode is
      record
         High_Speed_Enable : Integer range 0 .. 1;
         Pad : Integer range 0 .. 0;
         Mode_Select : Integer range 0 .. 3;
      end record;
   for Mode use
      record
         High_Speed_Enable at 0 range 7 .. 7;
         Pad at 0 range 2 .. 6;
         Mode_Select at 0 range 0 .. 1;
      end record;
   Mode_Address : constant Register := 16#02#;
   function Pack is new Ada.Unchecked_Conversion (Source => Mode,
                                                  Target => Byte);

   X_L : constant Register := 16#03#;
   Status_Address : constant Register := 16#09#;

   Declination : Float;

   --  Get the axes' raw values
   function Get_Axes (C : in Chip) return Vector_Math.Int3;
   --  Read the RDY register. It turns high when new values are written to the
   --  data registers
   procedure Wait_Ready (C : in Chip;
                         Timeout : in Duration := 1.0);
end HMC5883L;
