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
   procedure Set_Declination (C : in Chip;
                              Degrees : in Degree;
                              Minutes : in Minute);
private
   --  One magnetometer axis reading, eg X, Y or Z.
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

   ConfigurationA  : constant Register := 16#00#;
   ConfigurationB  : constant Register := 16#01#;
   Mode            : constant Register := 16#02#;
   X_L             : constant Register := 16#03#;
   Status          : constant Register := 16#09#;
   IdentificationA : constant Register := 16#10#;
   IdentificationB : constant Register := 16#11#;
   IdentificationC : constant Register := 16#12#;

   Declination : Float;

   --  Get the axes' raw values
   function Get_Axes (C : in Chip) return Vector_Math.Int3;
   --  Convert a raw axis reading into it's two's complement representation
   function To_Integer is
      new Ada.Unchecked_Conversion (Source => Axis_Reading,
                                    Target => Interfaces.Integer_16);
   --  Read the RDY register. It turns high when new values are written to the
   --  data registers
   procedure Wait_Ready (C : in Chip;
                         Timeout : in Duration := 1.0);
end HMC5883L;
