with I2C; use I2C;
with Vector_Math;
with Interfaces;
with Ada.Unchecked_Conversion;

package HMC5883L is
   type Chip is new I2C.Chip with null record;

   subtype Degree is Integer range 0 .. 359;
   subtype Minute is Integer range 0 .. 59;

   --  Reset the HMC5883L to default settings
   procedure Reset (C : in Chip);
   --  Run the self test procedure, return True if passed
   function Self_Test (C : in Chip) return Boolean;
   --  Return the current heading in deg as a Float
   function Get_Heading (C : in Chip) return Float;
   --  Set the current declination
   procedure Set_Declination (C : in Chip;
                              Degrees : in Degree;
                              Minutes : in Minute);
private
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
   X_H             : constant Register := 16#04#;
   Z_L             : constant Register := 16#05#;
   Z_H             : constant Register := 16#06#;
   Y_L             : constant Register := 16#07#;
   Y_H             : constant Register := 16#08#;
   Status          : constant Register := 16#09#;
   IdentificationA : constant Register := 16#10#;
   IdentificationB : constant Register := 16#11#;
   IdentificationC : constant Register := 16#12#;

   Declination : Float;

   function Get_Axes (C : in Chip) return Vector_Math.Int3;
   function To_Integer is
      new Ada.Unchecked_Conversion (Source => Axis_Reading,
                                    Target => Interfaces.Integer_16);
   procedure Wait_Ready (C : in Chip;
                         Timeout : in Duration := 1.0);
end HMC5883L;
