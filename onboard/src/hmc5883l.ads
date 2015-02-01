with I2C; use I2C;
with Vector_Math;

package HMC5883L is
   type Chip is new I2C.Chip with null record;

   procedure Reset (C : in Chip);

   function Self_Test (C : in Chip) return Boolean;

   function Get_Axes (C : in Chip) return Vector_Math.Int3;
private
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
end HMC5883L;
