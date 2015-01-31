with Interfaces; use Interfaces;

package body HMC5883L is
   function Self_Test (C : in Chip) return Boolean is
   begin
      return True;
   end Self_Test;

   function Get_Axes (C : in Chip) return Vector_Math.Int3 is
      Output : Vector_Math.Int3;
      Values : constant Byte_Array :=
         C.Read_Array_Data (X_L, 6);
   begin
      Output.x := Integer (
         Shift_Left (Byte (8), Values'First + 0) or Byte (Values'First + 1));
      Output.z := Integer (
         Shift_Left (Byte (8), Values'First + 2) or Byte (Values'First + 3));
      Output.y := Integer (
         Shift_Left (Byte (8), Values'First + 4) or Byte (Values'First + 5));
      C.Write_Byte (16#03#);
      return Output;
   end Get_Axes;
end HMC5883L;
