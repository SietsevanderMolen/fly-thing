with Interfaces; use Interfaces;

package body HMC5883L is
   procedure Reset (C : in Chip) is
      Ready : Byte := 0;
   begin
      C.Write_Byte_Data (ConfigurationA, 16#18#);
      C.Write_Byte_Data (ConfigurationB, 16#A0#);
      C.Write_Byte_Data (Mode, 16#00#); --  Continous mode
      loop
         Ready := C.Read_Byte_Data (Status);
         exit when (Ready and 1) = 1;
      end loop;
   end Reset;

   function Self_Test (C : in Chip) return Boolean is
   begin
      return True;
   end Self_Test;

   function Get_Axes (C : in Chip) return Vector_Math.Int3 is
      Values : Byte_Array (0 .. 5);
      Output : Vector_Math.Int3;
   begin
      Values := C.Read_Array_Data (X_L, 6);
      Output.x := Integer (Shift_Left (Values (0), 8) or Values (1));
      Output.z := Integer (Shift_Left (Values (2), 8) or Values (3));
      Output.y := Integer (Shift_Left (Values (4), 8) or Values (5));
      return Output;
   end Get_Axes;
end HMC5883L;
