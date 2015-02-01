with Interfaces; use Interfaces;

package body HMC5883L is
   procedure Reset (C : in Chip) is
   begin
      C.Write_Byte_Data (ConfigurationA, 16#18#);
      C.Write_Byte_Data (ConfigurationB, 16#A0#);
      C.Write_Byte_Data (Mode, 16#00#); --  Continous mode
      C.Wait_Ready;
   end Reset;

   function Self_Test (C : in Chip) return Boolean is
      Values : Vector_Math.Int3;
      X, Y, Z : Integer := 0;
   begin
      C.Write_Byte_Data (ConfigurationA, 16#71#); --  8avg, 15hz, test
      C.Write_Byte_Data (ConfigurationB, 16#A0#); --  gain 5
      C.Write_Byte_Data (Mode, 16#00#); --  Continous mode
      C.Wait_Ready;

      Read_Loop :
         for I in 1 .. 8 loop --  take and average out 8 measurements
            Values := C.Get_Axes;
            X := X + Values.x;
            Y := Y + Values.y;
            Z := Z + Values.z;
            delay (0.067);
         end loop Read_Loop;

      C.Reset;

      if ((X / 8) >= 243 and (X / 8) <= 575) then
         if ((Y / 8) >= 243 and (Y / 8) <= 575) then
            if ((Z / 8) >= 243 and (Z / 8) <= 575) then
               return True;
            end if;
         end if;
      end if;
      return False;
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

   procedure Wait_Ready (C : in Chip) is
      Ready : Byte := 0;
   begin
      loop
         Ready := C.Read_Byte_Data (Status);
         exit when (Ready and 1) = 1;
      end loop;
   end Wait_Ready;
end HMC5883L;
