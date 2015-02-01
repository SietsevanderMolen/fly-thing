with Interfaces; use Interfaces;

package body HMC5883L is
   procedure Reset (C : in Chip) is
   begin
      C.Write_Byte_Data (ConfigurationA, 16#70#);
      C.Write_Byte_Data (ConfigurationB, 16#A0#);
      C.Write_Byte_Data (Mode, 16#00#); --  Continous mode
      C.Wait_Ready;
   end Reset;

   function Self_Test (C : in Chip) return Boolean is
      Values : Vector_Math.Int3;
   begin
      C.Write_Byte_Data (ConfigurationA, 16#71#); --  8avg, 15hz, test
      C.Write_Byte_Data (ConfigurationB, 16#A0#); --  gain 5
      C.Write_Byte_Data (Mode, 16#00#); --  Continous mode

      C.Wait_Ready;
      Values := C.Get_Axes;
      C.Reset;

      if (Values.x >= 243 and Values.x <= 575) then
         if (Values.y >= 243 and Values.y <= 575) then
            if (Values.z >= 243 and Values.z <= 575) then
               return True;
            end if;
         end if;
      end if;
      return False;
   end Self_Test;

   function Get_Axes (C : in Chip) return Vector_Math.Int3 is
      Values : Byte_Array (0 .. 5);
      Output : Vector_Math.Int3;
      X, Y, Z : Word;
   begin
      Values := C.Read_Array_Data (X_L, 6);
      X := Shift_Left (Word (Values (0)), 8) or Word (Values (1));
      Z := Shift_Left (Word (Values (2)), 8) or Word (Values (3));
      Y := Shift_Left (Word (Values (4)), 8) or Word (Values (5));
      Output.x := To_Int (X);
      Output.z := To_Int (Z);
      Output.y := To_Int (Y);
      return Output;
   end Get_Axes;

   function To_Int (V : Word) return Integer is
      Output : Integer;
   begin
      if V >= 32768 then
         Output := Integer ((not V)+1);
         return Output;
      end if;
      return Integer (V);
   end To_Int;

   procedure Wait_Ready (C : in Chip) is
      Ready : Byte := 2;
   begin
      loop
         Ready := C.Read_Bit_Data (Status, 0);
         exit when Ready = 0;
      end loop;
      loop
         Ready := C.Read_Bit_Data (Status, 0);
         exit when Ready = 1;
      end loop;
   end Wait_Ready;
end HMC5883L;
