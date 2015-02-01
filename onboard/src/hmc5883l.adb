with Interfaces; use Interfaces;
with Ada.IO_Exceptions;
with Ada.Real_Time; use Ada.Real_Time;
with Ada.Numerics.Elementary_Functions; use Ada.Numerics.Elementary_Functions;

package body HMC5883L is
   procedure Reset (C : in Chip) is
   begin
      C.Write_Byte_Data (ConfigurationA, 16#70#);
      C.Write_Byte_Data (ConfigurationB, 16#20#);
      C.Write_Byte_Data (Mode, 16#00#); --  Continous mode
   end Reset;

   function Self_Test (C : in Chip) return Boolean is
      Values : Vector_Math.Float3;
   begin
      C.Write_Byte_Data (ConfigurationA, 16#71#); --  8avg, 15hz, test
      C.Write_Byte_Data (ConfigurationB, 16#A0#); --  gain 5
      C.Write_Byte_Data (Mode, 16#01#); --  oneshot mode, init selftest

      C.Wait_Ready;
      Values := C.Get_Axes;
      C.Reset;

      if (Values.x >= 243.0 and Values.x <= 575.0) then
         if (Values.y >= 243.0 and Values.y <= 575.0) then
            if (Values.z >= 243.0 and Values.z <= 575.0) then
               return True;
            end if;
         end if;
      end if;
      return False;
   end Self_Test;

   function Get_Heading (C : in Chip) return Float is
      Axes : constant Vector_Math.Float3 := C.Get_Axes;
      Heading : Float := Arctan (Y => Axes.y,
                                 X => Axes.x,
                                 Cycle => Ada.Numerics.Pi); --  use radians
      --  TODO add declination to Heading
   begin
      if Heading < 0.0 then --  Correct for reversed heading
         Heading := Heading + (2.0*Ada.Numerics.Pi);
      end if;
      if Heading > Float (2*Ada.Numerics.Pi) then --  Check wrap, compensate
         Heading := Heading - (2.0*Ada.Numerics.Pi);
      end if;
      return Heading * (180.0/Ada.Numerics.Pi);
   end Get_Heading;

   function Get_Axes (C : in Chip) return Vector_Math.Float3 is
      Values : Byte_Array (0 .. 5);
      Output : Vector_Math.Float3;
      X, Y, Z : Word;
   begin
      Values := C.Read_Array_Data (X_L, 6);
      X := Shift_Left (Word (Values (0)), 8) or Word (Values (1));
      Z := Shift_Left (Word (Values (2)), 8) or Word (Values (3));
      Y := Shift_Left (Word (Values (4)), 8) or Word (Values (5));
      Output.x := Float (To_Int (X));
      Output.z := Float (To_Int (Z));
      Output.y := Float (To_Int (Y));
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

   procedure Wait_Ready (C : in Chip;
                         Timeout : in Duration := 1.0) is
      Ready : Byte := 2;
      Finish_Time : Time;
      Start_Time : constant Time := Clock;
      Taken_Time : Duration;
   begin
      loop
         Ready := C.Read_Bit_Data (Status, 0);
         exit when Ready = 0;
      end loop;
      loop
         Ready := C.Read_Bit_Data (Status, 0);
         exit when Ready = 1;
      end loop;
      Finish_Time := Clock;

      Taken_Time := To_Duration (Finish_Time - Start_Time);
      if Taken_Time > Timeout then
         raise Ada.IO_Exceptions.Device_Error with
         "Get_Ready_Status exceeded timeout with" &
         Float'Image (Float (Taken_Time - Timeout)) & "s";
      end if;
   end Wait_Ready;
end HMC5883L;
