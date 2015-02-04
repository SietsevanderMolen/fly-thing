with Interfaces; use Interfaces;
with Ada.IO_Exceptions;
with Ada.Real_Time; use Ada.Real_Time;
with Ada.Numerics.Elementary_Functions; use Ada.Numerics.Elementary_Functions;

package body HMC5883L is
   procedure Reset (C : in Chip) is
      Settings : ConfigA;
   begin
      Settings.Averaged_Samples := 3;
      Settings.Data_Output_Rate := 4;
      Settings.Measurement_Mode := 0;
      C.Write_Byte_Data (ConfigA_Address, Pack (Settings));
      C.Set_Gain (Gain_1);
      C.Write_Byte_Data (Mode_Address, 16#00#); --  Continous mode
   end Reset;

   procedure Set_Gain (C : in Chip; G : in Gain) is
      Settings : ConfigB;
   begin
      Settings.Gain := Integer (LSb_Per_Gauss_List (Gain'Pos (G)));
      C.Write_Byte_Data (ConfigB_Address, Pack (Settings));
   end Set_Gain;

   procedure Set_Declination (C : in Chip;
                              Degrees : in Degree;
                              Minutes : in Minute) is
   begin
      Declination := Float ((Degrees + Minutes / 60)) *
                     Float ((Ada.Numerics.Pi / 180));
   end Set_Declination;

   --  A self test feature is incorporated in which the sensor offset straps
   --  are excited to create a nominal field strength (bias field) to be
   --  measured.  To implement self test, the least significant bits (MS1 and
   --  MS0) of configuration register A are changed from 00 to 01 (positive
   --  bias) or 10 (negative bias).  Then, by placing the mode register into
   --  single or continuous - measurement mode, two data acquisition cycles
   --  will be made on each magnetic vector. The first acquisition will be a
   --  set pulse followed shortly by measurement data of the external field.
   --  The second acquisition will have the offset strap excited (about 10 mA)
   --  in the positive bias mode for X, Y, and Z axes to create about a 1.1
   --  gauss self test field plus the external field. The first acquisition
   --  values will be subtracted from the second acquisition, and the net
   --  measurement will be placed into the data output registers
   function Self_Test (C : in Chip) return Boolean is
      Old_A : constant Byte := C.Read_Byte_Data (ConfigA_Address);
      Old_B : constant Byte := C.Read_Byte_Data (ConfigB_Address);
      Old_Mode : constant Byte := C.Read_Byte_Data (Mode_Address);
   begin
      C.Write_Byte_Data (ConfigA_Address, 16#71#); --  8avg, 15hz, test
      C.Write_Byte_Data (Mode_Address, 16#00#); --  continuous, init selftest

      for G in Gain'Range loop
         declare
            Values : Vector_Math.Int3; --  hold the values that are returned
            --  Take the current Gain setting and finds the right LSb/Gauss
            LSb_Per_Gauss : constant Float :=
               LSb_Per_Gauss_List (Gain'Pos (G));
            --  Minimum value to be considered
            Lo_Limit : constant Integer :=
               Integer (243.0 * (LSb_Per_Gauss / 390.0));
            Hi_Limit : constant Integer :=
               Integer (575.0 * (LSb_Per_Gauss / 390.0));
         begin
            C.Set_Gain (G);
            C.Wait_Ready; --  wait until values with new gain are written
            Values := C.Get_Axes;

            if (Values.x >= Lo_Limit and Values.x <= Hi_Limit) and
               (Values.y >= Lo_Limit and Values.y <= Hi_Limit) and
               (Values.z >= Lo_Limit and Values.z <= Hi_Limit)
            then
               C.Write_Byte_Data (ConfigA_Address, Old_A);
               C.Write_Byte_Data (ConfigB_Address, Old_B);
               C.Write_Byte_Data (Mode_Address, Old_Mode);
               return True; --  Self test complete
            end if;
         end;
      end loop;
      return False; --  Self test failed
   end Self_Test;

   function Get_Heading (C : in Chip) return Float is
      Axes : constant Vector_Math.Int3 := C.Get_Axes;
      Heading : Float := Arctan (Y => Float (Axes.y),
                                 X => Float (Axes.x),
                                 Cycle => 2.0*Ada.Numerics.Pi); --  use radians
   begin
      Heading := Heading + Declination; --  Correct for declination
      if Heading < 0.0 then --  Correct for reversed heading
         Heading := Heading + (2.0*Ada.Numerics.Pi);
      end if;
      if Heading > Float (2*Ada.Numerics.Pi) then --  Check wrap, compensate
         Heading := Heading - (2.0*Ada.Numerics.Pi);
      end if;
      return Heading * (180.0/Ada.Numerics.Pi);
   end Get_Heading;

   function Get_Axes (C : in Chip) return Vector_Math.Int3 is
      Values : constant Byte_Array (0 .. 5) := C.Read_Array_Data (X_L, 6);
      Output : Vector_Math.Int3;
      X, Y, Z : Axis_Reading;
   begin
      X.H := Values (0);
      X.L := Values (1);
      Z.H := Values (2);
      Z.L := Values (3);
      Y.H := Values (4);
      Y.L := Values (5);

      Output.x := Integer (Pack (X));
      Output.z := Integer (Pack (Z));
      Output.y := Integer (Pack (Y));
      return Output;
   end Get_Axes;

   procedure Wait_Ready (C : in Chip;
                         Timeout : in Duration := 1.0) is
      Ready : Byte := 1;
      Finish_Time : Time;
      Start_Time : constant Time := Clock;
      Taken_Time : Duration;
   begin
      loop
         Ready := C.Read_Bit_Data (Status_Address, 0);
         exit when Ready = 0;
      end loop;
      loop
         Ready := C.Read_Bit_Data (Status_Address, 0);
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
