with Ada.Real_Time; use Ada.Real_Time;

package PIDs is
   type PID is private;

   --  Automatic mode is the normal mode
   --  Manual overrides all PID functionality
   type Mode is (Automatic, Manual);

   type Direction is (Direct, Reversed);

   function Make (Kp : Float;
                  Ki : Float;
                  Kd : Float;
                  Output_Min : Float;
                  Output_Max : Float;
                  Setpoint : Float;
                  Sample_Rate : Integer) return PID;

   --  Retune proportional band, integral and derivative constants
   procedure Tune (P : out PID; Kp : Float; Ki : Float; Kd : Float)
      with Pre => Kp > 0.0 and
                  Ki > 0.0 and
                  Kd > 0.0;

   --  Set the sample rate, is converted to seconds internally
   procedure Set_Sample_Rate (P : out PID; Sample_Rate : Integer)
      with Pre => Sample_Rate > 0;

   --  Set the output limits this controller will stay within
   procedure Set_Output_Limits (P : out PID; Min : Float; Max : Float)
      with Pre => Max > Min;

   procedure Set_Mode (P : out PID; New_Mode : Mode);
   procedure Set_Direction (P : out PID; New_Direction : Direction);

   function Compute (P : in out PID) return Float;
private
   type PID is tagged
      record
         Kp : Float;
         Ki : Float;
         Kd : Float;
         Sample_Time : Time_Span;
         Output_Min : Float;
         Output_Max : Float;

         Input : Float;
         Output : Float;
         Setpoint : Float;

         Current_Mode : Mode := Automatic;
         Current_Direction : Direction := Direct;

         I_Term : Float;
         Last_Time : Ada.Real_Time.Time;
         Last_Input : Float;
      end record;

   --  Helper function to clamp a float between to values
   function Clamp (Num : Float;
                   Min : Float;
                   Max : Float) return Float;
end PIDs;
