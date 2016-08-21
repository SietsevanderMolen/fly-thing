package body PIDs is
   function Make (Kp : Float; Ki : Float; Kd : Float;
                  Output_Min : Float; Output_Max : Float;
                  Setpoint : Float; Sample_Rate : Integer)
      return PID
   is
   begin
      return (Kp => Kp, Ki => Ki, Kd => Kd,
             Sample_Time => Seconds (1) / Sample_Rate,
             Setpoint => Setpoint,

             Output_Min => Output_Min, Output_Max => Output_Max,
             Input => 0.0, Output => 0.0,

             I_Term => 0.0,
             Last_Input => 0.0, Last_Time => Clock,
             Current_Mode => Automatic, Current_Direction => Direct);
   end Make;

   procedure Tune (P : out PID; Kp : Float; Ki : Float; Kd : Float)
   is
   begin
      --  Adjust ki and kd for fixed sample rate
      P.Kp := Kp;
      P.Ki := Ki * Float (To_Duration (P.Sample_Time));
      P.Kp := Kd / Float (To_Duration (P.Sample_Time));

      if P.Current_Direction = Reversed then
         P.Kp := 0.0 - P.Kp;
         P.Ki := 0.0 - P.Ki;
         P.Kd := 0.0 - P.Kd;
      end if;
   end Tune;

   procedure Set_Sample_Rate (P : out PID;
                              Sample_Rate : Integer)
   is
      --  convert from rate (hz) to time_span
      New_Sample_Time : constant Time_Span := Seconds (1) / Sample_Rate;
      --  update with the new ratio in case rate gets changed during runtime
      Ratio : constant Float := Float (To_Duration (New_Sample_Time)) /
                                Float (To_Duration (P.Sample_Time));
   begin
      P.Ki := P.Ki * Ratio;
      P.Kd := P.Kd / Ratio;
      P.Sample_Time := New_Sample_Time;
   end Set_Sample_Rate;

   procedure Set_Output_Limits (P : out PID; Min : Float; Max : Float)
   is
   begin
      P.Output_Min := Min;
      P.Output_Max := Max;
      P.Output := Clamp (P.Output, P.Output_Min, P.Output_Max);
      P.I_Term := Clamp (P.I_Term, P.Output_Min, P.Output_Max);
   end Set_Output_Limits;

   procedure Set_Mode (P : out PID; New_Mode : Mode)
   is
   begin
      --  when switching to automatic
      if New_Mode = Automatic and P.Current_Mode = Manual then
         --  keep derivative from spiking
         P.Last_Input := P.Input;
         --  and reinitialize integral term
         P.I_Term := Clamp (P.Output, P.Output_Min, P.Output_Max);
      end if;
      P.Current_Mode := New_Mode;
   end Set_Mode;

   procedure Set_Direction (P : out PID; New_Direction : Direction)
   is
   begin
      P.Current_Direction := New_Direction;
   end Set_Direction;

   function Compute (P : in out PID) return Float
   is
      Now : constant Ada.Real_Time.Time := Clock;
      Time_Change : constant Time_Span := Now - P.Last_Time;
   begin
      if P.Current_Mode = Automatic then
         if Time_Change >= P.Sample_Time then
            declare
               Error : constant Float := P.Setpoint - P.Input;
               I_Term : constant Float := P.I_Term + (P.Ki * Error);
               Input_Derivative : constant Float := P.Input - P.Last_Input;
            begin
               --  clamp integral term to avoid reset windup
               P.I_Term := Clamp (I_Term, P.Output_Min, P.Output_Max);
               P.Output := P.Kp * Error +
                           P.I_Term -
                           P.Kd * Input_Derivative;
               --  clamp output to avoid reset windup
               P.Output := Clamp (P.Output, P.Output_Min, P.Output_Max);
               P.Last_Input := P.Input;
               P.Last_Time := Now;
            end;
         end if;
      end if;
      return P.Output;
   end Compute;

   function Clamp (Num : Float;
                   Min : Float;
                   Max : Float) return Float
   is
   begin
      if Num > Max then
         return Max;
      elsif Num < Min then
         return Min;
      else
         return Num;
      end if;
   end Clamp;
end PIDs;
