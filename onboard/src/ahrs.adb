with Ada.Numerics.Elementary_Functions; use Ada.Numerics.Elementary_Functions;

package body AHRS is
   function Make (Sample_Period : Float;
                  Proportional_Gain : Float;
                  Integral_Gain : Float)
      return Mahony
   is
      M : Mahony;
   begin
      M.Sample_Period := Sample_Period;
      M.Proportional_Gain := Proportional_Gain;
      M.Integral_Gain := Integral_Gain;
      M.Integral_Error := (0.0, 0.0, 0.0);
      M.Output := (1.0, 0.0, 0.0, 0.0);
      return M;
   end Make;

   procedure Update (M : in out Mahony;
                    Accelerometer : Vector_Math.Float3;
                    Gyroscope : Vector_Math.Float3;
                    Magnetometer : Vector_Math.Float3)
   is
      q1 : Float := M.Output.A;
      q2 : Float := M.Output.B;
      q3 : Float := M.Output.C;
      q4 : Float := M.Output.D;
      norm : Float;
      hx, hy, bx, bz : Float;
      vx, vy, vz, wx, wy, wz : Float;
      ex, ey, ez : Float;
      pa, pb, pc : Float;

      ax, ay, az, gx, gy, gz, mx, my, mz : Float;

      --  Auxiliary variables to avoid repeated arithmetic
      q1q1 : constant Float := q1 * q1;
      q1q2 : constant Float := q1 * q2;
      q1q3 : constant Float := q1 * q3;
      q1q4 : constant Float := q1 * q4;
      q2q2 : constant Float := q2 * q2;
      q2q3 : constant Float := q2 * q3;
      q2q4 : constant Float := q2 * q4;
      q3q3 : constant Float := q3 * q3;
      q3q4 : constant Float := q3 * q4;
      q4q4 : constant Float := q4 * q4;

   begin
      --  Normalize accelerometer measurement
      norm := Sqrt (Accelerometer.x * Accelerometer.x +
                    Accelerometer.y * Accelerometer.y +
                    Accelerometer.z * Accelerometer.z);
      norm := 1.0 / norm; --  use reciprocal for division
      ax := Accelerometer.x * norm;
      ay := Accelerometer.y * norm;
      az := Accelerometer.z * norm;

      --  Normalize magnetometer measurement
      norm := Sqrt (Magnetometer.x * Magnetometer.x +
                    Magnetometer.y * Magnetometer.y +
                    Magnetometer.z * Magnetometer.z);
      norm := 1.0 / norm; --  use reciprocal for division
      mx := Magnetometer.x * norm;
      my := Magnetometer.y * norm;
      mz := Magnetometer.z * norm;

      --  Reference direction of Earth's magnetic field
      hx := 2.0 * mx * (0.5 - q3q3 - q4q4) + 2.0 * my * (q2q3 - q1q4) + 2.0 * mz * (q2q4 + q1q3);
      hy := 2.0 * mx * (q2q3 + q1q4) + 2.0 * my * (0.5 - q2q2 - q4q4) + 2.0 * mz * (q3q4 - q1q2);
      bx := Sqrt ((hx * hx) + (hy * hy));
      bz := 2.0 * mx * (q2q4 - q1q3) + 2.0 * my * (q3q4 + q1q2) + 2.0 * mz * (0.5 - q2q2 - q3q3);

      --  Estimated direction of gravity and magnetic field
      vx := 2.0 * (q2q4 - q1q3);
      vy := 2.0 * (q1q2 + q3q4);
      vz := q1q1 - q2q2 - q3q3 + q4q4;
      wx := 2.0 * bx * (0.5 - q3q3 - q4q4) + 2.0 * bz * (q2q4 - q1q3);
      wy := 2.0 * bx * (q2q3 - q1q4) + 2.0 * bz * (q1q2 + q3q4);
      wz := 2.0 * bx * (q1q3 + q2q4) + 2.0 * bz * (0.5 - q2q2 - q3q3);

      --  Error is cross product between estimated direction and measured direction of gravity
      ex := (ay * vz - az * vy) + (my * wz - mz * wy);
      ey := (az * vx - ax * vz) + (mz * wx - mx * wz);
      ez := (ax * vy - ay * vx) + (mx * wy - my * wx);

      if M.Integral_Gain > 0.0 then
         M.Integral_Error.x := M.Integral_Error.x + ex; --  accumulate integral error
         M.Integral_Error.y := M.Integral_Error.y + ey;
         M.Integral_Error.z := M.Integral_Error.z + ez;
      else
         M.Integral_Error.x := 0.0; --  accumulate integral error
         M.Integral_Error.y := 0.0;
         M.Integral_Error.z := 0.0;
      end if;

      --  Apply feedback terms
      gx := Gyroscope.x + M.Proportional_Gain * ex + M.Integral_Gain * M.Integral_Error.x;
      gy := Gyroscope.y + M.Proportional_Gain * ey + M.Integral_Gain * M.Integral_Error.y;
      gz := Gyroscope.z + M.Proportional_Gain * ez + M.Integral_Gain * M.Integral_Error.z;

      --  Integrate rate of change of quaternion
      pa := q2;
      pb := q3;
      pc := q4;
      q1 := q1 + (-q2 * gx - q3 * gy - q4 * gz) * (0.5 * M.Sample_Period);
      q2 := pa + (q1 * gx + pb * gz - pc * gy) * (0.5 * M.Sample_Period);
      q3 := pb + (q1 * gy - pa * gz + pc * gx) * (0.5 * M.Sample_Period);
      q4 := pc + (q1 * gz + pa * gy - pb * gx) * (0.5 * M.Sample_Period);

      M.Output := Normalize ((q1, q2, q3, q4));
   end Update;

   procedure Update (M : in out Mahony;
                    Accelerometer : Vector_Math.Float3;
                    Gyroscope : Vector_Math.Float3)
   is
      q1 : Float := M.Output.A;
      q2 : Float := M.Output.B;
      q3 : Float := M.Output.C;
      q4 : Float := M.Output.D;
      norm : Float;

      vx, vy, vz : Float;
      ex, ey, ez : Float;
      pa, pb, pc : Float;

      ax, ay, az, gx, gy, gz : Float;
   begin
      --  Normalize accelerometer measurement
      norm := Sqrt (Accelerometer.x * Accelerometer.x +
                    Accelerometer.y * Accelerometer.y +
                    Accelerometer.z * Accelerometer.z);
      norm := 1.0 / norm; --  use reciprocal for division
      ax := Accelerometer.x * norm;
      ay := Accelerometer.y * norm;
      az := Accelerometer.z * norm;

      --  Estimated direction of gravity
      vx := 2.0 * (q2 * q4 - q1 * q3);
      vy := 2.0 * (q1 * q2 + q3 * q4);
      vz := q1 * q1 - q2 * q2 - q3 * q3 + q4 * q4;

      --  Error is cross product between estimated direction and measured direction of gravity
      ex := (ay * vz - az * vy);
      ey := (az * vx - ax * vz);
      ez := (ax * vy - ay * vx);

      if M.Integral_Gain > 0.0 then
         M.Integral_Error.x := M.Integral_Error.x + ex; --  accumulate integral error
         M.Integral_Error.y := M.Integral_Error.y + ey;
         M.Integral_Error.z := M.Integral_Error.z + ez;
      else
         M.Integral_Error.x := 0.0; --  accumulate integral error
         M.Integral_Error.y := 0.0;
         M.Integral_Error.z := 0.0;
      end if;

      --  Apply feedback terms
      gx := Gyroscope.x + M.Proportional_Gain * ex + M.Integral_Gain * M.Integral_Error.x;
      gy := Gyroscope.y + M.Proportional_Gain * ey + M.Integral_Gain * M.Integral_Error.y;
      gz := Gyroscope.z + M.Proportional_Gain * ez + M.Integral_Gain * M.Integral_Error.z;

      --  Integrate rate of change of quaternion
      pa := q2;
      pb := q3;
      pc := q4;
      q1 := q1 + (-q2 * gx - q3 * gy - q4 * gz) * (0.5 * M.Sample_Period);
      q2 := pa + (q1 * gx + pb * gz - pc * gy) * (0.5 * M.Sample_Period);
      q3 := pb + (q1 * gy - pa * gz + pc * gx) * (0.5 * M.Sample_Period);
      q4 := pc + (q1 * gz + pa * gy - pb * gx) * (0.5 * M.Sample_Period);

      M.Output := Normalize ((q1, q2, q3, q4));
   end Update;
end AHRS;
