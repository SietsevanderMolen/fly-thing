with Quaternions;

with Vector_Math; use Vector_Math;

package AHRS is
   package Float_Quaternion is new Quaternions (Float);
   use Float_Quaternion;

   type Mahony is
      record
         Sample_Period : Float;
         Proportional_Gain : Float;
         Integral_Gain : Float;
         Integral_Error : Vector_Math.Float3 := (0.0, 0.0, 0.0);
         Output : Quaternion := (1.0, 0.0, 0.0, 0.0);
      end record;

   function Make (Sample_Period : Float;
                  Proportional_Gain : Float;
                  Integral_Gain : Float) return Mahony;

   procedure Update (M : in out Mahony;
                    Accelerometer : Vector_Math.Float3;
                    Gyroscope : Vector_Math.Float3);

   procedure Update (M : in out Mahony;
                    Accelerometer : Vector_Math.Float3;
                    Gyroscope : Vector_Math.Float3;
                    Magnetometer : Vector_Math.Float3);
end AHRS;
