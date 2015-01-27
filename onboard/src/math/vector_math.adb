with Ada.Numerics;
with Ada.Numerics.Generic_Elementary_Functions;

use Ada.Numerics;

package body Vector_Math is
   package Float_Functions is new Generic_Elementary_Functions (Float);
   use Float_Functions;

   function Safe_Tan (x : Float) return Float is
      Half_Pi : constant Float :=  Ada.Numerics.Pi * 0.5;
   begin
      if abs (x) = Half_Pi then
         return Float'Last;
      else
         return Tan (x);
      end if;
   end Safe_Tan;

   function Normalize (a : Float3) return Float3 is
      l_inv : Float;
   begin
      l_inv := 1.0 / Sqrt (Dot (a, a));
      return (l_inv * a.x, l_inv * a.y, l_inv * a.z);
   end Normalize;

   function Length (a : Float3) return Float is
   begin
      return Sqrt (Dot (a, a));
   end Length;

   function Reflect (dir : Float3; normal : Float3) return Float3 is
   begin
      return Normalize ((normal * Dot (dir, normal) * (-2.0)) + dir);
   end Reflect;

   function RotationMatrix (angle : Float; a_v : Float3) return Float4x4 is
      M : Float4x4;
      v : Float3;
      cos_t, sin_t : Float;
   begin

      M := IdentityMatrix;
      v := Normalize (a_v);

      cos_t := Cos (angle);
      sin_t := Sin (angle);

      M (0, 0) := (1.0-cos_t) * v.x * v.x + cos_t;
      M (0, 1) := (1.0-cos_t) * v.x * v.y - sin_t * v.z;
      M (0, 2) := (1.0-cos_t) * v.x * v.z + sin_t * v.y;

      M (1, 0) := (1.0-cos_t) * v.y * v.x + sin_t * v.z;
      M (1, 1) := (1.0-cos_t) * v.y * v.y + cos_t;
      M (1, 2) := (1.0-cos_t) * v.y * v.z - sin_t * v.x;

      M (2, 0) := (1.0-cos_t) * v.x * v.z - sin_t * v.y;
      M (2, 1) := (1.0-cos_t) * v.z * v.y + sin_t * v.x;
      M (2, 2) := (1.0-cos_t) * v.z * v.z + cos_t;

      return M;
   end RotationMatrix;

   function LookAtMatrix (eye, center, up : Float3) return Float4x4 is
      M : Float4x4;
      f, s, u : Float3;
   begin
      M := IdentityMatrix;

      f := Normalize (center - eye);
      s := Cross (f, up);
      u := Cross (s, f);

      M (0, 0) :=  f.x; M (0, 1) :=  f.y; M (0, 2) := f.z;
      M (1, 0) :=  u.x; M (1, 1) :=  u.y; M (1, 2) := u.z;
      M (2, 0) := -f.x; M (2, 1) := -f.y; M (2, 2) := -f.z;

      M (0, 3) := -eye.x;
      M (1, 3) := -eye.y;
      M (2, 3) := -eye.z;

      return M;
   end LookAtMatrix;

   function "*" (m : Float4x4; v : Float3) return Float3 is
      res : Float3;
   begin
      res.x := m (0, 0) * v.x + m (0, 1) * v.y + m (0, 2) * v.z + m (0, 3);
      res.y := m (1, 0) * v.x + m (1, 1) * v.y + m (1, 2) * v.z + m (1, 3);
      res.z := m (2, 0) * v.x + m (2, 1) * v.y + m (2, 2) * v.z + m (2, 3);
      return res;
   end "*";

   function TransformNormal (m : Float4x4; n : Float3) return Float3 is
      res : Float3;
   begin
      res.x := m (0, 0) * n.x + m (0, 1) * n.y + m (0, 2) * n.z;
      res.y := m (1, 0) * n.x + m (1, 1) * n.y + m (1, 2) * n.z;
      res.z := m (2, 0) * n.x + m (2, 1) * n.y + m (2, 2) * n.z;
      return res;
   end TransformNormal;
end Vector_Math;

