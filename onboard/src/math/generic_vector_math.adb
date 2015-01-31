with Ada.Numerics.Generic_Elementary_Functions;

use Ada.Numerics;

package body Generic_Vector_Math is
   package Float_Functions is new Generic_Elementary_Functions (Float);
   use Float_Functions;

   function Min (a, b : T) return T is
   begin
      if a < b then
         return a;
      else
         return b;
      end if;
   end Min;

   function Max (a, b : T) return T is
   begin
      if a >= b then
         return a;
      else
         return b;
      end if;
   end Max;

   function Min (a, b, c : T) return T is
   begin
      if a < b and a < c then
         return a;
      elsif b < c and b < a then
         return b;
      else
         return c;
      end if;
   end Min;

   function Max (a, b, c : T) return T is
   begin
      if a >= b and a >= c then
         return a;
      elsif b >= c and b >= a then
         return b;
      else
         return c;
      end if;
   end Max;

   function Clamp (x, a, b : T) return T is
   begin
      return Min (Max (x, a), b);
   end Clamp;

   function Sqr (x : T) return T is
   begin
      return x * x;
   end Sqr;

   function "+" (a, b : Vector4) return Vector4 is
      res : Vector4;
   begin
      res.x := a.x + b.x;
      res.y := a.y + b.y;
      res.z := a.z + b.z;
      res.w := a.w + b.w;
      return res;
   end "+";

   function "+" (a, b : Vector3) return Vector3 is
      res : Vector3;
   begin
      res.x := a.x + b.x;
      res.y := a.y + b.y;
      res.z := a.z + b.z;
      return res;
   end "+";

   function "-" (a, b : Vector3) return Vector3 is
      res : Vector3;
   begin
      res.x := a.x - b.x;
      res.y := a.y - b.y;
      res.z := a.z - b.z;
      return res;
   end "-";

   function "*" (a, b : Vector3) return Vector3 is
      res : Vector3;
   begin
      res.x := a.x * b.x;
      res.y := a.y * b.y;
      res.z := a.z * b.z;
      return res;
   end "*";

   function Dot (a, b : Vector3) return T is
   begin
      return a.x * b.x + a.y * b.y + a.z * b.z;
   end Dot;

   function Cross (a, b : Vector3) return Vector3 is
      res : Vector3;
   begin
      res.x := a.y * b.z - b.y * a.z;
      res.y := a.z * b.x - b.z * a.x;
      res.z := a.x * b.y - b.x * a.y;
      return res;
   end Cross;

   function Min (a, b : Vector3) return Vector3 is
      res : Vector3;
   begin
      res.x := Min (a.x, b.x);
      res.y := Min (a.y, b.y);
      res.z := Min (a.z, b.z);
      return res;
   end Min;

   function Max (a, b : Vector3) return Vector3 is
   begin
      return (Max (a.x, b.x),
              Max (a.y, b.y),
              Max (a.z, b.z));
   end Max;

   function Clamp (x : Vector3; a, b : T) return Vector3 is
   begin
      return (Clamp (x.x, a, b),
              Clamp (x.y, a, b),
              Clamp (x.z, a, b));
   end Clamp;

   function Clamp (x, a, b : Vector3) return Vector3 is
   begin
      return (Clamp (x.x, a.x, b.x),
              Clamp (x.y, a.y, b.y),
              Clamp (x.z, a.z, b.z));
   end Clamp;

   function "*" (a : Vector3; k : T) return Vector3 is
   begin
      return (k * a.x, k * a.y, k * a.z);
   end "*";

   function "*" (k : T; a : Vector3) return Vector3 is
   begin
      return (k * a.x, k * a.y, k * a.z);
   end "*";

   function "*" (v : Vector3; m : Matrix4) return Vector3 is
      res : Vector3;
   begin
      res.x := v.x * m (0, 0) + v.y * m (1, 0) + v.z * m (2, 0) + m (3, 0);
      res.y := v.x * m (0, 1) + v.y * m (1, 1) + v.z * m (2, 1) + m (3, 1);
      res.z := v.x * m (0, 2) + v.y * m (1, 2) + v.z * m (2, 2) + m (3, 2);
      return res;
   end "*";

   function "*" (v : Vector4; m : Matrix4) return Vector4 is
      res : Vector4;
   begin
      res.x := v.x * m (0, 0) +
               v.y * m (1, 0) +
               v.z * m (2, 0) +
               v.w * m (3, 0);
      res.y := v.x * m (0, 1) +
               v.y * m (1, 1) +
               v.z * m (2, 1) +
               v.w * m (3, 1);
      res.z := v.x * m (0, 2) +
               v.y * m (1, 2) +
               v.z * m (2, 2) +
               v.w * m (3, 2);
      res.w := v.x * m (0, 3) +
               v.y * m (1, 3) +
               v.z * m (2, 3) +
               v.w * m (3, 3);
      return res;
   end "*";

   function "*" (m : Matrix4; v : Vector3) return Vector3 is
      res : Vector3;
   begin
      res.x := m (0, 0) * v.x + m (0, 1) * v.y + m (0, 2) * v.z + m (0, 3);
      res.y := m (1, 0) * v.x + m (1, 1) * v.y + m (1, 2) * v.z + m (1, 3);
      res.z := m (2, 0) * v.x + m (2, 1) * v.y + m (2, 2) * v.z + m (2, 3);
      return res;
   end "*";

   function "*" (m : Matrix4; v : Vector4) return Vector4 is
      res : Vector4;
   begin
      res.x := m (0, 0) * v.x +
               m (0, 1) * v.y +
               m (0, 2) * v.z +
               m (0, 3) * v.w;
      res.y := m (1, 0) * v.x +
               m (1, 1) * v.y +
               m (1, 2) * v.z +
               m (1, 3) * v.w;
      res.z := m (2, 0) * v.x +
               m (2, 1) * v.y +
               m (2, 2) * v.z +
               m (2, 3) * v.w;
      res.w := m (3, 0) * v.x +
               m (3, 1) * v.y +
               m (3, 2) * v.z +
               m (3, 3) * v.w;
      return res;
   end "*";


   function GetRow (m : Matrix4; i : Integer) return Vector4 is
      res : Vector4;
   begin
      res.x := m (i, 0);
      res.y := m (i, 1);
      res.z := m (i, 2);
      res.w := m (i, 3);
      return res;
   end GetRow;

   function GetCol (m : Matrix4; i : Integer) return Vector4 is
      res : Vector4;
   begin
      res.x := m (0, i);
      res.y := m (1, i);
      res.z := m (2, i);
      res.w := m (3, i);
      return res;
   end GetCol;

   procedure SetRow (m : in out Matrix4; i : in Integer; v : in Vector4) is
   begin
      m (i, 0) := v.x;
      m (i, 1) := v.y;
      m (i, 2) := v.z;
      m (i, 3) := v.w;
   end SetRow;

   procedure SetCol (m : in out Matrix4; i : in Integer; v : in Vector4) is
   begin
      m (0, i) := v.x;
      m (1, i) := v.y;
      m (2, i) := v.z;
      m (3, i) := v.w;
   end SetCol;

   function "*" (m1 : Matrix4; m2 : Matrix4) return Matrix4 is
      m   : Matrix4;
      row : Vector4;
   begin
      for i in 0 .. 3 loop
         row := m1 * GetCol (m2, i);
         SetRow (m, i, row);
      end loop;

      return m;
   end "*";
end Generic_Vector_Math;
