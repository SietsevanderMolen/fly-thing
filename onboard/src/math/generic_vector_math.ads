generic
   type T is private;

   with function "+" (L, R : T) return T is <>;
   with function "-" (L, R : T) return T is <>;
   with function "*" (L, R : T) return T is <>;

   with function ">" (L, R : T) return Boolean is <>;
   with function "<" (L, R : T) return Boolean is <>;
   with function ">=" (L, R : T) return Boolean is <>;
   with function "<=" (L, R : T) return Boolean is <>;
package Generic_Vector_Math is
   function Min (a, b : T) return T;
   function Max (a, b : T) return T;

   function Min (a, b, c : T) return T;
   function Max (a, b, c : T) return T;
   function Sqr (x : T) return T;

   type Vector2 is record
      x, y : T;
   end record;

   type Vector3 is record
      x, y, z : T;
   end record;

   type Vector4 is record
      x, y, z, w : T;
   end record;

   type Matrix4 is array (0 .. 3, 0 .. 3) of T;

   function "+" (a, b : Vector4) return Vector4;

   function "+" (a, b : Vector3) return Vector3;
   function "-" (a, b : Vector3) return Vector3;
   function Dot (a, b : Vector3) return T;
   function Cross (a, b : Vector3) return Vector3;

   function Min (a, b : Vector3) return Vector3;
   function Max (a, b : Vector3) return Vector3;
   function Clamp (x : Vector3; a, b : T) return Vector3;
   function Clamp (x, a, b : Vector3) return Vector3;

   function Clamp (x, a, b : T) return T;

   function "*" (a : Vector3; b : Vector3) return Vector3;

   function "*" (a : Vector3; k : T) return Vector3;
   function "*" (k : T; a : Vector3) return Vector3;

   function "*" (v : Vector3; m : Matrix4) return Vector3;
   function "*" (v : Vector4; m : Matrix4) return Vector4;

   function "*" (m : Matrix4; v : Vector3) return Vector3;
   function "*" (m : Matrix4; v : Vector4) return Vector4;

   function "*" (m1 : Matrix4; m2 : Matrix4) return Matrix4;

   function GetRow (m : Matrix4; i : Integer) return Vector4;
   function GetCol (m : Matrix4; i : Integer) return Vector4;

   procedure SetRow (m : in out Matrix4; i : in Integer; v : in Vector4);
   procedure SetCol (m : in out Matrix4; i : in Integer; v : in Vector4);

   pragma Inline ("+");
   pragma Inline ("*");
   pragma Inline ("-");
   pragma Inline (Dot);
   pragma Inline (Min);
   pragma Inline (Max);
   pragma Inline (Sqr);
   pragma Inline (Clamp);
end Generic_Vector_Math;
