with Generic_Vector_Math;

package Vector_Math is
   package Float_Math is new Generic_Vector_Math (Float);

   function Safe_Tan (x : Float) return Float;
   infinity : constant Float := Float'Last;

   type Float2 is new Float_Math.Vector2;
   type Float3 is new Float_Math.Vector3;
   type Float4 is new Float_Math.Vector4;
   type Float4x4 is new Float_Math.Matrix4;

   function Sqr (x : Float) return Float renames Float_Math.Sqr;
   function Min (a, b : Float) return Float renames Float_Math.Min;
   function Max (a, b : Float) return Float renames Float_Math.Max;
   function Min (a, b, c : Float) return Float renames Float_Math.Min;
   function Max (a, b, c : Float) return Float renames Float_Math.Max;
   function Clamp (x, a, b : Float) return Float renames Float_Math.Clamp;

   function Length (a : Float3) return Float;
   function Normalize (a : Float3) return Float3;
   function Reflect (dir : Float3; normal : Float3) return Float3;

   function Min (a, b : Float_Math.Vector3)
      return Float_Math.Vector3 renames Float_Math.Min;
   function Max (a, b : Float_Math.Vector3)
      return Float_Math.Vector3 renames Float_Math.Max;
   function Clamp (x  : Float_Math.Vector3; a, b : Float)
      return Float_Math.Vector3 renames Float_Math.Clamp;
   function Clamp (x, a, b : Float_Math.Vector3)
      return Float_Math.Vector3 renames Float_Math.Clamp;

   function RotationMatrix (angle : Float; a_v : Float3) return Float4x4;
   function LookAtMatrix (eye, center, up : Float3) return Float4x4;

   --  function "*" (m1 : Float4x4; m2 : Float4x4)
   --     return Float4x4 renames Float_Math."*";

   function "*" (m : Float4x4; v : Float3) return Float3;
   function TransformNormal (m : Float4x4; n : Float3) return Float3;


   IdentityMatrix : constant Float4x4 := ((1.0, 0.0, 0.0, 0.0),
                                          (0.0, 1.0, 0.0, 0.0),
                                          (0.0, 0.0, 1.0, 0.0),
                                          (0.0, 0.0, 0.0, 1.0));


   package Integer_Math is new Generic_Vector_Math (Integer);

   type Int3 is new Integer_Math.Vector3;
   type Int4 is new Integer_Math.Vector4;

   --  function Sqr (x : Integer) return Float renames Integer_Math.Sqr;
   function Min (a, b : Integer) return Integer renames Integer_Math.Min;
   function Max (a, b : Integer) return Integer renames Integer_Math.Max;
   function Min (a, b, c : Integer) return Integer renames Integer_Math.Min;
   function Max (a, b, c : Integer) return Integer renames Integer_Math.Max;
   function Clamp (x, a, b : Integer)
      return Integer renames Integer_Math.Clamp;

   pragma Inline (Normalize);
   pragma Inline (Length);
   pragma Inline (Reflect);
end Vector_Math;
