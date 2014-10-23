pragma Profile (Ravenscar);
with PCA9685;
with I2C;

procedure Ravn is
   B : aliased I2C.Bus (Address => 42);
   C : PCA9685.Chip (Address => 16#20#, On_Bus => B'Access);
begin
   C.Reset;
   null;
end Ravn;
