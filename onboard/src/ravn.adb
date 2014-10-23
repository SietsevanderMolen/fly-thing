pragma Profile (Ravenscar);
with I2C.PCA9685;

procedure Ravn is
    B : aliased I2C.Bus (Address=>0);
    C : I2C.PCA9685.Chip (Address=>16#20#, On_Bus=>B'Access);
begin
    C.Reset;
end Ravn;
