pragma Profile (Ravenscar);
with PCA9685;
with I2C;

procedure Ravn is
   I2C_Bus : aliased I2C.Bus (Adapter_Number => 1);
   PWM_Driver : PCA9685.Chip (On_Bus => I2C_Bus'Access,
                              Address => 16#40#);
begin
   PWM_Driver.Reset;
   PWM_Driver.SetPWMFreq (1000.0); --  Set to max frequency
end Ravn;
