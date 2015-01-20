pragma Profile (Ravenscar);
with PCA9685;
with I2C;

procedure Ravn is
   I2C_Bus : aliased I2C.Bus (Adapter_Number => 1); --  /dev/i2c-1
   PWM_Driver : PCA9685.Chip (On_Bus => I2C_Bus'Access,
                              Address => 16#40#);  --  default address
   Motor_NE_Pin : Integer := 0;
   Motor_SE_Pin : Integer := 1;
   Motor_SW_Pin : Integer := 2;
   Motor_NW_Pin : Integer := 3;
begin
   PWM_Driver.Reset;
   PWM_Driver.SetPWMFreq (400.0); --  Max frequency as per datasheet
end Ravn;
