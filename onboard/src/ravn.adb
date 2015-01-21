with Interfaces; use Interfaces;
with Ada.Text_IO;
with Ada.Real_Time; use Ada.Real_Time;
with Ada.Float_Text_IO; use Ada.Float_Text_IO;

with PCA9685;
with I2C;
with i2c_interface_c;
with Interfaces.C;

procedure Ravn is
   I2C_Bus : aliased I2C.Bus (Adapter_Number => 1); --  /dev/i2c-1
   PWM_Driver : PCA9685.Chip (On_Bus => I2C_Bus'Access,
                              Address => 16#40#);  --  default address
   bytes : i2c_interface_c.Byte_Array (1 .. 16);
begin
   PWM_Driver.Reset;
   PWM_Driver.SetPWMFreq (1000.0); --  Max frequency as per datasheet
   PWM_Driver.SetPin (61, 0); --  Initialize with all off

   --  Values to write
   bytes (1) := Interfaces.C.unsigned_char (0);
   bytes (2) := Interfaces.C.unsigned_char (0);
   bytes (3) := Interfaces.C.unsigned_char (0);
   bytes (4) := Interfaces.C.unsigned_char (0);

   bytes (5) := Interfaces.C.unsigned_char (0);
   bytes (6) := Interfaces.C.unsigned_char (0);
   bytes (7) := Interfaces.C.unsigned_char (0);
   bytes (8) := Interfaces.C.unsigned_char (0);

   bytes (9) := Interfaces.C.unsigned_char (0);
   bytes (10) := Interfaces.C.unsigned_char (0);
   bytes (11) := Interfaces.C.unsigned_char (0);
   bytes (12) := Interfaces.C.unsigned_char (0);

   bytes (13) := Interfaces.C.unsigned_char (0);
   bytes (14) := Interfaces.C.unsigned_char (0);
   bytes (15) := Interfaces.C.unsigned_char (0);
   bytes (16) := Interfaces.C.unsigned_char (0);

   Ada.Text_IO.Put_Line ("Setting 4 I2C outputs to 0 afap, ops per second:");
   loop
      declare
         Finish_Time : Time;
         Start_Time : constant Time := Clock;
      begin

         for j in Integer range 0 .. 1000 loop
            I2C.Write_Array_Data (C => PWM_Driver,
                                  R => I2C.Register (50), --  Pin 11 .. 15
                                  Values => bytes);
         end loop;

         Finish_Time := Clock;
         Ada.Float_Text_IO.Put (
            1000.0 / Float (To_Duration (Finish_Time - Start_Time)),
            Fore => 4, Aft => 2, Exp => 0
         );
         Ada.Text_IO.Put (",");
      end;
   end loop;
end Ravn;
