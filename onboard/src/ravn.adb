with Interfaces; use Interfaces;
with Ada.Text_IO;
with Ada.Calendar; use Ada.Calendar;

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

   loop
      Ada.Text_IO.Put_Line ("Running update loop 1000 times afap");
      declare
         Start_Time : constant Time := Clock;
         Finish_Time : Time;
      begin

         for j in Integer range 0 .. 1000 loop
            I2C.Write_Array_Data (C => PWM_Driver,
            R => I2C.Register (Unsigned_8 (50)), --  P 11
            Values => bytes);
         end loop;

         Finish_Time := Clock;
         Ada.Text_IO.Put_Line ("Ops: " &
            Integer'Image (1000 / Integer (Finish_Time - Start_Time)));
      end;
   end loop;
end Ravn;
