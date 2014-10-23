--  This package is free software; you can redistribute it and/or
--  modify it under terms of the GNU General Public License as
--  published by the Free Software Foundation; either version 3, or
--  (at your option) any later version.  It is distributed in the hope
--  that it will be useful, but WITHOUT ANY WARRANTY; without even the
--  implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
--  PURPOSE.
--
--   Copyright Simon Wright <simon@pushface.org>

with Ada.Exceptions;
with Ada.Text_IO; use Ada.Text_IO;
with I2C.MCP23017.Polling;

procedure T is

   B : aliased I2C.Bus (Address => 0);
   C : I2C.MCP23017.Polling.Chip (Address => 16#20#, On_Bus => B'Access);

   Interval : constant Duration := 0.25;

   use type I2C.MCP23017.GPIO_Pin;

   procedure Callback (Pin_Changed : I2C.MCP23017.GPIO_Pin; To : Boolean);

   Stopped : Boolean := False;
   pragma Volatile (Stopped);
   In_Reverse : Boolean := False;
   pragma Volatile (In_Reverse);

   procedure Callback (Pin_Changed : I2C.MCP23017.GPIO_Pin; To : Boolean)
   is
   begin
      Put_Line ("Pin " & Pin_Changed'Img & " changed: " & To'Img);
      if Pin_Changed = I2C.MCP23017.B0 then
         Stopped := To;
      end if;
      if Pin_Changed = I2C.MCP23017.B4 then
         In_Reverse := To;
      end if;
   end Callback;

   Current_Pin : I2C.MCP23017.GPIO_Pin;

begin

   C.Reset;

   C.Configure (Connect_Interrupt_Pins => True,
                Interrupt_Pins_Open_Drain => False,
                Interrupt_Pins_Active_High => True);

   C.Register_Callback (Callback'Unrestricted_Access);

   --  All GPA lines to output
   for J in I2C.MCP23017.A0 .. I2C.MCP23017.A7 loop
      C.Configure (Pin => J, As_Input => False);
   end loop;

   --  All GPB lines to input, with reverse polarity and pull-up
   --  enabled
   for J in I2C.MCP23017.B0 .. I2C.MCP23017.B7 loop
      C.Configure (Pin => J,
                   As_Input => True,
                   Normal_Input_Polarity => False,
                   Pullup_Enabled => True);
   end loop;

   Current_Pin := I2C.MCP23017.A7;
   loop
      exit when Stopped;
      if not In_Reverse then
         loop
            exit when Stopped or In_Reverse;
            if Current_Pin = I2C.MCP23017.A7 then
               Current_Pin := I2C.MCP23017.A0;
            else
               Current_Pin := I2C.MCP23017.GPIO_Pin'Succ (Current_Pin);
            end if;
            for J in I2C.MCP23017.A0 .. I2C.MCP23017.A7 loop
               C.Write (J, Item => J = Current_Pin);
            end loop;
            delay Interval;
         end loop;
      else
         loop
            exit when Stopped or not In_Reverse;
            if Current_Pin = I2C.MCP23017.A0 then
               Current_Pin := I2C.MCP23017.A7;
            else
               Current_Pin := I2C.MCP23017.GPIO_Pin'Pred (Current_Pin);
            end if;
            for J in I2C.MCP23017.A0 .. I2C.MCP23017.A7 loop
               C.Write (J, Item => J = Current_Pin);
            end loop;
            delay Interval;
         end loop;
      end if;
   end loop;

exception
   when E : others =>
      Put_Line (Ada.Exceptions.Exception_Information (E));
end T;
