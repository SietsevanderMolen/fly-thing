--  This package is free software; you can redistribute it and/or
--  modify it under terms of the GNU General Public License as
--  published by the Free Software Foundation; either version 3, or
--  (at your option) any later version.  It is distributed in the
--  hope that it will be useful, but WITHOUT ANY WARRANTY; without
--  even the implied warranty of MERCHANTABILITY or FITNESS FOR A
--  PARTICULAR PURPOSE.
--
--  As a special exception under Section 7 of GPL version 3, you are
--  granted additional permissions described in the GCC Runtime
--  Library Exception, version 3.1, as published by the Free Software
--  Foundation.
--
--  You should have received a copy of the GNU General Public License
--  and a copy of the GCC Runtime Library Exception along with this
--  program; see the files COPYING3 and COPYING.RUNTIME respectively.
--  If not, see <http://www.gnu.org/licenses/>.
--
--  Copyright Simon Wright <simon@pushface.org>

pragma License (Modified_GPL);

--  This package interfaces to the MCP23017 I/O multiplexer on the
--  Raspberry Pi's I2C bus, providing polling support for pin change
--  detection.

with Ada.Exceptions;
with Ada.Text_IO;
with Ada.Unchecked_Deallocation;

package body I2C.MCP23017.Polling is

   procedure Set_Polling_Interval (For_Chip : in out Chip; To : Duration)
   is
   begin
      For_Chip.Polling_Interval := To;
   end Set_Polling_Interval;

   task body Polling_Task is
      A_Ports : Byte;
      B_Ports : Byte;
   begin
      A_Ports := For_Chip.Get (GPIO);
      B_Ports := For_Chip.Get (GPIO + 1);
      loop
         select
            accept Stop;
            exit;
         or
            delay For_Chip.Polling_Interval;
            if For_Chip.Callback /= null then
               declare
                  --  Determine which ports are inputs.
                  --
                  --  XXX Done here because this is configurable
                  --  post-creation. Of course, it only actually changes
                  --  when the configuration is done.
                  A_Inputs : constant Byte := For_Chip.Get (IODIR);
                  B_Inputs : constant Byte := For_Chip.Get (IODIR + 1);
                  New_A_Ports : constant Byte := For_Chip.Get (GPIO);
                  New_B_Ports : constant Byte := For_Chip.Get (GPIO + 1);
                  Changed_A_Input_Ports : constant Byte
                    := (A_Ports xor New_A_Ports) and A_Inputs;
                  Changed_B_Input_Ports : constant Byte
                    := (B_Ports xor New_B_Ports) and B_Inputs;
               begin
                  if Changed_A_Input_Ports /= 0 then
                     for J in GPIO_Pin range A0 .. A7 loop
                        declare
                           Mask : constant Byte
                             := Shift_Left (1, GPIO_Pin'Pos (J));
                        begin
                           if (Changed_A_Input_Ports and Mask) /= 0 then
                              For_Chip.Callback
                                (J, (New_A_Ports and Mask) /= 0);
                           end if;
                        end;
                     end loop;
                  end if;
                  if Changed_B_Input_Ports /= 0 then
                     for J in GPIO_Pin range B0 .. B7 loop
                        declare
                           Mask : constant Byte :=
                             Shift_Left (1,
                                         GPIO_Pin'Pos (J) - GPIO_Pin'Pos (B0));
                        begin
                           if (Changed_B_Input_Ports and Mask) /= 0 then
                              For_Chip.Callback
                                (J, (New_B_Ports and Mask) /= 0);
                           end if;
                        end;
                     end loop;
                  end if;
                  A_Ports := New_A_Ports;
                  B_Ports := New_B_Ports;
               end;
            end if;
         end select;
      end loop;
   exception
      when E : others =>
         Ada.Text_IO.Put_Line ("I2C.MCP23017.Polling.Polling_Task exception: "
                                 & Ada.Exceptions.Exception_Information (E));
   end Polling_Task;

   overriding
   procedure Initialize (C : in out Chip)
   is
   begin
      MCP23017.Chip (C).Initialize;
      C.Poller := new Polling_Task (For_Chip => C'Unchecked_Access);
   end Initialize;

   overriding
   procedure Finalize (C : in out Chip)
   is
      procedure Free is new Ada.Unchecked_Deallocation (Polling_Task,
                                                        Polling_Task_P);
   begin
      if C.Poller /= null then
         if not C.Poller'Terminated then
            C.Poller.Stop;
            while not C.Poller'Terminated loop
               delay 0.1;
            end loop;
         end if;
         Free (C.Poller);
      end if;
      MCP23017.Chip (C).Finalize;
   end Finalize;

end I2C.MCP23017.Polling;
