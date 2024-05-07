------------------------------------------------------------------------------
--                           GNAT Pro Morello                               --
--                                                                          --
--                     Copyright (C) 2024, AdaCore                          --
--                                                                          --
-- This is free software;  you can redistribute it  and/or modify it  under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  This software is distributed in the hope  that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for  more details.  You should have  received  a copy of the GNU --
-- General  Public  License  distributed  with  this  software;   see  file --
-- COPYING3.  If not, go to http://www.gnu.org/licenses for a complete copy --
-- of the license.                                                          --
------------------------------------------------------------------------------

package body Monitored_Tasking is

   ------------------
   -- Task_Control --
   ------------------

   protected body Task_Control is

      ------------------------
      -- Exception_Occurred --
      ------------------------

      function Exception_Occurred return Boolean is (Exception_Occurred_Flag);

      ---------------------
      -- Write_Exception --
      ---------------------

      procedure Write_Exception (E : Ada.Exceptions.Exception_Occurrence) is
      begin
         Ada.Exceptions.Save_Occurrence (Stored_Exception, E);
         Exception_Occurred_Flag := True;
      end Write_Exception;

      --------------------
      -- Last_Exception --
      --------------------

      procedure Last_Exception (E : out Ada.Exceptions.Exception_Occurrence) is
      begin
         if Exception_Occurred_Flag then
            Ada.Exceptions.Save_Occurrence (E, Stored_Exception);
         else
            Ada.Exceptions.Save_Occurrence (E, Ada.Exceptions.Null_Occurrence);
         end if;
      end Last_Exception;

      ----------------
      -- Set_Status --
      ----------------

      procedure Set_Status (Status : Task_Status_Kind) is
      begin
         if not Status_Change then
            Status_Change  := Current_Status /= Status;
         end if;
         Current_Status := Status;
      end Set_Status;

      ----------------
      -- Get_Status --
      ----------------

      function Get_Status return Task_Status_Kind is (Current_Status);

      ------------
      -- Status --
      ------------

      procedure Status (Status : out Task_Status_Kind; Changed : out Boolean)
      is
      begin
         Status := Current_Status;
         if Status_Change then
            Status_Change := False;
            Changed := True;
         else
            Changed := False;
         end if;
      end Status;

      -----------
      -- Reset --
      -----------

      procedure Reset is
      begin
         Reset_Requested_Flag := True;
         Current_Status       := Resetting;
      end Reset;

      --------------
      -- Set_CBIT --
      --------------

      procedure Set_CBIT (CBIT : BIT_Result_Type) is
      begin
         CBIT_Status := CBIT;
      end Set_CBIT;

      --------------
      -- Get_CBIT --
      --------------

      function Get_CBIT return BIT_Result_Type is (CBIT_Status);

      --------------
      -- Set_PBIT --
      --------------

      procedure Set_PBIT (PBIT : BIT_Result_Type) is
      begin
         PBIT_Status := PBIT;
      end Set_PBIT;

      --------------
      -- Get_PBIT --
      --------------

      function Get_PBIT return BIT_Result_Type is (PBIT_Status);

      ----------------
      -- Wait_Reset --
      ----------------

      entry Wait_Reset when Reset_Requested_Flag is
      begin
         Reset_Requested_Flag := False;

         Exception_Occurred_Flag := False;
         Current_Status          := Normal;
      end Wait_Reset;

   end Task_Control;

   --------------------
   -- Monitored_Task --
   --------------------

   task body Monitored_Task is
   begin

      Control.all.Set_Status (PBIT);

      PBIT_Func.all;

      if Control.Get_PBIT.Result = Pass then

         loop

            Control.all.Set_Status (Normal);

            CBIT_Func.all;

            begin
               Task_Body.all;

            exception
               when E : others =>

                  declare
                     Failure_Result : constant BIT_Result_Type :=
                       (Result => Fail,
                        Failure_Message => To_Unbounded_String
                          (Ada.Exceptions.Exception_Name (E)),
                        Time_Stamp => Ada.Real_Time.Clock);
                  begin
                     Control.all.Set_CBIT (Failure_Result);
                  end;

                  Control.all.Write_Exception (E);
                  Control.all.Set_Status (Compromised);
                  Control.all.Wait_Reset;

                  Control.all.Set_Status (Resetting);
                  delay 2.0; --  Simulated processing time for resetting
            end;
         end loop;
      end if;
   end Monitored_Task;

end Monitored_Tasking;
