with Ada.Text_IO; use Ada.Text_IO;
--
--  Copyright (C) 2023, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--
package body Monitored_Tasking is

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
         Current_Status := Status;
      end Set_Status;

      ------------
      -- Status --
      ------------

      function Status return Task_Status_Kind is (Current_Status);

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
