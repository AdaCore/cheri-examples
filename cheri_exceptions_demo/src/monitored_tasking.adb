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
      loop
         Control.all.Set_Status (Normal);

         begin
            Task_Body.all;
         exception
            when E : others =>
               Control.all.Write_Exception (E);
               Control.all.Set_Status (Compromised);
               Control.all.Wait_Reset;

               Control.all.Set_Status (Resetting);
               delay 2.0; --  Simulated processing time for resetting
         end;
      end loop;
   end Monitored_Task;

end Monitored_Tasking;
