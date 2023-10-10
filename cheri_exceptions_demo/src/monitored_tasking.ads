--
--  Copyright (C) 2023, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--
with Ada.Exceptions;
with System;

package Monitored_Tasking is

   type Task_Status_Kind is (Normal, Degraded, Compromised, Resetting);

   protected type Task_Control is

      function Exception_Occurred return Boolean;
      --  Check if any exception has previously occurred

      procedure Write_Exception (E : Ada.Exceptions.Exception_Occurrence);
      --  Store an exception

      procedure Last_Exception (E : out Ada.Exceptions.Exception_Occurrence);
      --  Get the last exception that occurred

      procedure Set_Status (Status : Task_Status_Kind);
      --  Set the task status

      function Status return Task_Status_Kind;
      --  Get the task status

      procedure Reset;
      --  Send a request to the task to reset

      entry Wait_Reset;
      --  Wait for a reset request.
      --
      --  When a reset is requested, the exception data is cleared and the
      --  task status is reset to Normal.

   private

      Stored_Exception        : Ada.Exceptions.Exception_Occurrence;
      Current_Status          : Task_Status_Kind := Normal;
      Exception_Occurred_Flag : Boolean          := False;
      Reset_Requested_Flag    : Boolean          := False;

   end Task_Control;

   type Task_Control_Access is not null access all Task_Control;

   type Parameterless_Procedure is not null access procedure;

   task type Monitored_Task
     (Task_Body : Parameterless_Procedure;
      Control   : Task_Control_Access;
      Priority  : System.Priority)
   with
     Priority => Priority;

end Monitored_Tasking;
