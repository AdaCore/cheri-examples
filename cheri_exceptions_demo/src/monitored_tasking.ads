--
--  Copyright (C) 2023, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--
with Ada.Exceptions;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with System;
with Ada.Real_Time;

package Monitored_Tasking is

   type Task_Status_Kind is
     (PBIT, Normal, Degraded, Compromised, Resetting);

   type BIT_Pass_Fail_Type is (Pass, Fail, Not_Performed, In_Progress);

   type BIT_Result_Type is record
      Result          : BIT_Pass_Fail_Type;
      Failure_Message : Unbounded_String;
      Time_Stamp      : Ada.Real_Time.Time;
   end record;

   Default_BIT_State : constant BIT_Result_Type :=
     BIT_Result_Type'(Result          => Not_Performed,
                      Failure_Message => To_Unbounded_String (""),
                      Time_Stamp      => Ada.Real_Time.Clock);

   Pass_BIT_State : constant BIT_Result_Type :=
     BIT_Result_Type'(Result          => Pass,
                      Failure_Message => To_Unbounded_String (""),
                      Time_Stamp      => Ada.Real_Time.Clock);

   In_Progress_BIT_State : constant BIT_Result_Type :=
     BIT_Result_Type'(Result          => In_Progress,
                      Failure_Message => To_Unbounded_String (""),
                      Time_Stamp      => Ada.Real_Time.Clock);

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

      procedure Set_CBIT (CBIT : BIT_Result_Type);
      --  Write the results of the last continuous built in test

      function Get_CBIT return BIT_Result_Type;
      --  Get the results of the last continuous built in test

      procedure Set_PBIT (PBIT : BIT_Result_Type);
      --  Write the results of the last powerup built in test

      function Get_PBIT return BIT_Result_Type;
      --  Get the results of the powerup built in test

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
      PBIT_Status             : BIT_Result_Type  := Default_BIT_State;
      CBIT_Status             : BIT_Result_Type  := Default_BIT_State;

   end Task_Control;

   type Task_Control_Access is not null access all Task_Control;

   type Parameterless_Procedure is not null access procedure;

   task type Monitored_Task
     (Task_Body : Parameterless_Procedure;
      PBIT_Func : Parameterless_Procedure;
      CBIT_Func : Parameterless_Procedure;
      Control   : Task_Control_Access;
      Priority  : System.Priority)
   with
     Priority => Priority;

end Monitored_Tasking;
