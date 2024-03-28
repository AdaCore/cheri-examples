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

      function Get_Status return Task_Status_Kind;
      --  Get the task status

      procedure Status (Status : out Task_Status_Kind; Changed : out Boolean);
      --  Get the task status and if it has changed

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
      Status_Change           : Boolean          := True;
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
