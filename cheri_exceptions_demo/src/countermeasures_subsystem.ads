--
--  Copyright (C) 2023, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--
with System;
with Monitored_Tasking;

--  This subsystem implements a task to simulate the concept of "cyber
--  resilience" countermeasures. For the purposes of this demo this involves
--  resetting any task that is in a "compromised" state to return that task
--  to normal operation.

package Countermeasures_Subsystem is

   type Status_Kind is (Ready, Deployed);

   protected Countermeasures_Control is

      function Get_Status return Status_Kind;

      procedure Deploy;

      entry Wait_Deploy;

      procedure Make_Ready;

   private

      Deploy_Requested : Boolean     := False;
      Status           : Status_Kind := Ready;

   end Countermeasures_Control;

   Countermeasures_Task_Control : aliased Monitored_Tasking.Task_Control;

private

   procedure Simulate_Countermeasures;

   procedure PBIT;

   procedure CBIT;

   Countermeasures_Task : Monitored_Tasking.Monitored_Task
     (Task_Body => Simulate_Countermeasures'Access,
      PBIT_Func      => PBIT'Access,
      CBIT_Func      => CBIT'Access,
      Control   => Countermeasures_Task_Control'Access,
      Priority  => System.Priority'Last);

end Countermeasures_Subsystem;
