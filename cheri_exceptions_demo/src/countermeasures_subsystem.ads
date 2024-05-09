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

with System;
with Monitored_Tasking;

--  This subsystem implements a task to simulate the concept of "cyber
--  resilience" countermeasures. For the purposes of this demo this involves
--  resetting any task that is in a "compromised" state to return that task
--  to normal operation.

package Countermeasures_Subsystem is

   type Status_Kind is (Ready, Deployed);

   protected Countermeasures_Control is

      procedure Get_Status_Change (Change : out Boolean);

      function Get_Status return Status_Kind;

      procedure Deploy;

      entry Wait_Deploy;

      procedure Make_Ready;

      procedure Reset;

   private

      Deploy_Requested : Boolean     := False;
      Status           : Status_Kind := Ready;
      Status_Change    : Boolean     := True;

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
