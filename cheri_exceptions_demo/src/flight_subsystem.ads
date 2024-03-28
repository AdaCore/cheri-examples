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

--  This subsystem implements a task to simulate some basic flight
--  instrumentation data.

package Flight_Subsystem is

   type Feet is new Natural;

   type Knots is new Natural;

   protected Flight_Data is

      procedure Get_Altitude_Changed (Changed : out Boolean);

      function Get_Altitude return Feet;

      procedure Set_Altitude (Altitude : Feet);

      procedure Get_Airspeed_Changed (Changed : out Boolean);

      function Get_Airspeed return Knots;

      procedure Set_Airspeed (Airspeed : Knots);

   private

      Current_Altitude : Feet  := 0;
      Current_Airspeed : Knots := 0;
      Altitude_Changed : Boolean  := True;
      Airspeed_Changed : Boolean  := True;

   end Flight_Data;

   Flight_Task_Control : aliased Monitored_Tasking.Task_Control;

private

   procedure Simulate_Flight_Data;

   procedure PBIT;

   procedure CBIT;

   Flight_Task : Monitored_Tasking.Monitored_Task
     (Task_Body => Simulate_Flight_Data'Access,
      PBIT_Func      => PBIT'Access,
      CBIT_Func      => CBIT'Access,
      Control   => Flight_Task_Control'Access,
      Priority  => System.Priority'Last);

end Flight_Subsystem;
