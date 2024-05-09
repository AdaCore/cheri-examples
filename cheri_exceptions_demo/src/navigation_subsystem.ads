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

--  This subsystem implements a task to simulate basic aircraft navigation
--  instrumentation. The current position (latitude/longitude) is updated
--  periodically based on the current heading and airspeed.

package Navigation_Subsystem is

   type Latitude is delta 0.000_001 range -90.0 .. +90.0 with
     Small => 0.000_001;

   type Longitude is delta 0.000_001 range -180.0 .. +180.0 with
     Small => 0.000_001;

   type Bearing is range 0 .. 359;

   type Coordinate is record
      Lat  : Latitude;
      Long : Longitude;
   end record;

   protected Navigation_Data is

      procedure Get_Position_Changed (Changed : out Boolean);

      function Get_Position return Coordinate;

      procedure Set_Position (Position : Coordinate);

      procedure Get_Heading_Changed (Changed : out Boolean);

      function Get_Heading return Bearing;

      procedure Set_Heading (Heading : Bearing);

      procedure Reset;

   private

      Current_Position : Coordinate := (0.0, 0.0);
      Position_Changed : Boolean    := True;
      Current_Heading  : Bearing    := 0;
      Heading_Changed  : Boolean    := True;

   end Navigation_Data;

   Navigation_Task_Control : aliased Monitored_Tasking.Task_Control;

private

   procedure Simulate_Navigation_Data;

   procedure PBIT;

   procedure CBIT;

   Navigation_Task : Monitored_Tasking.Monitored_Task
     (Task_Body => Simulate_Navigation_Data'Access,
      PBIT_Func      => PBIT'Access,
      CBIT_Func      => CBIT'Access,
      Control   => Navigation_Task_Control'Access,
      Priority  => System.Priority'Last);

end Navigation_Subsystem;
