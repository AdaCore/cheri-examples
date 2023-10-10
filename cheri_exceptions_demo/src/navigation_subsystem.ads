--
--  Copyright (C) 2023, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--
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

      function Get_Position return Coordinate;

      procedure Set_Position (Position : Coordinate);

      function Get_Heading return Bearing;

      procedure Set_Heading (Heading : Bearing);

   private

      Current_Position : Coordinate := (0.0, 0.0);
      Current_Heading  : Bearing    := 0;

   end Navigation_Data;

   Navigation_Task_Control : aliased Monitored_Tasking.Task_Control;

private

   procedure Simulate_Navigation_Data;

   Navigation_Task : Monitored_Tasking.Monitored_Task
     (Task_Body => Simulate_Navigation_Data'Access,
      Control   => Navigation_Task_Control'Access,
      Priority  => System.Priority'Last);

end Navigation_Subsystem;
