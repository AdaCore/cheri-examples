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

with Ada.Numerics;                      use Ada.Numerics;
with Ada.Numerics.Elementary_Functions; use Ada.Numerics.Elementary_Functions;
with Ada.Real_Time;                     use Ada.Real_Time;

with Flight_Subsystem; use Flight_Subsystem;

package body Navigation_Subsystem is

   Simulated_Heading  : constant Bearing    := 230;
   Simulated_Position : constant Coordinate := (51.482_578, -0.007_659);

   Earth_Radius : constant := 6_371_000.0;
   --  Mean radius of the earth in meters

   ----------------
   -- To_Radians --
   ----------------

   function To_Radians (Degrees : Float) return Float is
     (Degrees * (Pi / 180.0));
   --  Convert from degrees to radians

   ----------------
   -- To_Degrees --
   ----------------

   function To_Degrees (Radians : Float) return Float is
     (Radians * (180.0 / Pi));
   --  Convert from radians to degrees

   --------------------------
   -- To_Meters_Per_Second --
   --------------------------

   function To_Meters_Per_Second (Speed : Knots) return Float is
     (Float (Speed) * 0.514_444);
   --  Convert from knots to meters per second

   ---------------------
   -- Update_Position --
   ---------------------

   function Update_Position
     (Old_Pos : Coordinate;
      Speed   : Knots;
      Heading : Bearing)
      return Coordinate;
   --  Calculate a new latitude/longitude based on a heading and speed,
   --  assuming a time delta of 1 second.

   Update_Period : constant Time_Span := Seconds (1);

   ---------------------
   -- Navigation_Data --
   ---------------------

   protected body Navigation_Data is

      --------------------------
      -- Get_Position_Changed --
      --------------------------

      procedure Get_Position_Changed (Changed : out Boolean) is
      begin
         if Position_Changed then
            Changed := True;
            Position_Changed := False;
         else
            Changed := False;
         end if;
      end Get_Position_Changed;

      ------------------
      -- Get_Position --
      ------------------

      function Get_Position return Coordinate is (Current_Position);

      ------------------
      -- Set_Position --
      ------------------

      procedure Set_Position (Position : Coordinate) is
      begin
         Position_Changed := Current_Position /= Position;
         Current_Position := Position;
      end Set_Position;

      -------------------------
      -- Get_Heading_Changed --
      -------------------------

      procedure Get_Heading_Changed (Changed : out Boolean) is
      begin
         if Heading_Changed then
            Changed := True;
            Heading_Changed := False;
         else
            Changed := False;
         end if;
      end Get_Heading_Changed;

      -----------------
      -- Get_Heading --
      -----------------

      function Get_Heading return Bearing is (Current_Heading);

      -----------------
      -- Set_Heading --
      -----------------

      procedure Set_Heading (Heading : Bearing) is
      begin
         Heading_Changed := Current_Heading /= Heading;
         Current_Heading := Heading;
      end Set_Heading;

      -----------
      -- Reset --
      -----------

      procedure Reset is
      begin
         Set_Position (Simulated_Position);
         Set_Heading (Simulated_Heading);
      end Reset;

   end Navigation_Data;

   ------------------------------
   -- Simulate_Navigation_Data --
   ------------------------------

   procedure Simulate_Navigation_Data is
      New_Position : Coordinate;

      Next_Time : Time := Clock;

   begin
      Navigation_Data.Reset;

      loop
         --  Update the position based on the current speed and heading.
         --
         --  Airspeed is good enough for the purposes of this demo (we don't
         --  simulate ground speed).

         New_Position := Update_Position
                           (Old_Pos => Navigation_Data.Get_Position,
                            Speed   => Flight_Data.Get_Airspeed,
                            Heading => Navigation_Data.Get_Heading);

         Navigation_Data.Set_Position (New_Position);

         Next_Time := Next_Time + Update_Period;
         delay until Next_Time;
      end loop;
   end Simulate_Navigation_Data;

   ---------------------
   -- Update_Position --
   ---------------------

   function Update_Position
     (Old_Pos : Coordinate;
      Speed   : Knots;
      Heading : Bearing)
      return Coordinate
   is
      New_Pos : Coordinate;

      Heading_Rads : Float;

      Speed_ms : Float;
      D        : Float;

      Lat_Rads  : Float;
      Long_Rads : Float;
   begin

      Lat_Rads  := To_Radians (Float (Old_Pos.Lat));
      Long_Rads := To_Radians (Float (Old_Pos.Long));

      Heading_Rads := To_Radians (Float (Heading));
      Speed_ms     := To_Meters_Per_Second (Speed);
      D            := Speed_ms / Earth_Radius;

      New_Pos.Lat := Latitude (To_Degrees (Arcsin (Sin (Lat_Rads) * Cos (D)
        + Cos (Lat_Rads) * Sin (D) * Cos (Heading_Rads))));

      New_Pos.Long := Longitude
        (To_Degrees (Long_Rads
         + Arctan
           (Sin (Heading_Rads) * Sin (D) * Cos (Lat_Rads),
            Cos (D) - Sin (Lat_Rads) * Sin (To_Radians (Float (New_Pos.Lat))))
        ));

      return New_Pos;
   end Update_Position;

   ----------
   -- PBIT --
   ----------

   procedure PBIT is
      use Monitored_Tasking;
   begin

      Navigation_Task_Control.Set_PBIT (In_Progress_BIT_State);

      delay 5.0;

      Navigation_Task_Control.Set_PBIT (Pass_BIT_State);
   end PBIT;

   ----------
   -- CBIT --
   ----------

   procedure CBIT is
      use Monitored_Tasking;
   begin
      Navigation_Task_Control.Set_CBIT (Pass_BIT_State);
   end CBIT;

end Navigation_Subsystem;
