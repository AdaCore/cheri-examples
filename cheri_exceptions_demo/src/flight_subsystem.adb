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

with Ada.Real_Time; use Ada.Real_Time;

package body Flight_Subsystem is

   Simulated_Airspeed : constant Knots := 500;
   Simulated_Altitude : constant Feet  := 38_000;

   Update_Period : constant Time_Span := Seconds (1);

   protected body Flight_Data is

      --------------------------
      -- Get_Altitude_Changed --
      --------------------------

      procedure Get_Altitude_Changed (Changed : out Boolean) is
      begin
         if Altitude_Changed then
            Changed := True;
            Altitude_Changed := False;
         else
            Changed := False;
         end if;
      end Get_Altitude_Changed;

      ------------------
      -- Get_Altitude --
      ------------------

      function Get_Altitude return Feet is (Current_Altitude);

      ------------------
      -- Set_Altitude --
      ------------------

      procedure Set_Altitude (Altitude : Feet) is
      begin
         Current_Altitude := Altitude;
         Altitude_Changed := True;
      end Set_Altitude;

      ------------------
      -- Get_Airspeed --
      ------------------

      function Get_Airspeed return Knots is (Current_Airspeed);

      --------------------------
      -- Get_Airspeed_Changed --
      --------------------------

      procedure Get_Airspeed_Changed (Changed : out Boolean) is
      begin
         if Airspeed_Changed then
            Changed := True;
            Airspeed_Changed := False;
         else
            Changed := False;
         end if;
      end Get_Airspeed_Changed;

      ------------------
      -- Set_Airspeed --
      ------------------

      procedure Set_Airspeed (Airspeed : Knots) is
      begin
         Current_Airspeed := Airspeed;
         Airspeed_Changed := True;
      end Set_Airspeed;

   end Flight_Data;

   --------------------------
   -- Simulate_Flight_Data --
   --------------------------

   procedure Simulate_Flight_Data is
      Next_Time : Time := Clock;
   begin
      loop
         Flight_Data.Set_Altitude (Simulated_Altitude);
         Flight_Data.Set_Airspeed (Simulated_Airspeed);
         Next_Time := Next_Time + Update_Period;
         delay until Next_Time;
      end loop;
   end Simulate_Flight_Data;

   ----------
   -- PBIT --
   ----------

   procedure PBIT is
      use Monitored_Tasking;
   begin

      Flight_Task_Control.Set_PBIT (In_Progress_BIT_State);

      delay 6.0;

      Flight_Task_Control.Set_PBIT (Pass_BIT_State);
   end PBIT;

   ----------
   -- CBIT --
   ----------

   procedure CBIT is
      use Monitored_Tasking;
   begin
      Flight_Task_Control.Set_CBIT (Pass_BIT_State);
   end CBIT;

end Flight_Subsystem;
