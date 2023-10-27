--
--  Copyright (C) 2023, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--
with Ada.Real_Time; use Ada.Real_Time;

package body Flight_Subsystem is

   Simulated_Airspeed : constant Knots := 500;
   Simulated_Altitude : constant Feet  := 38_000;

   Update_Period : constant Time_Span := Seconds (1);

   protected body Flight_Data is

      function Get_Altitude return Feet is (Current_Altitude);

      procedure Set_Altitude (Altitude : Feet) is
      begin
         Current_Altitude := Altitude;
      end Set_Altitude;

      function Get_Airspeed return Knots is (Current_Airspeed);

      procedure Set_Airspeed (Airspeed : Knots) is
      begin
         Current_Airspeed := Airspeed;
      end Set_Airspeed;

   end Flight_Data;

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
