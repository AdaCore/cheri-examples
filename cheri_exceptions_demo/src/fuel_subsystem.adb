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

with Ada.Real_Time;

package body Fuel_Subsystem is

   Simulated_Fuel_Burn_Rate : constant Kilograms_Per_Second := 0.642;
   --  A simulated fuel burn rate for demo purposes

   Simulated_Minimum_Fuel : constant Kilograms := 1_000.0;

   ---------------
   -- Fuel_Data --
   ---------------

   protected body Fuel_Data is

      -----------------------------
      -- Fuel_Quantity_Available --
      -----------------------------

      function Fuel_Quantity_Available return Boolean is (FQI_Valid);

      ---------------------------
      -- Fuel_Quantity_Changed --
      ---------------------------

      procedure Fuel_Quantity_Changed (Changed : out Boolean) is
      begin
         if FQI_Changed then
            Changed := True;
            FQI_Changed := False;
         else
            Changed := False;
         end if;
      end Fuel_Quantity_Changed;

      -------------------
      -- Fuel_Quantity --
      -------------------

      function Fuel_Quantity return Kilograms is (FQI_Value);

      -------------------------------
      -- Time_To_Minimum_Available --
      -------------------------------

      function Time_To_Minimum_Available return Boolean is
        (Time_To_Minimum_Fuel_Valid);

      ----------------------------------
      -- Time_To_Minimum_Fuel_Changed --
      ----------------------------------

      procedure Time_To_Minimum_Fuel_Changed (Changed : out Boolean) is
      begin
         if Time_To_Minimum_Fuel_Has_Changed then
            Changed := True;
            Time_To_Minimum_Fuel_Has_Changed := False;
         else
            Changed := False;
         end if;
      end Time_To_Minimum_Fuel_Changed;

      --------------------------
      -- Time_To_Minimum_Fuel --
      --------------------------

      function Time_To_Minimum_Fuel return Duration is
        (Time_To_Minimum_Fuel_Value);

      ------------------------------
      -- Invalidate_Fuel_Quantity --
      ------------------------------

      procedure Invalidate_Fuel_Quantity is
      begin
         FQI_Valid := False;
      end Invalidate_Fuel_Quantity;

      -------------------------------------
      -- Invalidate_Time_To_Minimum_Fuel --
      -------------------------------------

      procedure Invalidate_Time_To_Minimum_Fuel is
      begin
         Time_To_Minimum_Fuel_Valid := False;
      end Invalidate_Time_To_Minimum_Fuel;

      -----------------------------
      -- Fuel_Quantity_Available --
      -----------------------------

      procedure Set_Fuel_Quantity (Value : Kilograms) is
      begin
         FQI_Changed := FQI_Value /= Value;
         FQI_Value   := Value;
         FQI_Valid   := True;
      end Set_Fuel_Quantity;

      ------------------------------
      -- Set_Time_To_Minimum_Fuel --
      ------------------------------

      procedure Set_Time_To_Minimum_Fuel (Value : Duration) is
      begin
         Time_To_Minimum_Fuel_Has_Changed :=
           Time_To_Minimum_Fuel_Value /= Value;
         Time_To_Minimum_Fuel_Value   := Value;
         Time_To_Minimum_Fuel_Valid   := True;
      end Set_Time_To_Minimum_Fuel;

   end Fuel_Data;

   ------------------------
   -- Simulate_Fuel_Data --
   ------------------------

   procedure Simulate_Fuel_Data is
      use type Ada.Real_Time.Time;

      Period : constant Ada.Real_Time.Time_Span := Ada.Real_Time.Seconds (1);

      Next_Time : Ada.Real_Time.Time;

      FQI : Kilograms;
      TTM : Duration;

   begin
      Next_Time := Ada.Real_Time.Clock;

      loop
         FQI := Fuel_Data.Fuel_Quantity;

         if FQI >= Kilograms (Simulated_Fuel_Burn_Rate) then
            FQI := FQI - Kilograms (Simulated_Fuel_Burn_Rate);
         else
            FQI := 0.0;
         end if;

         Fuel_Data.Set_Fuel_Quantity (FQI);

         if FQI > Simulated_Minimum_Fuel then
            TTM := Duration (FQI - Simulated_Minimum_Fuel) /
              Duration (Simulated_Fuel_Burn_Rate);
         else
            TTM := 0.0;
         end if;

         Fuel_Data.Set_Time_To_Minimum_Fuel (TTM);

         Next_Time := Next_Time + Period;
         delay until Next_Time;
      end loop;

   exception
      when others =>
         Fuel_Data.Invalidate_Fuel_Quantity;
         Fuel_Data.Invalidate_Time_To_Minimum_Fuel;
         raise;
   end Simulate_Fuel_Data;

   ----------
   -- PBIT --
   ----------

   procedure PBIT is
      use Monitored_Tasking;
   begin

      Fuel_Task_Control.Set_PBIT (In_Progress_BIT_State);

      delay 3.0;

      Fuel_Task_Control.Set_PBIT (Pass_BIT_State);
   end PBIT;

   ----------
   -- CBIT --
   ----------

   procedure CBIT is
      use Monitored_Tasking;
   begin
      Fuel_Task_Control.Set_CBIT (Pass_BIT_State);
   end CBIT;

end Fuel_Subsystem;
