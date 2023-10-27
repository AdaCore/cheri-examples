--
--  Copyright (C) 2023, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--
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

      -------------------
      -- Fuel_Quantity --
      -------------------

      function Fuel_Quantity return Kilograms is (FQI_Value);

      -------------------------------
      -- Time_To_Minimum_Available --
      -------------------------------

      function Time_To_Minimum_Available return Boolean is
        (Time_To_Minimum_Fuel_Valid);

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
         FQI_Value := Value;
         FQI_Valid := True;
      end Set_Fuel_Quantity;

      ------------------------------
      -- Set_Time_To_Minimum_Fuel --
      ------------------------------

      procedure Set_Time_To_Minimum_Fuel (Value : Duration) is
      begin
         Time_To_Minimum_Fuel_Value := Value;
         Time_To_Minimum_Fuel_Valid := True;
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
