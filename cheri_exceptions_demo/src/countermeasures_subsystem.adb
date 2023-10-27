--
--  Copyright (C) 2023, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--
with Flight_Subsystem;     use Flight_Subsystem;
with Fuel_Subsystem;       use Fuel_Subsystem;
with Navigation_Subsystem; use Navigation_Subsystem;
with Radar_Subsystem;      use Radar_Subsystem;
with Stores_Subsystem;     use Stores_Subsystem;
with Targeting_Subsystem;  use Targeting_Subsystem;

package body Countermeasures_Subsystem is

   -----------------------------
   -- Countermeasures_Control --
   -----------------------------

   protected body Countermeasures_Control is

      ----------------
      -- Get_Status --
      ----------------

      function Get_Status return Status_Kind is (Status);

      ------------
      -- Deploy --
      ------------

      procedure Deploy is
      begin
         if Status = Ready then
            Deploy_Requested := True;
         end if;
      end Deploy;

      -----------------
      -- Wait_Deploy --
      -----------------

      entry Wait_Deploy when Deploy_Requested is
      begin
         Deploy_Requested := False;
         Status           := Deployed;
      end Wait_Deploy;

      ----------------
      -- Make_Ready --
      ----------------

      procedure Make_Ready is
      begin
         Status := Ready;
      end Make_Ready;

   end Countermeasures_Control;

   ------------------------------
   -- Simulate_Countermeasures --
   ------------------------------

   procedure Simulate_Countermeasures is
      use Monitored_Tasking;

   begin
      loop
         Countermeasures_Control.Wait_Deploy;

         if Radar_Task_Control.Status = Compromised then
            Radar_Task_Control.Reset;
         end if;

         if Targeting_Task_Control.Status = Compromised then
            Targeting_Task_Control.Reset;
         end if;

         if Stores_Task_Control.Status = Compromised then
            Stores_Task_Control.Reset;
         end if;

         if Flight_Task_Control.Status = Compromised then
            Flight_Task_Control.Reset;
         end if;

         if Fuel_Task_Control.Status = Compromised then
            Fuel_Task_Control.Reset;
         end if;

         if Navigation_Task_Control.Status = Compromised then
            Navigation_Task_Control.Reset;
         end if;

         delay 2.0; --  Simulate processing time to reset the countermeasures

         Countermeasures_Control.Make_Ready;
      end loop;
   end Simulate_Countermeasures;

   ----------
   -- PBIT --
   ----------

   procedure PBIT is
      use Monitored_Tasking;
   begin

      Countermeasures_Task_Control.Set_PBIT (In_Progress_BIT_State);

      delay 5.0;

      Countermeasures_Task_Control.Set_PBIT (Pass_BIT_State);
   end PBIT;

   ----------
   -- CBIT --
   ----------

   procedure CBIT is
      use Monitored_Tasking;
   begin
      Countermeasures_Task_Control.Set_CBIT (Pass_BIT_State);
   end CBIT;

end Countermeasures_Subsystem;
