--
--  Copyright (C) 2023, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--
with Ada.Real_Time; use Ada.Real_Time;

with Targeting_Subsystem;
with Monitored_Tasking;    use Monitored_Tasking;

package body Stores_Subsystem is

   Update_Period : constant Time_Span := Milliseconds (500);

   protected body Stores_Data is

      function Get_Light_Status (Idx : Light_Index) return Status_Kind is
      begin
         --  Light armaments require targeting data

         if Targeting_Subsystem.Targeting_Task_Control.Status = Normal then
            return Light_Status (Idx);
         elsif Light_Status (Idx) = Launched then
            return Launched;
         else
            return Unavailable;
         end if;
      end Get_Light_Status;

      function Get_Heavy_Status (Idx : Heavy_Index) return Status_Kind is
        (Heavy_Status (Idx));

      procedure Launch_Light is
      begin
         for Idx in Light_Index loop
            if Get_Light_Status (Idx) = Ready then
               Light_Status (Idx) := Launched;
               exit;
            end if;
         end loop;
      end Launch_Light;

      procedure Launch_Heavy is
      begin
         for Idx in Heavy_Index loop
            if Get_Heavy_Status (Idx) = Ready then
               Heavy_Status (Idx) := Launched;
               exit;
            end if;
         end loop;
      end Launch_Heavy;

   end Stores_Data;

   protected body Stores_Control is

      procedure Request_Launch_Light is
      begin
         Launch_Light := True;
      end Request_Launch_Light;

      procedure Request_Launch_Heavy is
      begin
         Launch_Heavy := True;
      end Request_Launch_Heavy;

      procedure Get_Requests
        (Light_Requested : out Boolean;
         Heavy_Requested : out Boolean)
      is
      begin
         Light_Requested := Launch_Light;
         Heavy_Requested := Launch_Heavy;

         Launch_Light := False;
         Launch_Heavy := False;
      end Get_Requests;

   end Stores_Control;

   procedure Simulate_Stores is
      Next_Time : Time := Clock;

      Light_Requested : Boolean;
      Heavy_Requested : Boolean;
   begin
      loop
         if Targeting_Subsystem.Targeting_Task_Control.Status /= Normal then
            Stores_Task_Control.Set_Status (Degraded);
         else
            Stores_Task_Control.Set_Status (Normal);
         end if;

         Stores_Control.Get_Requests
           (Light_Requested => Light_Requested,
            Heavy_Requested => Heavy_Requested);

         if Light_Requested then
            Stores_Data.Launch_Light;
         end if;

         if Heavy_Requested then
            Stores_Data.Launch_Heavy;
         end if;

         Next_Time := Next_Time + Update_Period;
         delay until Next_Time;
      end loop;
   end Simulate_Stores;

end Stores_Subsystem;
