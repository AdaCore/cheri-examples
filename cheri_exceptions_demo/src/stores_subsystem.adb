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

with Targeting_Subsystem;

package body Stores_Subsystem is

   Update_Period : constant Time_Span := Milliseconds (500);

   protected body Stores_Data is

      ------------------------------
      -- Get_Light_Status_Changed --
      ------------------------------

      procedure Get_Light_Status_Changed
        (Idx : Light_Index; Changed : out Boolean)
      is
      begin
         if Light_Status_Changed (Idx) then
            Changed                    := True;
            Light_Status_Changed (Idx) := False;
         else
            Changed := False;
         end if;
      end Get_Light_Status_Changed;

      ----------------------
      -- Get_Light_Status --
      ----------------------

      function Get_Light_Status (Idx : Light_Index) return Status_Kind is
        (Light_Status (Idx));

      ------------------------------
      -- Get_Heavy_Status_Changed --
      ------------------------------

      procedure Get_Heavy_Status_Changed
        (Idx : Heavy_Index; Changed : out Boolean)
      is
      begin
         if Heavy_Status_Changed (Idx) then
            Changed                    := True;
            Heavy_Status_Changed (Idx) := False;
         else
            Changed := False;
         end if;
      end Get_Heavy_Status_Changed;

      ----------------------
      -- Get_Heavy_Status --
      ----------------------

      function Get_Heavy_Status (Idx : Heavy_Index) return Status_Kind is
        (Heavy_Status (Idx));

      ------------------
      -- Launch_Light --
      ------------------

      procedure Launch_Light is
      begin
         for Idx in Light_Index loop
            if Get_Light_Status (Idx) = Ready then
               Light_Status_Changed (Idx) := True;
               Light_Status (Idx)         := Launched;
               exit;
            end if;
         end loop;
      end Launch_Light;

      ------------------
      -- Launch_Heavy --
      ------------------

      procedure Launch_Heavy is
      begin
         for Idx in Heavy_Index loop
            if Get_Heavy_Status (Idx) = Ready then
               Heavy_Status_Changed (Idx) := True;
               Heavy_Status (Idx)         := Launched;
               exit;
            end if;
         end loop;
      end Launch_Heavy;

      ------------------------------------
      -- Targetting_System_State_Change --
      ------------------------------------

      procedure Targetting_System_State_Change (New_State : Task_Status_Kind)
      is
      begin
         Targetting_System_State := New_State;

         if Targetting_System_State = Degraded then

            for J in Light_Status_Array'Range loop
               if Light_Status (J) = Ready then
                  Light_Status (J) := Unavailable;
                  Light_Status_Changed (J) := True;
               end if;
            end loop;

         elsif Targetting_System_State = Normal then
            for J in Light_Status_Array'Range loop
               if Light_Status (J) = Unavailable then
                  Light_Status (J) := Ready;
                  Light_Status_Changed (J) := True;
               end if;
            end loop;
         end if;
      end Targetting_System_State_Change;

      -----------
      -- Reset --
      -----------

      procedure Reset is
      begin
         Light_Status         := (others => Ready);
         Light_Status_Changed := (others => True);
         Heavy_Status         := (others => Ready);
         Heavy_Status_Changed := (others => True);
      end Reset;

   end Stores_Data;

   --------------------
   -- Stores_Control --
   --------------------

   protected body Stores_Control is

      --------------------------
      -- Request_Launch_Light --
      --------------------------

      procedure Request_Launch_Light is
      begin
         Launch_Light := True;
      end Request_Launch_Light;

      --------------------------
      -- Request_Launch_Heavy --
      --------------------------

      procedure Request_Launch_Heavy is
      begin
         Launch_Heavy := True;
      end Request_Launch_Heavy;

      ------------------
      -- Get_Requests --
      ------------------

      procedure Get_Requests
        (Light_Requested : out Boolean; Heavy_Requested : out Boolean)
      is
      begin
         Light_Requested := Launch_Light;
         Heavy_Requested := Launch_Heavy;

         Launch_Light := False;
         Launch_Heavy := False;
      end Get_Requests;

   end Stores_Control;

   ---------------------
   -- Simulate_Stores --
   ---------------------

   procedure Simulate_Stores is
      Next_Time : Time := Clock;

      Light_Requested : Boolean;
      Heavy_Requested : Boolean;
   begin
      loop
         if Stores_Task_Control.Get_Status /= PBIT then
            if Targeting_Subsystem.Targeting_Task_Control.Get_Status = Degraded
            then
               Stores_Task_Control.Set_Status (Degraded);
            else
               Stores_Task_Control.Set_Status (Normal);
            end if;
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

   ----------
   -- PBIT --
   ----------

   procedure PBIT is
   begin

      Stores_Task_Control.Set_PBIT (In_Progress_BIT_State);

      delay 5.0;

      Stores_Task_Control.Set_PBIT (Pass_BIT_State);
   end PBIT;

   ----------
   -- CBIT --
   ----------

   procedure CBIT is
   begin
      Stores_Task_Control.Set_CBIT (Pass_BIT_State);
   end CBIT;

end Stores_Subsystem;
