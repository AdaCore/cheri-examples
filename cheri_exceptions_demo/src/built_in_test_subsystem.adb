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

with Countermeasures_Subsystem;
with Flight_Subsystem;
with Fuel_Subsystem;
with Navigation_Subsystem;
with Radar_Subsystem;
with Stores_Subsystem;
with Targeting_Subsystem;

package body Built_In_Test_Subsystem is

   Update_Period : constant Time_Span := Seconds (1);

   -----------------------------
   -- Get_PBIT_Status_Changed --
   -----------------------------

   procedure Get_PBIT_Status_Changed (Changed : out Boolean) is
   begin
      Built_In_Test_Data.Get_OVerall_PBIT_Status_Changed (Changed);
   end Get_PBIT_Status_Changed;

   ---------------------
   -- Get_PBIT_Status --
   ---------------------

   function Get_PBIT_Status return BIT_Pass_Fail_Type is
      (Built_In_Test_Data.Get_Overall_PBIT_Status);

   -----------------------------
   -- Get_CBIT_Status_Changed --
   -----------------------------

   procedure Get_CBIT_Status_Changed (Changed : out Boolean) is
   begin
      Built_In_Test_Data.Get_Overall_CBIT_Status_Changed (Changed);
   end Get_CBIT_Status_Changed;

   ---------------------
   -- Get_CBIT_Status --
   ---------------------

   function Get_CBIT_Status return BIT_Pass_Fail_Type is
     (Built_In_Test_Data.Get_Overall_CBIT_Status);

   -----------------------------
   -- Get_PBIT_Status_Changed --
   -----------------------------

   procedure Get_PBIT_Status_Changed
     (Subsystem : Subsystem_Name_Type; Changed : out Boolean) is
   begin
      Built_In_Test_Data.Get_PBIT_Status_Changed (Subsystem, Changed);
   end Get_PBIT_Status_Changed;

   ---------------------
   -- Get_PBIT_Status --
   ---------------------

   function Get_PBIT_Status (Subsystem : Subsystem_Name_Type)
                             return BIT_Pass_Fail_Type is
      (Built_In_Test_Data.Get_PBIT_Status (Subsystem));

   -----------------------------
   -- Get_CBIT_Status_Changed --
   -----------------------------

   procedure Get_CBIT_Status_Changed
     (Subsystem : Subsystem_Name_Type; Changed : out Boolean) is
   begin
      Built_In_Test_Data.Get_CBIT_Status_Changed (Subsystem, Changed);
   end Get_CBIT_Status_Changed;

   ---------------------
   -- Get_CBIT_Status --
   ---------------------

   function Get_CBIT_Status (Subsystem : Subsystem_Name_Type)
                             return BIT_Pass_Fail_Type is
     (Built_In_Test_Data.Get_CBIT_Status (Subsystem));

   -------------------------------------
   -- Get_BIT_Historic_Report_Changed --
   -------------------------------------

   procedure Get_BIT_Historic_Report_Changed (Changed : out Boolean) is
   begin
      Built_In_Test_Data.Get_BIT_Historic_Report_Changed (Changed);
   end Get_BIT_Historic_Report_Changed;

   ------------------------------
   -- Get_CBIT_Historic_Report --
   ------------------------------

   function Get_BIT_Historic_Report return Historic_BIT_Entries_Data is
      (Built_In_Test_Data.Get_Historic_BIT_Report);

   ---------------
   -- Built_In_Test_Data --
   ---------------

   protected body Built_In_Test_Data is

      --------------------
      -- Get_BIT_Status --
      --------------------

      function Get_BIT_Status
        (BIT_List : Subsystem_BIT_Status_List; Subsystem : Subsystem_Name_Type)
         return BIT_Pass_Fail_Type
      is
         Next_Entry : Subsystem_BIT_Status_List_Entry_Access :=
           BIT_List.Head;
      begin
         while Next_Entry /= null loop
            if Next_Entry.Subsystem = Subsystem then
               return Next_Entry.Result.Result;
            else
               Next_Entry := Next_Entry.Next;
            end if;
         end loop;

         return Not_Performed;
      end Get_BIT_Status;

      ----------------------------
      -- Get_BIT_Status_Changed --
      ----------------------------

      procedure Get_BIT_Status_Changed
        (BIT_List : Subsystem_BIT_Status_List; Subsystem : Subsystem_Name_Type;
         Changed : out Boolean)
      is
         Next_Entry : Subsystem_BIT_Status_List_Entry_Access :=
           BIT_List.Head;
         Found_Entry : Boolean := False;
      begin

         --  Loop until we have found the subsystem we are interested in
         while Next_Entry /= null and then Found_Entry = False loop
            if Next_Entry.Subsystem = Subsystem then
               Changed := Next_Entry.Changed;
               Next_Entry.Changed := False;
               Found_Entry := True;
               exit;
            end if;
            Next_Entry := Next_Entry.Next;
         end loop;

         if not Found_Entry then
            Changed := False;
         end if;

      end Get_BIT_Status_Changed;

       -------------------------------------------
       -- Convert_BIT_Results_To_Overall_Status --
       -------------------------------------------

      function Convert_BIT_Results_To_Overall_Status
        (BIT_List : Subsystem_BIT_Status_List) return BIT_Pass_Fail_Type
      is
         Next_Entry : Subsystem_BIT_Status_List_Entry_Access :=
           BIT_List.Head;
         type List_Count_Type is new Natural
         range Natural'First .. Number_Of_Subsystems;
         List_Count : List_Count_Type := List_Count_Type'First;
      begin

         --  Check if we have recieved all of the submissions
         while Next_Entry /= null loop
            List_Count := List_Count_Type'Succ (List_Count);
            Next_Entry := Next_Entry.Next;
         end loop;

         if List_Count = List_Count_Type'Last then

            Next_Entry := BIT_List.Head;

            while Next_Entry /= null loop
               if Next_Entry.Result.Result /= Pass then
                  return Next_Entry.Result.Result;
               end if;
               Next_Entry := Next_Entry.Next;
            end loop;

            return Pass;
         else
            return In_Progress;
         end if;
      end Convert_BIT_Results_To_Overall_Status;

      ----------------------------------
      -- Add_New_Entry_To_BIT_Results --
      ----------------------------------

      procedure Add_New_Entry_To_BIT_Results
        (BIT_Entry : BIT_Result_Type;
         Subsystem : Subsystem_Name_Type;
         BIT_List  : in out Subsystem_BIT_Status_List)
      is
         Entry_Not_In_List : Boolean := True;
         Next_Entry : Subsystem_BIT_Status_List_Entry_Access :=
           BIT_List.Head;
      begin

         --  Check if we are updating an entry
         while Next_Entry /= null loop
            if Next_Entry.Subsystem = Subsystem then
               Next_Entry.Result     := BIT_Entry;
               Next_Entry.Changed    := True;
               Entry_Not_In_List     := False;
               exit;
            else
               Next_Entry := Next_Entry.Next;
            end if;
         end loop;

         --  Or adding a new entry
         if Entry_Not_In_List then
            declare
               New_entry : constant Subsystem_BIT_Status_List_Entry_Access :=
                 new Subsystem_BIT_Status_List_Entry'
                   (Subsystem  => Subsystem,
                    Result     => BIT_Entry,
                    Changed    => True,
                    Next       => null);
            begin
               if BIT_List.Head = null then
                  BIT_List.Head := New_entry;
                  BIT_List.Tail := New_entry;
               else
                  BIT_List.Tail.Next := New_entry;
                  BIT_List.Tail      := New_entry;
               end if;
            end;
         end if;
      end Add_New_Entry_To_BIT_Results;

      -------------------------------------------
      -- Add_New_Entry_To_Historic_BIT_Results --
      -------------------------------------------

      procedure Add_Entry_To_Historic_BIT_Results
        (BIT_Entry : BIT_Result_Type;
         Subsystem : Subsystem_Name_Type)
      is
         Next_Entry : Subsystem_BIT_Status_List_Entry_Access :=
           BIT_Failures.Head;
         Entry_Not_In_List : Boolean := True;
      begin

         --  Check if we already have this entry
         while Next_Entry /= null loop
            if Next_Entry.Result.Time_Stamp = BIT_Entry.Time_Stamp then
               Entry_Not_In_List     := False;
               exit;
            else
               Next_Entry := Next_Entry.Next;
            end if;
         end loop;

         if Entry_Not_In_List then
            declare
               New_entry : constant Subsystem_BIT_Status_List_Entry_Access :=
                 new Subsystem_BIT_Status_List_Entry'
                   (Subsystem  => Subsystem,
                    Result     => BIT_Entry,
                    Changed    => True,
                    Next       => null);
            begin
               if BIT_Failures.Head = null then
                  BIT_Failures.Head := New_entry;
               else
                  New_entry.Next    := BIT_Failures.Head;
                  BIT_Failures.Head := New_entry;
               end if;
            end;
         end if;
      end Add_Entry_To_Historic_BIT_Results;

      -----------------------------
      -- Get_PBIT_Status_Changed --
      -----------------------------

      procedure Get_Overall_PBIT_Status_Changed (Changed : out Boolean) is
      begin
         if PBIT_Changed then
            Changed := True;
            PBIT_Changed := False;
         else
            Changed := False;
         end if;
      end Get_Overall_PBIT_Status_Changed;

      ---------------------
      -- Get_PBIT_Status --
      ---------------------

      function Get_Overall_PBIT_Status return BIT_Pass_Fail_Type is
        (Convert_BIT_Results_To_Overall_Status (Current_PBIT));

      -----------------------------
      -- Get_PBIT_Status_Changed --
      -----------------------------

      procedure Get_PBIT_Status_Changed
        (Subsystem : Subsystem_Name_Type; Changed : out Boolean) is
      begin
         Get_BIT_Status_Changed (Current_PBIT, Subsystem, Changed);
      end Get_PBIT_Status_Changed;

      ---------------------
      -- Get_PBIT_Status --
      ---------------------

      function Get_PBIT_Status (Subsystem : Subsystem_Name_Type)
                                return BIT_Pass_Fail_Type is
        (Get_BIT_Status (Current_PBIT, Subsystem));

      -----------------------------
      -- Get_PBIT_Status_Changed --
      -----------------------------

      procedure Get_Overall_CBIT_Status_Changed (Changed : out Boolean) is
      begin
         if CBIT_Changed then
            Changed := True;
            CBIT_Changed := False;
         else
            Changed := False;
         end if;
      end Get_Overall_CBIT_Status_Changed;

      ---------------------
      -- Get_CBIT_Status --
      ---------------------

      function Get_Overall_CBIT_Status return BIT_Pass_Fail_Type is
        (Convert_BIT_Results_To_Overall_Status (Current_CBIT));

      ---------------------
      -- Get_CBIT_Status --
      ---------------------

      procedure Get_CBIT_Status_Changed
        (Subsystem : Subsystem_Name_Type; Changed : out Boolean) is
      begin
         Get_BIT_Status_Changed (Current_CBIT, Subsystem, Changed);
      end Get_CBIT_Status_Changed;

      ---------------------
      -- Get_CBIT_Status --
      ---------------------

      function Get_CBIT_Status (Subsystem : Subsystem_Name_Type)
                                return BIT_Pass_Fail_Type is
        (Get_BIT_Status (Current_CBIT, Subsystem));

      --------------
      -- Set_PBIT --
      --------------

      procedure Set_PBIT (PBIT : BIT_Result_Type;
                          Subsystem : Subsystem_Name_Type) is
      begin

         PBIT_Changed := True;

         Add_New_Entry_To_BIT_Results
           (BIT_Entry => PBIT,
            Subsystem => Subsystem,
            BIT_List  => Current_PBIT);

         --  If the result was a failure add it to the historic list
         if PBIT.Result = Fail then
            Add_Entry_To_Historic_BIT_Results
              (BIT_Entry => PBIT,
               Subsystem => Subsystem);
         end if;

      end Set_PBIT;

      -------------------------------------
      -- Get_BIT_Historic_Report_Changed --
      -------------------------------------

      procedure Get_BIT_Historic_Report_Changed (Changed : out Boolean) is
      begin
         if Report_Changed then
            Changed := True;
            Report_Changed := False;
         else
            Changed := False;
         end if;
      end Get_BIT_Historic_Report_Changed;

      -----------------------
      -- Get_Historic_CBIT --
      -----------------------

      function Get_Historic_BIT_Report return Historic_BIT_Entries_Data is
         Return_Data : Historic_BIT_Entries_Data;
         Next_Entry : Subsystem_BIT_Status_List_Entry_Access :=
           BIT_Failures.Head;
         Current_Entry : Historic_BIT_Entries_Index :=
           Historic_BIT_Entries_Index'First;
      begin

         while Next_Entry /= null
           and then
             Return_Data.Number_Of_Entries /=
               Number_Of_Historic_BIT_Entries_Type'Last
         loop

            Return_Data.Number_Of_Entries :=
              Number_Of_Historic_BIT_Entries_Type'Succ
                (Return_Data.Number_Of_Entries);

            Return_Data.Entries (Current_Entry).Subsystem :=
              Next_Entry.Subsystem;

            Return_Data.Entries (Current_Entry).Time_Stamp :=
              Next_Entry.Result.Time_Stamp;

            Return_Data.Entries (Current_Entry).Failure_Text :=
              Next_Entry.Result.Failure_Message;

            --  Only show the last 4 entries
            if Current_Entry = Historic_BIT_Entries_Index'Last then
               exit;
            else
               Current_Entry :=
                 Historic_BIT_Entries_Index'Succ (Current_Entry);
               Next_Entry := Next_Entry.Next;
            end if;
         end loop;

         return Return_Data;

      end Get_Historic_BIT_Report;

      --------------
      -- Set_CBIT --
      --------------

      procedure Set_CBIT (CBIT      : BIT_Result_Type;
                          Subsystem : Subsystem_Name_Type) is
      begin

         CBIT_Changed := True;
         Report_Changed := True;

         --  Add results to the active list
         Add_New_Entry_To_BIT_Results
           (BIT_Entry => CBIT,
            Subsystem => Subsystem,
            BIT_List  => Current_CBIT);

         --  If the result was a failure add it to the historic list
         if CBIT.Result = Fail then
            Add_Entry_To_Historic_BIT_Results
              (BIT_Entry => CBIT,
               Subsystem => Subsystem);
         end if;
      end Set_CBIT;

   end Built_In_Test_Data;

   ----------
   -- PBIT --
   ----------

   procedure PBIT is
   begin
      Built_In_Test_Control_Task.Set_PBIT (Pass_BIT_State);
   end PBIT;

   ----------
   -- CBIT --
   ----------

   procedure CBIT is
   begin
      Built_In_Test_Control_Task.Set_CBIT (Pass_BIT_State);
   end CBIT;

   ----------------------------
   -- Perform_Built_In_Test_Functions --
   ----------------------------

   procedure Perform_Built_In_Test_Functions is
      Next_Time : Ada.Real_Time.Time;
   begin
      Next_Time := Ada.Real_Time.Clock;
      if PBIT_Performed then
         Collate_CBIT_Report;
      else
         PBIT_Performed := Collate_PBIT_Report;
      end if;

      Next_Time := Next_Time + Update_Period;
      delay until Next_Time;

   end Perform_Built_In_Test_Functions;

   ------------------
   -- Perform_PBIT --
   ------------------

   function Collate_PBIT_Report return Boolean is

      --  Get all of the PBIT results from each subsystem
      Built_In_Test_PBIT : constant BIT_Result_Type :=
        Built_In_Test_Control_Task.Get_PBIT;

      Countermeasures_PBIT : constant BIT_Result_Type :=
        Countermeasures_Subsystem.Countermeasures_Task_Control.Get_PBIT;

      Flight_PBIT : constant BIT_Result_Type :=
        Flight_Subsystem.Flight_Task_Control.Get_PBIT;

      Fuel_PBIT : constant BIT_Result_Type :=
        Fuel_Subsystem.Fuel_Task_Control.Get_PBIT;

      Navigation_PBIT : constant BIT_Result_Type :=
        Navigation_Subsystem.Navigation_Task_Control.Get_PBIT;

      Radar_PBIT : constant BIT_Result_Type :=
        Radar_Subsystem.Radar_Task_Control.Get_PBIT;

      Stores_PBIT : constant BIT_Result_Type :=
        Stores_Subsystem.Stores_Task_Control.Get_PBIT;

      Targeting_PBIT : constant BIT_Result_Type :=
        Targeting_Subsystem.Targeting_Task_Control.Get_PBIT;

   begin

      Built_In_Test_Data.Set_PBIT
        (PBIT      => Built_In_Test_PBIT,
         Subsystem => Built_In_Test);

      Built_In_Test_Data.Set_PBIT
        (PBIT      => Countermeasures_PBIT,
         Subsystem => Countermeasures);

      Built_In_Test_Data.Set_PBIT
        (PBIT      => Flight_PBIT,
         Subsystem => Flight);

      Built_In_Test_Data.Set_PBIT
        (PBIT      => Fuel_PBIT,
         Subsystem => Fuel);

      Built_In_Test_Data.Set_PBIT
        (PBIT      => Navigation_PBIT,
         Subsystem => Navigation);

      Built_In_Test_Data.Set_PBIT
        (PBIT      => Radar_PBIT,
         Subsystem => Radar);

      Built_In_Test_Data.Set_PBIT
        (PBIT      => Stores_PBIT,
         Subsystem => Stores);

      Built_In_Test_Data.Set_PBIT
        (PBIT      => Targeting_PBIT,
         Subsystem => Targeting);

      --  Return true if all subsystems have complete PBIT
      return
        (Countermeasures_PBIT.Result = Pass or else
         Countermeasures_PBIT.Result = Fail)
        and then
        (Flight_PBIT.Result          = Pass or else
         Flight_PBIT.Result          = Fail)
        and then
        (Fuel_PBIT.Result            = Pass or else
         Fuel_PBIT.Result            = Fail)
        and then
        (Navigation_PBIT.Result      = Pass or else
         Navigation_PBIT.Result      = Fail)
        and then
        (Radar_PBIT.Result           = Pass or else
         Radar_PBIT.Result           = Fail)
        and then
        (Stores_PBIT.Result          = Pass or else
         Stores_PBIT.Result          = Fail)
        and then
        (Targeting_PBIT.Result       = Pass or else
         Targeting_PBIT.Result       = Fail);

   end Collate_PBIT_Report;

   ------------------
   -- Perform_CBIT --
   ------------------

   procedure Collate_CBIT_Report is
   begin

      --  Derive the latest set of results
      Built_In_Test_Data.Set_CBIT
        (CBIT      => Built_In_Test_Control_Task.Get_CBIT,
         Subsystem => Built_In_Test);

      Built_In_Test_Data.Set_CBIT
        (CBIT =>
           Countermeasures_Subsystem.Countermeasures_Task_Control.Get_CBIT,
         Subsystem => Countermeasures);

      Built_In_Test_Data.Set_CBIT
        (CBIT      => Flight_Subsystem.Flight_Task_Control.Get_CBIT,
         Subsystem => Flight);

      Built_In_Test_Data.Set_CBIT
        (CBIT      => Fuel_Subsystem.Fuel_Task_Control.Get_CBIT,
         Subsystem => Fuel);

      Built_In_Test_Data.Set_CBIT
        (CBIT      => Navigation_Subsystem.Navigation_Task_Control.Get_CBIT,
         Subsystem => Navigation);

      Built_In_Test_Data.Set_CBIT
        (CBIT      => Radar_Subsystem.Radar_Task_Control.Get_CBIT,
         Subsystem => Radar);

      Built_In_Test_Data.Set_CBIT
        (CBIT      => Stores_Subsystem.Stores_Task_Control.Get_CBIT,
         Subsystem => Stores);

      Built_In_Test_Data.Set_CBIT
        (CBIT      => Targeting_Subsystem.Targeting_Task_Control.Get_CBIT,
         Subsystem => Targeting);
   end Collate_CBIT_Report;

end Built_In_Test_Subsystem;
