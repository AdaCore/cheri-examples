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

with System;
with Monitored_Tasking; use Monitored_Tasking;
with Ada.Exceptions;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Real_Time; use Ada.Real_Time;

--  Emulates an Built_In_Test avionics subsystem by managing and collating BIT
--  reports PBIT and CBIT
package Built_In_Test_Subsystem is

   Number_Of_Subsystems : constant Positive := 8;

   type Subsystem_Name_Type is
     (Built_In_Test,
      Countermeasures,
      Flight,
      Fuel,
      Navigation,
      Radar,
      Stores,
      Targeting);

   type Historic_BIT_Entries_Index is new Natural range 0 .. 7;
   type Number_Of_Historic_BIT_Entries_Type is new Natural range 0 .. 8;

   type Historic_BIT_Entry is record
      Subsystem    : Subsystem_Name_Type;
      Failure_Text : Unbounded_String;
      Time_Stamp   : Ada.Real_Time.Time;
   end record;

   type Historic_BIT_Entries_Array is array (Historic_BIT_Entries_Index)
     of Historic_BIT_Entry;

   type Historic_BIT_Entries_Data is record
      Number_Of_Entries : Number_Of_Historic_BIT_Entries_Type := 0;
      Entries           : Historic_BIT_Entries_Array;
   end record;

   procedure Get_PBIT_Status_Changed (Changed : out Boolean);
   --  Set Changed to true if PBIT has changed since last read

   function Get_PBIT_Status return BIT_Pass_Fail_Type;

   --  Derive the overall PBIT status from the individual statuses

   procedure Get_CBIT_Status_Changed (Changed : out Boolean);
   --  Set Changed to true if CBIT has changed since last read

   function Get_CBIT_Status return BIT_Pass_Fail_Type;
   --  Derive the overall CBIT status from the individual statuses

   procedure Get_PBIT_Status_Changed
     (Subsystem : Subsystem_Name_Type; Changed : out Boolean);
   --  Set Changed to true if PBIT has transitioned for this subsystem since
   --  the last time is was read

   function Get_PBIT_Status (Subsystem : Subsystem_Name_Type)
                             return BIT_Pass_Fail_Type;
   --  Return a PBIT status for a given Subsystem

   procedure Get_CBIT_Status_Changed
     (Subsystem : Subsystem_Name_Type; Changed : out Boolean);
   --  Set Changed to true if CBIT has transitioned for this subsystem since
   --  the last time is was read

   function Get_CBIT_Status (Subsystem : Subsystem_Name_Type)
                            return BIT_Pass_Fail_Type;
   --  Return a CBIT status for a given Subsystem

   procedure Get_BIT_Historic_Report_Changed (Changed : out Boolean);
   --  Changed is set to True if the report has changed since last read

   function Get_BIT_Historic_Report return Historic_BIT_Entries_Data;
   --  Return a list of all historical BIT failures

   Built_In_Test_Control_Task : aliased Monitored_Tasking.Task_Control;

private

   --  Derive a linked list to hold BIT reports
   type Subsystem_BIT_Status_List_Entry;

   type Subsystem_BIT_Status_List_Entry_Access is access all
     Subsystem_BIT_Status_List_Entry;

   type Subsystem_BIT_Status_List_Entry is record
      Subsystem  : Subsystem_Name_Type;
      Result     : BIT_Result_Type;
      Changed    : Boolean;
      Next       : Subsystem_BIT_Status_List_Entry_Access;
   end record;

   type Subsystem_BIT_Status_List is record
      Head : Subsystem_BIT_Status_List_Entry_Access := null;
      Tail : Subsystem_BIT_Status_List_Entry_Access := null;
   end record;

   --  Protected body to allow thread safe transfer of Built_In_Test data
   --  (BIT reports)
   protected Built_In_Test_Data is

      procedure Get_Overall_PBIT_Status_Changed (Changed : out Boolean);
      --  Set Changed to true if PBIT has changed since last read

      function Get_Overall_PBIT_Status return BIT_Pass_Fail_Type;

      procedure Get_PBIT_Status_Changed
        (Subsystem : Subsystem_Name_Type; Changed : out Boolean);

      function Get_PBIT_Status (Subsystem : Subsystem_Name_Type)
                                return BIT_Pass_Fail_Type;

      procedure Get_Overall_CBIT_Status_Changed (Changed : out Boolean);
      --  Set Changed to true if CBIT has changed since last read

      function Get_Overall_CBIT_Status return BIT_Pass_Fail_Type;

      procedure Get_CBIT_Status_Changed
        (Subsystem : Subsystem_Name_Type; Changed : out Boolean);

      function Get_CBIT_Status (Subsystem : Subsystem_Name_Type)
                                return BIT_Pass_Fail_Type;

      procedure Set_PBIT (PBIT : BIT_Result_Type;
                          Subsystem : Subsystem_Name_Type);

      procedure Get_BIT_Historic_Report_Changed (Changed : out Boolean);

      function Get_Historic_BIT_Report return Historic_BIT_Entries_Data;

      procedure Set_CBIT (CBIT : BIT_Result_Type;
                          Subsystem : Subsystem_Name_Type);

   private

      Stored_Exception : Ada.Exceptions.Exception_Occurrence;
      Current_CBIT     : Subsystem_BIT_Status_List;
      CBIT_Changed     : Boolean := True;
      BIT_Failures     : Subsystem_BIT_Status_List;
      Current_PBIT     : Subsystem_BIT_Status_List;
      PBIT_Changed     : Boolean := True;
      Report_Changed   : Boolean := True;
   end Built_In_Test_Data;

   PBIT_Performed : Boolean := False;

   procedure Perform_Built_In_Test_Functions;
   --  On first execute collate the PBIT report, on subsequent calls collate
   --  the CBIT reports

   function Collate_PBIT_Report return Boolean;
   --  Derive a PBIT report by asking each LRU for their PBIT status, return
   --  True if all subsystems have complete PBIT

   procedure Collate_CBIT_Report;
   --  Derive a CBIT report by asking each LRU for their PBIT status

   procedure PBIT;

   procedure CBIT;

   Built_In_Test_Task : Monitored_Tasking.Monitored_Task
     (Task_Body => Perform_Built_In_Test_Functions'Access,
      PBIT_Func      => PBIT'Access,
      CBIT_Func      => CBIT'Access,
      Control   => Built_In_Test_Control_Task'Access,
      Priority  => System.Priority'Last);

end Built_In_Test_Subsystem;
