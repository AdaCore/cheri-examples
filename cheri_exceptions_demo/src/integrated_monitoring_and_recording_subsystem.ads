--
--  Copyright (C) 2023, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--
with System;
with Monitored_Tasking; use Monitored_Tasking;
with Ada.Exceptions;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Real_Time; use Ada.Real_Time;

package Integrated_Monitoring_And_Recording_Subsystem is

   Number_Of_Subsystems : constant Positive := 8;

   type Subsystem_Name_Type is
     (IMRS,
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

   function Get_PBIT_Status return BIT_Pass_Fail_Type;

   function Get_CBIT_Status return BIT_Pass_Fail_Type;

   function Get_PBIT_Status (Subsystem : Subsystem_Name_Type)
                             return BIT_Pass_Fail_Type;

   function Get_CBIT_Status (Subsystem : Subsystem_Name_Type)
                            return BIT_Pass_Fail_Type;

   function Get_PBIT_Report return String;

   function Get_CBIT_Report return String;

   function Get_BIT_Historic_Report return Historic_BIT_Entries_Data;

   IMRS_Control_Task : aliased Monitored_Tasking.Task_Control;

private

   type Subsystem_BIT_Status_List_Entry;

   type Subsystem_BIT_Status_List_Entry_Access is access all
     Subsystem_BIT_Status_List_Entry;

   type Subsystem_BIT_Status_List_Entry is record
      Subsystem  : Subsystem_Name_Type;
      Result     : BIT_Result_Type;
      Next       : Subsystem_BIT_Status_List_Entry_Access;
   end record;

   type Subsystem_BIT_Status_List is record
      Head : Subsystem_BIT_Status_List_Entry_Access := null;
      Tail : Subsystem_BIT_Status_List_Entry_Access := null;
   end record;

   protected IMRS_Data is

      function Get_PBIT_Status return BIT_Pass_Fail_Type;

      function Get_PBIT_Status (Subsystem : Subsystem_Name_Type)
                                return BIT_Pass_Fail_Type;

      function Get_CBIT_Status return BIT_Pass_Fail_Type;

      function Get_CBIT_Status (Subsystem : Subsystem_Name_Type)
                                return BIT_Pass_Fail_Type;

      function Get_PBIT return String;

      procedure Set_PBIT (PBIT : BIT_Result_Type;
                          Subsystem : Subsystem_Name_Type);

      function Get_CBIT return String;

      function Get_Historic_BIT_Report return Historic_BIT_Entries_Data;

      procedure Set_CBIT (CBIT : BIT_Result_Type;
                          Subsystem : Subsystem_Name_Type);

   private

      Stored_Exception : Ada.Exceptions.Exception_Occurrence;
      Current_CBIT     : Subsystem_BIT_Status_List;
      BIT_Failures     : Subsystem_BIT_Status_List;
      Current_PBIT     : Subsystem_BIT_Status_List;

   end IMRS_Data;

   PBIT_Performed : Boolean := False;

   procedure Perform_IMRS_Functions;
   --  On first execute collate the PBIT report, on subsequent calls collate
   --  the CBIT reports

   function Collate_PBIT_Report return Boolean;
   --  Derive a PBIT report by asking each LRU for their PBIT status, return
   --  True if all subsystems have complete PBIT

   procedure Collate_CBIT_Report;
   --  Derive a CBIT report by asking each LRU for their PBIT status

   procedure PBIT;

   procedure CBIT;

   IMRS_Task : Monitored_Tasking.Monitored_Task
     (Task_Body => Perform_IMRS_Functions'Access,
      PBIT_Func      => PBIT'Access,
      CBIT_Func      => CBIT'Access,
      Control   => IMRS_Control_Task'Access,
      Priority  => System.Priority'Last);

end Integrated_Monitoring_And_Recording_Subsystem;
