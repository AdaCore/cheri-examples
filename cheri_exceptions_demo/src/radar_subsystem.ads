--
--  Copyright (C) 2023, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--
with System;
with System.Storage_Elements; use System.Storage_Elements;

with Monitored_Tasking;

--  This subsystem implements a task to simulate a basic radar subsystem.
--  For the purposes of this demo this subsystem is intentionally vulnerable
--  to a CHERI capability bounds fault by sending a radar data buffer that is
--  too small.

package Radar_Subsystem is

   -----------------
   -- Radar Types --
   -----------------

   type Coordinate is record
      X : Integer;
      Y : Integer;
   end record;

   type Bearing is range 0 .. 359;

   type Knots is new Natural;

   type Track_ID is new Natural;

   type Track is record
      Position : Coordinate := (0, 0);
      Heading  : Bearing    := 0;
      Speed    : Knots      := 0;
      ID       : Track_ID   := 0;
   end record;

   Max_Tracks : constant := 10;

   subtype Track_Count is Natural range 0 .. Max_Tracks;

   subtype Track_Index is Track_Count range 1 .. Max_Tracks;

   type Track_Array is array (Track_Index range <>) of Track;

   ----------------
   -- Radar Data --
   ----------------

   protected Radar_Data is

      function Is_Operational return Boolean;

      procedure Set_Operational (Operational : Boolean);

      function Tracks return Track_Array;

      procedure Add_Track (T : Track);

      procedure Clear_Tracks;

   private

      Operational_Flag : Boolean := False;

      Tracks_List : Track_Array (1 .. Max_Tracks) := (others => <>);
      Num_Tracks  : Track_Count := 0;

   end Radar_Data;

   -------------------
   -- Radar Control --
   -------------------

   type Storage_Array_Access is access Storage_Array;

   Raw_Data_Size : constant Storage_Count :=
     Track'Size / System.Storage_Unit;

   protected Radar_Control is

      procedure Process_Radar_Data (Raw_Data : Storage_Array);

      function Get_Radar_Data return Storage_Array;

      entry Wait_Data_Ready;

   private

      Buffer : Storage_Array_Access := new Storage_Array (1 .. Raw_Data_Size);
      --  Buffer for holding raw radar data

      Data_Length : Storage_Count := 0;
      Data_Ready  : Boolean       := False;

   end Radar_Control;

   Radar_Task_Control : aliased Monitored_Tasking.Task_Control;

private

   procedure Simulate_Radar;

   procedure PBIT;

   procedure CBIT;

   Radar_Task : Monitored_Tasking.Monitored_Task
     (Task_Body => Simulate_Radar'Access,
      PBIT_Func      => PBIT'Access,
      CBIT_Func      => CBIT'Access,
      Control   => Radar_Task_Control'Access,
      Priority  => System.Priority'Last);

end Radar_Subsystem;
