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

      procedure Is_Operational_Changed (Changed : out Boolean);

      function Is_Operational return Boolean;

      procedure Set_Operational (Operational : Boolean);

      function Tracks return Track_Array;

      procedure Have_Tracks_Changed (Changed : out Boolean);

      procedure Add_Track (T : Track);

      procedure Clear_Tracks;

   private

      Operational_Flag : Boolean := False;
      Operational_Flag_Changed : Boolean := True;

      Tracks_List : Track_Array (1 .. Max_Tracks) := (others => <>);
      Num_Tracks  : Track_Count := 0;
      Num_Tracks_Changed : Boolean := True;

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
