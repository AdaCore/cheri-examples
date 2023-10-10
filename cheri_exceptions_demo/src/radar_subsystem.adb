--
--  Copyright (C) 2023, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--
package body Radar_Subsystem is

   ----------------
   -- Radar_Data --
   ----------------

   protected body Radar_Data is

      --------------------
      -- Is_Operational --
      --------------------

      function Is_Operational return Boolean is (Operational_Flag);

      ---------------------
      -- Set_Operational --
      ---------------------

      procedure Set_Operational (Operational : Boolean) is
      begin
         Operational_Flag := Operational;
      end Set_Operational;

      ------------
      -- Tracks --
      ------------

      function Tracks return Track_Array is (Tracks_List (1 .. Num_Tracks));

      ---------------
      -- Add_Track --
      ---------------

      procedure Add_Track (T : Track) is
      begin
         if Num_Tracks < Max_Tracks then
            Num_Tracks := Num_Tracks + 1;
            Tracks_List (Num_Tracks) := T;
         end if;
      end Add_Track;

      ------------------
      -- Clear_Tracks --
      ------------------

      procedure Clear_Tracks is
      begin
         Num_Tracks := 0;
      end Clear_Tracks;

   end Radar_Data;

   -------------------
   -- Radar_Control --
   -------------------

   protected body Radar_Control is

      ------------------------
      -- Process_Radar_Data --
      ------------------------

      procedure Process_Radar_Data (Raw_Data : Storage_Array) is
      begin
         Data_Length := Storage_Count'Min (Raw_Data'Length, Buffer.all'Length);

         Buffer.all (1 .. Data_Length) :=
           Raw_Data (Raw_Data'First .. Raw_Data'First + Data_Length - 1);

         Data_Ready := True;

      end Process_Radar_Data;

      --------------------
      -- Get_Radar_Data --
      --------------------

      function Get_Radar_Data return Storage_Array is
      begin
         return Buffer.all (1 .. Data_Length);
      end Get_Radar_Data;

      ---------------------
      -- Wait_Data_Ready --
      ---------------------

      entry Wait_Data_Ready when Data_Ready is
      begin
         Data_Ready := False;
      end Wait_Data_Ready;

   end Radar_Control;

   --------------------
   -- Simulate_Radar --
   --------------------

   procedure Simulate_Radar is
      Next_ID : Track_ID := 0;

   begin
      Radar_Data.Set_Operational (True);

      loop
         Radar_Control.Wait_Data_Ready;

         declare
            --  This is intentionally unsafe way to convert data via a memory
            --  overlay to demonstrate a CHERI bounds error.

            Raw_Data : Storage_Array := Radar_Control.Get_Radar_Data;
            T        : Track with Import, Address => Raw_Data'Address;

         begin
            --  Assign an ID to this track. This triggers a CHERI capability
            --  bounds fault when Raw_Data is too small.

            T.ID    := Next_ID;
            Next_ID := Next_ID + 1;

            Radar_Data.Add_Track (T);
         end;
      end loop;

   exception
      when others =>
         Radar_Data.Set_Operational (False);
         Radar_Data.Clear_Tracks;
         raise;
   end Simulate_Radar;

end Radar_Subsystem;
