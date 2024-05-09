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

with Ada.Real_Time;          use Ada.Real_Time;
with Ada.Text_IO;            use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with Countermeasures_Subsystem; use Countermeasures_Subsystem;
with Flight_Subsystem;
with Fuel_Subsystem;
with Monitored_Tasking;         use Monitored_Tasking;
with Navigation_Subsystem;
with Radar_Subsystem;
with Stores_Subsystem;          use Stores_Subsystem;
with Targeting_Subsystem;
with Built_In_Test_Subsystem;
use Built_In_Test_Subsystem;
with Common_Terminal_Controls; use Common_Terminal_Controls;
with Splash_Screen;

package body Display is

   Update_Period_Max     : constant Integer := 500;
   Update_Period_Min     : constant Integer := 3000;
   Update_Period_Integer : Integer := 500;
   Update_Period         : Time_Span := Milliseconds (Update_Period_Integer);

   -------------------------------
   -- Show_Main_Avionics_Screen --
   -------------------------------

   procedure Show_Main_Avionics_Screen is
   begin
      Display_Control_Data.Show_Main_Avioics_Screen;
   end Show_Main_Avionics_Screen;

   ---------------------------------
   -- Show_Flight_Recorder_Screen --
   ---------------------------------

   procedure Show_Flight_Recorder_Screen is
   begin
      Display_Control_Data.Show_Flight_Recorder_Screen;
   end Show_Flight_Recorder_Screen;

   -------------------------------
   -- Show_Splash_Screen_Screen --
   -------------------------------

   procedure Show_Splash_Screen_Screen is
   begin
      Display_Control_Data.Show_Splash_Screen;
   end Show_Splash_Screen_Screen;

   function Get_Current_Screen return Requested_Display_Type is
     (Display_Control_Data.Get_Current_Display);

   ----------------------
   -- Increase_Refresh --
   ----------------------

   procedure Increase_Refresh is
   begin
      Update_Period_Integer :=
        Update_Period_Integer -
          Integer ((Float (Update_Period_Integer) * 0.2));
      if Update_Period_Integer <= Update_Period_Max then
         Update_Period_Integer := Update_Period_Max;
      end if;
      Update_Period := Milliseconds (Update_Period_Integer);
   end Increase_Refresh;

   ----------------------
   -- Decrease_Refresh --
   ----------------------

   procedure Decrease_Refresh is
   begin
      Update_Period_Integer :=
        Update_Period_Integer +
          Integer ((Float (Update_Period_Integer) * 0.2));
      if Update_Period_Integer >= Update_Period_Min then
         Update_Period_Integer := Update_Period_Min;
      end if;
      Update_Period := Milliseconds (Update_Period_Integer);
   end Decrease_Refresh;

   Task_Status_Colour : constant
     array (Monitored_Tasking.Task_Status_Kind)
     of Text_Colour :=
       (PBIT             => Yellow,
        Normal           => Green,
        Degraded         => Yellow,
        Compromised      => Red,
        Resetting        => Yellow);
   --  Lookup table for the text colour of each task status

   BIT_Status_Colour : constant
     array (Monitored_Tasking.BIT_Pass_Fail_Type)
     of Text_Colour :=
       (Pass          => Green,
        Fail          => Red,
        Not_Performed => Yellow,
        In_Progress   => Yellow);
   --  Lookup table for the text colour of each BIT status

   Countermeasures_Status_Colour : constant
     array (Countermeasures_Subsystem.Status_Kind)
     of Text_Colour :=
       (Ready    => Green,
        Deployed => Red);
   --  Lookup table for the text colour of each countermeasure status

   Stores_Status_Colour : constant
     array (Stores_Subsystem.Status_Kind)
     of Text_Colour :=
       (Unavailable => Yellow,
        others      => Default);

   --  Subprograms for printing different parts of the UI.
   --  Each component has a static and dynamic part. Static components are only
   --  updated on a page change, dynamic components are updated on a page
   --  change and on a regular refresh loop.
   procedure Put_AdaCore_Logo;
   procedure Put_Radar_Data_Dynamic
     (Force_Refresh : Boolean := False);
   procedure Put_Radar_Data_Static;
   procedure Put_Flight_Data_Dynamic
     (Force_Refresh : Boolean := False);
   procedure Put_Flight_Data_Static;
   procedure Put_Fuel_Data_Dynamic
     (Force_Refresh : Boolean := False);
   procedure Put_Fuel_Data_Static;
   procedure Put_Navigation_Data_Dynamic
     (Force_Refresh : Boolean := False);
   procedure Put_Navigation_Data_Static;
   procedure Put_Targeting_Data_Dynamic
     (Force_Refresh : Boolean := False);
   procedure Put_Targeting_Data_Static;
   procedure Put_Stores_Data_Dynamic
     (Force_Refresh : Boolean := False);
   procedure Put_Stores_Data_Static;
   procedure Put_Built_In_Test_Data_Dynamic
     (Force_Refresh : Boolean := False);
   procedure Put_Built_In_Test_Data_Static;
   procedure Put_Countermeasures_Data_Dynamic
     (Force_Refresh : Boolean := False);
   procedure Put_Countermeasures_Data_Static;
   procedure Put_Commands;
   procedure Put_Flight_Recorder_Information_Dynamic
     (Force_Refresh : Boolean := False);
   procedure Put_Flight_Recorder_Information_Static;

   procedure Put_Task_Status (EXB : in out Monitored_Tasking.Task_Control;
                              Force_Refresh : Boolean);

   procedure Put_BIT_Status (Status : Monitored_Tasking.BIT_Pass_Fail_Type);

   procedure Put_Subsystem_Name
     (Name : String; X : Natural; Y : Natural; Width : Positive := 27);

   procedure Put_Countermeasure_Status
     (Status : Countermeasures_Subsystem.Status_Kind);

   procedure Put_Stores_Status (Status : Stores_Subsystem.Status_Kind);

   ---------------
   -- UI Layout --
   ---------------

   --  Screen coordinates of the UI elements. (0, 0) is the top-left.

   Countermeasures_X       : constant := 26;
   Countermeasures_Y       : constant := 0;
   Countermeasures_RSide_X : constant := 56;

   COl1_RSide_X : constant := 28;
   COl2_RSide_X : constant := 82;

   AdaCore_Logo_X : constant := 35;
   AdaCore_Logo_Y : constant := 8;

   Radar_X : constant := 0;
   Radar_Y : constant := 4;

   Targeting_X : constant := 0;
   Targeting_Y : constant := 10;

   Stores_X : constant := 0;
   Stores_Y : constant := 15;

   Flight_X : constant := 54;
   Flight_Y : constant := 4;

   Fuel_X : constant := 54;
   Fuel_Y : constant := 10;

   Navigation_X : constant := 54;
   Navigation_Y : constant := 16;

   Built_In_Test_X       : constant := 27;
   Built_In_Test_Y       : constant := 22;
   Built_In_Test_RSide_X : constant := 55;

   Commands_Main_Screen_X_Left  : constant := 0;
   Commands_Main_Screen_Y_Left  : constant := 27;

   Commands_Main_Screen_X_Right : constant := 40;
   Commands_Main_Screen_Y_Right : constant := 28;

   Commands_Flight_Recorder_Screen_X : constant := 0;
   Commands_Flight_Recorder_Screen_Y : constant := 29;

   Flight_Recorder_X       : constant := 0;
   Flight_Recorder_RSide_X : constant := 91;
   Flight_Recorder_Y       : constant := 0;

   Flight_Recorder_CBIT_X : constant := 0;
   Flight_Recorder_CBIT_Y : constant := 3;

   Flight_Recorder_PBIT_X : constant := 54;
   Flight_Recorder_PBIT_Y : constant := 3;

   Flight_Recorder_His_CBIT_X : constant := 0;
   Flight_Recorder_His_CBIT_Y : constant := 16;

   -----------------------
   -- Put_Ada_Core_Name --
   -----------------------

   procedure Put_AdaCore_Logo is
   begin
      pragma Style_Checks (Off);
      Move_Cursor_To (AdaCore_Logo_X + 1, AdaCore_Logo_Y);
      Set_Text_Colour (Red);
      Put ("𝗔𝗱𝗮𝗖𝗼𝗿𝗲 𝟮𝟬𝟮𝟰");
      Move_Cursor_To (AdaCore_Logo_X - 7, AdaCore_Logo_Y + 2);
      Put ("🍒  GNAT Pro for Morello 🍒");
      Move_Cursor_To (AdaCore_Logo_X - 4, AdaCore_Logo_Y + 5);
      Put (" 🇨 🇭 🇪 🇷 🇮");
      Put("  ");
      Put ("🇩 🇪 🇲 🇴 ");
      Move_Cursor_To (AdaCore_Logo_X - 1, AdaCore_Logo_Y + 8);
      Set_Text_Colour (White);
      Put ("🅓 ⓘ ⓖ ⓘ ⓣ ⓐ ⓛ");
      Move_Cursor_To (AdaCore_Logo_X - 1, AdaCore_Logo_Y + 9);
      Put ("🅢 ⓔ ⓒ ⓤ ⓡ ⓘ ⓣ ⓨ");
      Move_Cursor_To (AdaCore_Logo_X - 1, AdaCore_Logo_Y + 10);
      Put ("🅑 ⓨ");
      Move_Cursor_To (AdaCore_Logo_X - 1, AdaCore_Logo_Y + 11);
      Put ("🅓 ⓔ ⓢ ⓘ ⓖ ⓝ");
      Set_Text_Colour (Default);
      pragma Style_Checks (On);
   end Put_AdaCore_Logo;

   ---------------------------
   -- Put_Radar_Data_Static --
   ---------------------------

   procedure Put_Radar_Data_Static is
   begin
      pragma Style_Checks (Off);
      Put_Subsystem_Name ("Radar", Radar_X, Radar_Y);
      Move_Cursor_To (Radar_X, Radar_Y + 2);
      Put ("│ Status: ");

      Move_Cursor_To (COl1_RSide_X, Radar_Y + 2);
      Put ("│");

      Move_Cursor_To (Radar_X, Radar_Y + 3);
      Put ("│ On/Off:");


      Move_Cursor_To (COl1_RSide_X, Radar_Y + 3);
      Put ("│");

      Move_Cursor_To (Radar_X, Radar_Y + 4);
      Put ("│ Tracks:");

      Move_Cursor_To (COl1_RSide_X, Radar_Y + 4);
      Put ("│");

      Move_Cursor_To (Radar_X, Radar_Y + 5);
      Put_Line ("╰───────────────────────────╯");
      pragma Style_Checks (On);
   end Put_Radar_Data_Static;

   ----------------------------
   -- Put_Radar_Data_Dynamic --
   ----------------------------

   procedure Put_Radar_Data_Dynamic (Force_Refresh : Boolean := False)
   is
      Operation_State_Changed : Boolean;
      Tracks_Changed : Boolean;
   begin

      --  Output the task status
      Move_Cursor_To (Radar_X + 10, Radar_Y + 2);
      Put_Task_Status (Radar_Subsystem.Radar_Task_Control, Force_Refresh);

      --  Update the screen if the operation state has changed
      Radar_Subsystem.Radar_Data.Is_Operational_Changed
        (Operation_State_Changed);
      if Operation_State_Changed or Force_Refresh then
         Move_Cursor_To (Radar_X + 10, Radar_Y + 3);
         if Radar_Subsystem.Radar_Data.Is_Operational then
            Put ("On ");
         else
            Put ("Off");
         end if;
      end if;

      --  Update the screen if the tracks have changed and
      Radar_Subsystem.Radar_Data.Have_Tracks_Changed (Tracks_Changed);
      if Tracks_Changed or Force_Refresh then
         Move_Cursor_To (Radar_X + 9, Radar_Y + 4);
         Put ("               ");
         Move_Cursor_To (Radar_X + 9, Radar_Y + 4);
         Put (Radar_Subsystem.Radar_Data.Tracks'Length'Image);
      end if;
   end Put_Radar_Data_Dynamic;

   ----------------------------
   -- Put_Flight_Data_Static --
   ----------------------------

   procedure Put_Flight_Data_Static is
   begin
      pragma Style_Checks (Off);
      Put_Subsystem_Name ("Flight Data", Flight_X, Flight_Y);

      Move_Cursor_To (COl2_RSide_X, Flight_Y + 2);
      Put ("│");

      Move_Cursor_To (Flight_X, Flight_Y + 2);
      Put ("│ Status: ");

      Move_Cursor_To (COl2_RSide_X, Flight_Y + 3);
      Put ("│");

      Move_Cursor_To (Flight_X, Flight_Y + 3);
      Put ("│ Altitude:");

      Move_Cursor_To (COl2_RSide_X, Flight_Y + 4);
      Put ("│");

      Move_Cursor_To (Flight_X, Flight_Y + 4);
      Put ("│ Airspeed:");

      Move_Cursor_To (Flight_X, Flight_Y + 5);
      Put_Line ("╰───────────────────────────╯");
      pragma Style_Checks (On);
   end Put_Flight_Data_Static;

   -----------------------------
   -- Put_Flight_Data_Dynamic --
   -----------------------------

   procedure Put_Flight_Data_Dynamic (Force_Refresh : Boolean := False)
   is
      Altitude_Changed : Boolean;
      Airspeed_Changed : Boolean;
   begin
      Move_Cursor_To (Flight_X + 12, Flight_Y + 2);
      Put_Task_Status (Flight_Subsystem.Flight_Task_Control, Force_Refresh);

      --  Update the screen if the altitude has changed
      Flight_Subsystem.Flight_Data.Get_Altitude_Changed (Altitude_Changed);
      if Altitude_Changed or Force_Refresh then
         Move_Cursor_To (Flight_X + 11, Flight_Y + 3);
         Put (Flight_Subsystem.Flight_Data.Get_Altitude'Image);
         Put (" ft");
      end if;

      --  Update the screen if the airspeed has changed
      Flight_Subsystem.Flight_Data.Get_Airspeed_Changed (Airspeed_Changed);
      if Airspeed_Changed or Force_Refresh then
         Move_Cursor_To (Flight_X + 11, Flight_Y + 4);
         Put (Flight_Subsystem.Flight_Data.Get_Airspeed'Image);
         Put (" KIAS");
      end if;
   end Put_Flight_Data_Dynamic;

   --------------------------
   -- Put_Fuel_Data_Static --
   --------------------------

   procedure Put_Fuel_Data_Static is
   begin
      pragma Style_Checks (Off);
      Put_Subsystem_Name ("Fuel", Fuel_X, Fuel_Y);

      Move_Cursor_To (COl2_RSide_X, Fuel_Y + 2);
      Put ("│");

      Move_Cursor_To (Fuel_X, Fuel_Y + 2);
      Put ("│ Status: ");

      Move_Cursor_To (COl2_RSide_X, Fuel_Y + 3);
      Put ("│");

      Move_Cursor_To (Fuel_X, Fuel_Y + 3);
      Put ("│ FQI:   ");

      Move_Cursor_To (COl2_RSide_X, Fuel_Y + 4);
      Put ("│");

      Move_Cursor_To (Fuel_X, Fuel_Y + 4);
      Put ("│ Bingo: ");

      Move_Cursor_To (Fuel_X, Fuel_Y + 5);
      Put_Line ("╰───────────────────────────╯");
      pragma Style_Checks (On);
   end Put_Fuel_Data_Static;

   ---------------------------
   -- Put_Fuel_Data_Dynamic --
   ---------------------------

   procedure Put_Fuel_Data_Dynamic (Force_Refresh : Boolean := False)
   is
      Bingo_Minutes : Duration;
      Bingo_Changed : Boolean;
      FQI_Changed   : Boolean;
   begin
      Move_Cursor_To (Fuel_X + 10, Fuel_Y + 2);
      Put_Task_Status (Fuel_Subsystem.Fuel_Task_Control, Force_Refresh);

      --  Update the screen if the fuel level has changed
      Fuel_Subsystem.Fuel_Data.Fuel_Quantity_Changed (FQI_Changed);
      if FQI_Changed or Force_Refresh then
         Move_Cursor_To (Fuel_X + 9, Fuel_Y + 3);
         Put (Fuel_Subsystem.Fuel_Data.Fuel_Quantity'Image);
         Put (" kg");
      end if;

      --  Update the screen if the bingo time has changed
      Fuel_Subsystem.Fuel_Data.Time_To_Minimum_Fuel_Changed (Bingo_Changed);
      if Bingo_Changed or Force_Refresh then
         Move_Cursor_To (Fuel_X + 9, Fuel_Y + 4);
         Bingo_Minutes := Fuel_Subsystem.Fuel_Data.Time_To_Minimum_Fuel / 60.0;
         Put (Integer (Bingo_Minutes)'Image);
         Put (" minutes");
      end if;

   end Put_Fuel_Data_Dynamic;

   --------------------------------
   -- Put_Navigation_Data_Static --
   --------------------------------

   procedure Put_Navigation_Data_Static is
   begin
      pragma Style_Checks (Off);
      Put_Subsystem_Name ("Navigation", Navigation_X, Navigation_Y);

      Move_Cursor_To (COl2_RSide_X, Navigation_Y + 2);
      Put ("│");

      Move_Cursor_To (Navigation_X, Navigation_Y + 2);
      Put ("│ Status:    ");

      Move_Cursor_To (COl2_RSide_X, Navigation_Y + 3);
      Put ("│");

      Move_Cursor_To (Navigation_X, Navigation_Y + 3);
      Put ("│ Latitude:  ");

      Move_Cursor_To (COl2_RSide_X, Navigation_Y + 4);
      Put ("│");

      Move_Cursor_To (Navigation_X, Navigation_Y + 4);
      Put ("│ Longitude: ");

      Move_Cursor_To (COl2_RSide_X, Navigation_Y + 5);
      Put ("│");

      Move_Cursor_To (Navigation_X, Navigation_Y + 5);
      Put ("│ Heading:   ");

      Move_Cursor_To (Navigation_X, Navigation_Y + 6);
      Put_Line ("╰───────────────────────────╯");
      pragma Style_Checks (On);
   end Put_Navigation_Data_Static;

   ---------------------------------
   -- Put_Navigation_Data_Dynamic --
   ---------------------------------

   procedure Put_Navigation_Data_Dynamic (Force_Refresh : Boolean := False)
   is
      Position : constant Navigation_Subsystem.Coordinate :=
        Navigation_Subsystem.Navigation_Data.Get_Position;

      Position_Changed : Boolean;
      Heading_Changed  : Boolean;
   begin
      Move_Cursor_To (Navigation_X + 14, Navigation_Y + 2);
      Put_Task_Status
        (Navigation_Subsystem.Navigation_Task_Control, Force_Refresh);

      --  Update the screen if the position has changed
      Navigation_Subsystem.Navigation_Data.Get_Position_Changed
        (Position_Changed);
      if Position_Changed or Force_Refresh then
         Move_Cursor_To (Navigation_X + 13, Navigation_Y + 3);
         Put (Position.Lat'Image);

         Move_Cursor_To (Navigation_X + 13, Navigation_Y + 4);
         Put (Position.Long'Image);
      end if;

      --  Update the screen if the heading has changed
      Navigation_Subsystem.Navigation_Data.Get_Heading_Changed
        (Heading_Changed);
      if Heading_Changed or Force_Refresh then
         Move_Cursor_To (Navigation_X + 13, Navigation_Y + 5);
         Put (Navigation_Subsystem.Navigation_Data.Get_Heading'Image);
         Put ("°");
      end if;

   end Put_Navigation_Data_Dynamic;

   -------------------------------
   -- Put_Targeting_Data_Static --
   -------------------------------

   procedure Put_Targeting_Data_Static is
   begin
      pragma Style_Checks (Off);
      Put_Subsystem_Name ("Targeting", Targeting_X, Targeting_Y);

      Move_Cursor_To (COl1_RSide_X, Targeting_Y + 2);
      Put ("│");

      Move_Cursor_To (Targeting_X, Targeting_Y + 2);
      Put ("│ Status:  ");

      Move_Cursor_To (COl1_RSide_X, Targeting_Y + 3);
      Put ("│");

      Move_Cursor_To (Targeting_X, Targeting_Y + 3);
      Put ("│ Targets: ");

      Move_Cursor_To (Targeting_X, Targeting_Y + 4);
      Put_Line ("╰───────────────────────────╯");
      pragma Style_Checks (On);
   end Put_Targeting_Data_Static;

   --------------------------------
   -- Put_Targeting_Data_Dynamic --
   --------------------------------

   procedure Put_Targeting_Data_Dynamic (Force_Refresh : Boolean := False)
   is
      Targets_Changed : Boolean;
   begin
      Move_Cursor_To (Targeting_X + 11, Targeting_Y + 2);
      Put_Task_Status
        (Targeting_Subsystem.Targeting_Task_Control, Force_Refresh);

      --  Update the screen if the number of targets has changes
      Targeting_Subsystem.Targeting_Data.Get_Targets_Changed (Targets_Changed);
      if Targets_Changed or Force_Refresh then
         Move_Cursor_To (Targeting_X + 10, Targeting_Y + 3);
         Put (Targeting_Subsystem.Targeting_Data.Get_Targets'Length'Image);
      end if;

   end Put_Targeting_Data_Dynamic;

   ---------------------
   -- Put_Stores_Data --
   ---------------------

   procedure Put_Stores_Data_Static is
      Offset : Natural := 3;
   begin
      pragma Style_Checks (Off);
      Put_Subsystem_Name ("Stores", Stores_X, Stores_Y);

      Move_Cursor_To (COl1_RSide_X, Stores_Y + 2);
      Put ("│");

      Move_Cursor_To (Stores_X, Stores_Y + 2);
      Put ("│ Status:  ");

      for I in Stores_Subsystem.Light_Index loop
         Move_Cursor_To (Stores_X, Stores_Y + Offset);
         Put ("│ Light");
         Put (I'Image);
         Put (": ");
         Move_Cursor_To (COl1_RSide_X, Stores_Y + Offset);
         Put ("│");
         Offset := Offset + 1;
      end loop;

      for I in Stores_Subsystem.Heavy_Index loop
         Move_Cursor_To (Stores_X, Stores_Y + Offset);
         Put ("│ Heavy");
         Put (I'Image);
         Put (": ");
         Move_Cursor_To (COl1_RSide_X, Stores_Y + Offset);
         Put ("│");
         Offset := Offset + 1;
      end loop;


      Move_Cursor_To (Stores_X, Stores_Y + 7);
      Put_Line ("╰───────────────────────────╯");
      pragma Style_Checks (On);
   end Put_Stores_Data_Static;

   -----------------------------
   -- Put_Stores_Data_Dynamic --
   -----------------------------

   procedure Put_Stores_Data_Dynamic (Force_Refresh : Boolean := False)
   is
      Offset : Natural := 3;
   begin
      Move_Cursor_To (Stores_X + 11, Stores_Y + 2);
      Put_Task_Status
        (Stores_Subsystem.Stores_Task_Control, Force_Refresh);

      --  Update the screen each time a light store changes
      for I in Stores_Subsystem.Light_Index loop
         declare
            Light_Stores_Changed : Boolean;
         begin
            Stores_Subsystem.Stores_Data.Get_Light_Status_Changed
              (I, Light_Stores_Changed);
            if Light_Stores_Changed or Force_Refresh then
               Move_Cursor_To (Stores_X + 11, Stores_Y + Offset);
               Put ("               ");
               Move_Cursor_To (Stores_X + 11, Stores_Y + Offset);
               Put_Stores_Status
                 (Stores_Subsystem.Stores_Data.Get_Light_Status (I));
            end if;
         end;
         Offset := Offset + 1;
      end loop;

      --  Update the screen each time a heavy store changes
      for I in Stores_Subsystem.Heavy_Index loop
         declare
            Heavy_Stores_Changed : Boolean;
         begin
            Stores_Subsystem.Stores_Data.Get_Heavy_Status_Changed
              (I, Heavy_Stores_Changed);
            if Heavy_Stores_Changed or Force_Refresh then
               Move_Cursor_To (Stores_X + 11, Stores_Y + Offset);
               Put ("               ");
               Move_Cursor_To (Stores_X + 11, Stores_Y + Offset);
               Put_Stores_Status
                 (Stores_Subsystem.Stores_Data.Get_Heavy_Status (I));
            end if;
         end;
         Offset := Offset + 1;
      end loop;
   end Put_Stores_Data_Dynamic;

   --------------------------
   -- Put_Built_In_Test_Data_Static --
   --------------------------

   procedure Put_Built_In_Test_Data_Static is
   begin
      pragma Style_Checks (Off);
      Put_Subsystem_Name ("Built In Test", Built_In_Test_X, Built_In_Test_Y);

      Move_Cursor_To (Built_In_Test_RSide_X, Built_In_Test_Y + 2);
      Put ("│");

      Move_Cursor_To (Built_In_Test_X, Built_In_Test_Y + 2);
      Put ("│  PBIT Status: ");

      Move_Cursor_To (Built_In_Test_RSide_X, Built_In_Test_Y + 3);
      Put ("│");

      Move_Cursor_To (Built_In_Test_X, Built_In_Test_Y + 3);
      Put ("│  CBIT Status: ");

      Move_Cursor_To (Built_In_Test_X, Built_In_Test_Y + 4);
      Put_Line ("╰───────────────────────────╯");
      pragma Style_Checks (On);
   end Put_Built_In_Test_Data_Static;

   ---------------------------
   -- Put_Built_In_Test_Data_Dynamic --
   ---------------------------

   procedure Put_Built_In_Test_Data_Dynamic (Force_Refresh : Boolean := False)
   is
      PBIT_Changed : Boolean;
      CBIT_Changed : Boolean;
   begin

      --  Update the screen if PBIT has changed
      Built_In_Test_Subsystem.Get_PBIT_Status_Changed
        (PBIT_Changed);
      if PBIT_Changed or Force_Refresh then
         Move_Cursor_To (Built_In_Test_X + 16, Built_In_Test_Y + 2);
         Put ("           ");
         Move_Cursor_To (Built_In_Test_X + 16, Built_In_Test_Y + 2);
         Put_BIT_Status
           (Built_In_Test_Subsystem.Get_PBIT_Status);
      end if;

      --  Update the screen if CBIT has changed
      Built_In_Test_Subsystem.Get_CBIT_Status_Changed
        (CBIT_Changed);
      if CBIT_Changed or Force_Refresh then
         Move_Cursor_To (Built_In_Test_X + 16, Built_In_Test_Y + 3);
         Put ("           ");
         Move_Cursor_To (Built_In_Test_X + 16, Built_In_Test_Y + 3);
         Put_BIT_Status
           (Built_In_Test_Subsystem.Get_CBIT_Status);
      end if;

   end Put_Built_In_Test_Data_Dynamic;

   --------------------------------------------
   -- Put_Flight_Recorder_Information_Static --
   --------------------------------------------

   procedure Put_Flight_Recorder_Information_Static is
      Y_Cursor_Offset : Integer := 0;
   begin
      pragma Style_Checks (Off);

      --  Add flight recorder header
      Move_Cursor_To (38, Flight_Recorder_Y);
      Put ("╭───────────────╮");
      Move_Cursor_To (Flight_Recorder_X, Flight_Recorder_Y + 1);
      Put ("╭─────────────────────────────────────╯");
      Set_Text_Colour (Cyan);
      Put("Flight Recorder");
      Set_Text_Colour (Default);
      Put ("╰────────────────────────────────────╮");
      Move_Cursor_To (Flight_Recorder_X, Flight_Recorder_Y + 2);
      Put ("│");
      Move_Cursor_To (Flight_Recorder_RSide_X, Flight_Recorder_Y + 2);
      Put ("│");

      --  Add the CBIT header
      Move_Cursor_To (Flight_Recorder_CBIT_X, Flight_Recorder_CBIT_Y);
      Put ("│ Continuous Built In Test (CBIT) ");
      Move_Cursor_To (Flight_Recorder_RSide_X, Flight_Recorder_CBIT_Y);
      Put ("│");
      Move_Cursor_To (Flight_Recorder_X, Flight_Recorder_CBIT_Y + 1);
      Put ("│");
      Move_Cursor_To (Flight_Recorder_RSide_X, Flight_Recorder_CBIT_Y + 1);
      Put ("│");

      --  Add the overall CBIT status
      Move_Cursor_To (Flight_Recorder_CBIT_X, Flight_Recorder_CBIT_Y + 2);
      Put ("│  ⦿  Overall status  : ");
      Move_Cursor_To (Flight_Recorder_RSide_X, Flight_Recorder_CBIT_Y + 2);
      Put ("│");
      Move_Cursor_To (Flight_Recorder_X, Flight_Recorder_CBIT_Y + 3);
      Put ("│");
      Move_Cursor_To (Flight_Recorder_RSide_X, Flight_Recorder_CBIT_Y + 3);
      Put ("│");

      --  Add individual CBIT status
      Y_Cursor_Offset := Y_Cursor_Offset + 4;
      for Avionics_Subsystem in Subsystem_Name_Type loop
         Move_Cursor_To
           (Flight_Recorder_CBIT_X, Flight_Recorder_CBIT_Y + Y_Cursor_Offset);
         Put ("│  ⦾ [" & Avionics_Subsystem'Image);
         Move_Cursor_To
           (Flight_Recorder_CBIT_X + 20,
            Flight_Recorder_CBIT_Y + Y_Cursor_Offset);
         Put ("] : ");
         Put_BIT_Status
           (Built_In_Test_Subsystem.Get_CBIT_Status
              (Avionics_Subsystem));
         Move_Cursor_To
           (Flight_Recorder_RSide_X, Flight_Recorder_CBIT_Y + Y_Cursor_Offset);
         Put ("│");
         Y_Cursor_Offset := Integer'Succ (Y_Cursor_Offset);
      end loop;

      --  Add the PBIT header
      Y_Cursor_Offset := 0;
      Move_Cursor_To (Flight_Recorder_PBIT_X, Flight_Recorder_PBIT_Y);
      Put (" Powerup Built In Test (PBIT) ");

      --  Add the overall PBIT status
      Move_Cursor_To (Flight_Recorder_PBIT_X, Flight_Recorder_PBIT_Y + 2);
      Put ("  ⦿  Overall status   : ");

      --  Add individual PBIT status
      Y_Cursor_Offset := Y_Cursor_Offset + 4;
      for Avionics_Subsystem in Subsystem_Name_Type loop
         Move_Cursor_To
           (Flight_Recorder_PBIT_X, Flight_Recorder_PBIT_Y + Y_Cursor_Offset);
         Put ("  ⦾ [" & Avionics_Subsystem'Image);
         Move_Cursor_To
           (Flight_Recorder_PBIT_X + 20,
            Flight_Recorder_PBIT_Y + Y_Cursor_Offset);
         Put ("] : ");
         Put_BIT_Status
           (Built_In_Test_Subsystem.Get_PBIT_Status
              (Avionics_Subsystem));
         Y_Cursor_Offset := Integer'Succ (Y_Cursor_Offset);
      end loop;

      -- Add a blank line
      Move_Cursor_To
        (Flight_Recorder_X, Flight_Recorder_CBIT_Y + Y_Cursor_Offset);
      Put ("│");
      Move_Cursor_To
        (Flight_Recorder_RSide_X,
         Flight_Recorder_CBIT_Y + Y_Cursor_Offset);
      Put ("│");

      --  Add the historic CBIT status header
      Move_Cursor_To (Flight_Recorder_His_CBIT_X, Flight_Recorder_His_CBIT_Y);
      Put ("│  ⦿ Historic BIT Failures ");
      Move_Cursor_To
        (Flight_Recorder_RSide_X, Flight_Recorder_His_CBIT_Y);
      Put ("│");
      Move_Cursor_To (Flight_Recorder_His_CBIT_X,
                      Flight_Recorder_His_CBIT_Y + 1);
      Put ("│");
      Move_Cursor_To
        (Flight_Recorder_RSide_X, Flight_Recorder_His_CBIT_Y + 1);
      Put ("│");

      pragma Style_Checks (On);
   end Put_Flight_Recorder_Information_Static;

   ---------------------------------------------
   -- Put_Flight_Recorder_Information_Dynamic --
   ---------------------------------------------

   procedure Put_Flight_Recorder_Information_Dynamic
     (Force_Refresh : Boolean := False)
   is
      Overall_PBIT_Changed : Boolean;
      Overall_CBIT_Changed : Boolean;
      Y_Cursor_Offset : Integer := 0;
   begin
      pragma Style_Checks (Off);

      --  Update the screen if the overall CBIT status has changed
      Built_In_Test_Subsystem.Get_CBIT_Status_Changed
        (Overall_CBIT_Changed);
      if Overall_CBIT_Changed or Force_Refresh then
         Move_Cursor_To (Flight_Recorder_CBIT_X + 24,
                         Flight_Recorder_CBIT_Y + 2);
         Put ("           ");
         Move_Cursor_To (Flight_Recorder_CBIT_X + 24,
                         Flight_Recorder_CBIT_Y + 2);
         Put_BIT_Status
           (Built_In_Test_Subsystem.Get_CBIT_Status);
      end if;

      --  Add individual CBIT status
      Y_Cursor_Offset := Y_Cursor_Offset + 4;
      for Avionics_Subsystem in Subsystem_Name_Type loop
         declare
            CBIT_Changed : Boolean;
         begin
            Built_In_Test_Subsystem.
              Get_CBIT_Status_Changed (Avionics_Subsystem, CBIT_Changed);
            if CBIT_Changed or Force_Refresh then
               Move_Cursor_To
                 (Flight_Recorder_CBIT_X + 24,
                  Flight_Recorder_CBIT_Y + Y_Cursor_Offset);
               Put ("             ");
               Move_Cursor_To
                 (Flight_Recorder_CBIT_X + 24,
                  Flight_Recorder_CBIT_Y + Y_Cursor_Offset);
               Put_BIT_Status
                 (Built_In_Test_Subsystem.Get_CBIT_Status
                    (Avionics_Subsystem));
            end if;
         end;
         Y_Cursor_Offset := Integer'Succ (Y_Cursor_Offset);
      end loop;

      --  Update the screen if the overall PBIT status has changed
      Y_Cursor_Offset := 0;
      Built_In_Test_Subsystem.Get_PBIT_Status_Changed
        (Overall_PBIT_Changed);
      if Overall_PBIT_Changed or Force_Refresh then
         Move_Cursor_To
           (Flight_Recorder_PBIT_X + 24, Flight_Recorder_PBIT_Y + 2);
         Put ("           ");
         Move_Cursor_To
           (Flight_Recorder_PBIT_X + 24, Flight_Recorder_PBIT_Y + 2);
         Put_BIT_Status
           (Built_In_Test_Subsystem.Get_PBIT_Status);
      end if;

      --  Add individual PBIT status
      Y_Cursor_Offset := Y_Cursor_Offset + 4;
      for Avionics_Subsystem in Subsystem_Name_Type loop
         declare
            PBIT_Changed : Boolean;
         begin
            Built_In_Test_Subsystem.
              Get_PBIT_Status_Changed (Avionics_Subsystem, PBIT_Changed);
            if PBIT_Changed or Force_Refresh then
               Move_Cursor_To
                 (Flight_Recorder_PBIT_X + 24,
                  Flight_Recorder_PBIT_Y + Y_Cursor_Offset);
               Put ("             ");
               Move_Cursor_To
                 (Flight_Recorder_PBIT_X + 24,
                  Flight_Recorder_PBIT_Y + Y_Cursor_Offset);
               Put_BIT_Status
                 (Built_In_Test_Subsystem.Get_PBIT_Status
                    (Avionics_Subsystem));
            end if;
         end;
         Y_Cursor_Offset := Integer'Succ (Y_Cursor_Offset);
      end loop;

      --  Add the individual historic entries
      declare
         BIT_Historic_Report : constant Historic_BIT_Entries_Data :=
           Built_In_Test_Subsystem.
             Get_BIT_Historic_Report;
         New_Historic_Entries : Boolean;
      begin

         --  Update the screen if the entries have changed
         Built_In_Test_Subsystem.
           Get_BIT_Historic_Report_Changed (New_Historic_Entries);
         if New_Historic_Entries or Force_Refresh then

            --  Check if we have any
            if BIT_Historic_Report.Number_Of_Entries = 0 then
               Move_Cursor_To (Flight_Recorder_His_CBIT_X,
                               Flight_Recorder_His_CBIT_Y + 2);
               Put ("│  ⦾ N/A ");
               Move_Cursor_To
                 (Flight_Recorder_RSide_X, Flight_Recorder_His_CBIT_Y + 2);
               Put ("│");
               Move_Cursor_To (Flight_Recorder_X, Flight_Recorder_His_CBIT_Y + 3);
               Put ("╰──────────────────────────────────────────────────────────────────────────────────────────╯");

            else

               --  We have a least one historic failure therefore list them
               Y_Cursor_Offset := 2;

               for j in 0 .. BIT_Historic_Report.Number_Of_Entries - 1 loop
                  Move_Cursor_To
                    (Flight_Recorder_X, Flight_Recorder_His_CBIT_Y +
                       Y_Cursor_Offset);
                  Put ("                                                                                            ");

                  Move_Cursor_To
                    (Flight_Recorder_X, Flight_Recorder_His_CBIT_Y +
                       Y_Cursor_Offset);

                  --  Print the name of the subsystem
                  Put ("│     ⦾ [" & BIT_Historic_Report.Entries
                       (Historic_BIT_Entries_Index (j)).Subsystem'Image & "]");

                  --  Calculate and print the timestamp
                  declare
                     Time_Span : constant Ada.Real_Time.Time_Span :=
                       BIT_Historic_Report.Entries
                         (Historic_BIT_Entries_Index (j)).Time_Stamp - Start_Time;

                     Time_Stamp_Duration : constant Duration :=
                       Ada.Real_Time.To_Duration (Time_Span);
                  begin
                     Put (" [" & Time_Stamp_Duration'Image & "]");
                  end;

                  --  Print the failure text

                  Put (" [" & To_String (BIT_Historic_Report.Entries
                       (Historic_BIT_Entries_Index (j)).Failure_Text) & "]");
                  Move_Cursor_To
                    (Flight_Recorder_RSide_X, Flight_Recorder_His_CBIT_Y +
                       Y_Cursor_Offset);
                  Put ("│");
                  Move_Cursor_To
                    (Flight_Recorder_RSide_X, Flight_Recorder_His_CBIT_Y +
                       Y_Cursor_Offset);
                  Put ("│");

                  Y_Cursor_Offset := Y_Cursor_Offset + 1;
               end loop;

               Move_Cursor_To (Flight_Recorder_X,
                               Flight_Recorder_His_CBIT_Y + Y_Cursor_Offset);
               Put ("╰──────────────────────────────────────────────────────────────────────────────────────────╯");
            end if;
         end if;
      end;
      pragma Style_Checks (On);
   end Put_Flight_Recorder_Information_Dynamic;

   ------------------------------
   -- Put_Countermeasures_Data --
   ------------------------------

   procedure Put_Countermeasures_Data_Static is
   begin
      pragma Style_Checks (Off);
      Put_Subsystem_Name
        ("Cyber-Countermeasures", Countermeasures_X, Countermeasures_Y, 29);

      Move_Cursor_To (Countermeasures_X, Countermeasures_Y + 2);
      Put ("│ Status:");

      Move_Cursor_To (Countermeasures_RSide_X, Countermeasures_Y + 2);
      Put ("│");

      Move_Cursor_To (Countermeasures_X, Countermeasures_Y + 3);
      Put ("│ Countermeasures:");

      Move_Cursor_To (Countermeasures_RSide_X, Countermeasures_Y + 3);
      Put ("│");

      Move_Cursor_To (Countermeasures_X, Countermeasures_Y + 4);
      Put_Line ("╰─────────────────────────────╯");
      pragma Style_Checks (On);
   end Put_Countermeasures_Data_Static;

   -------------------------------------
   -- Put_Countermeasures_Data_Dynamc --
   -------------------------------------

   procedure Put_Countermeasures_Data_Dynamic
     (Force_Refresh : Boolean := False)
   is
      Countermeasure_Status_Change : Boolean;
   begin
      Move_Cursor_To (Countermeasures_X + 19, Countermeasures_Y + 2);
      Put_Task_Status
        (Countermeasures_Subsystem.Countermeasures_Task_Control,
         Force_Refresh);

      Countermeasures_Subsystem.Countermeasures_Control.Get_Status_Change
        (Countermeasure_Status_Change);
      if Countermeasure_Status_Change or Force_Refresh then
         Move_Cursor_To (Countermeasures_X + 19, Countermeasures_Y + 3);
         Put ("          ");
         Move_Cursor_To (Countermeasures_X + 19, Countermeasures_Y + 3);
         Put_Countermeasure_Status
           (Countermeasures_Subsystem.Countermeasures_Control.Get_Status);
      end if;
   end Put_Countermeasures_Data_Dynamic;

   ------------------
   -- Put_Commands --
   ------------------

   procedure Put_Commands is
   begin

      if Display_Control_Data.Get_Requested_Display = Main_Avionics_Screen then
         Move_Cursor_To
           (Commands_Main_Screen_X_Left,
            Commands_Main_Screen_Y_Left);

         Put_Line ("Key commands:");
         Put_Line (" 🅰 ーSend valid radar packet");
         Put_Line (" 🅲 ーSend truncated radar packet");
         Put_Line (" 🅳 ーDeploy cyber countermeasures");
         Put_Line (" 🅷 ーLaunch heavy armament");

         Move_Cursor_To
           (Commands_Main_Screen_X_Right,
            Commands_Main_Screen_Y_Right);

         Put_Line (" 🅻 ーLaunch light armament");

         Move_Cursor_To
           (Commands_Main_Screen_X_Right,
            Commands_Main_Screen_Y_Right + 1);

         Put_Line (" 🅵 ーDisplay flight recorder information");

         Move_Cursor_To
           (Commands_Main_Screen_X_Right,
            Commands_Main_Screen_Y_Right + 2);

         Put_Line (" 🅾 ーIncrease Refresh 🅿 ーDecrease Refresh");

         Move_Cursor_To
           (Commands_Main_Screen_X_Right,
            Commands_Main_Screen_Y_Right + 3);

         Put_Line (" 🆂 ーShow splash screen");

      else
         Move_Cursor_To
           (Commands_Flight_Recorder_Screen_X,
            Commands_Flight_Recorder_Screen_Y);

         Put_Line ("Key commands:");
         Put_Line (" 🅼 ーDisplay main avionics screen");
      end if;
   end Put_Commands;

   ---------------------
   -- Put_Task_Status --
   ---------------------

   procedure Put_Task_Status (EXB : in out Monitored_Tasking.Task_Control;
                              Force_Refresh : Boolean)
   is
      Status         : Task_Status_Kind;
      Status_Changed : Boolean;
   begin
      EXB.Status (Status, Status_Changed);
      if Status_Changed or Force_Refresh then
         Set_Text_Colour (Task_Status_Colour (Status));
         Put (Status'Image);
         for j in Status'Image'Length .. 10 loop
            Put (" ");
         end loop;
         Set_Text_Colour (Default);
      end if;
   end Put_Task_Status;

   ------------------------
   -- Put_Subsystem_Name --
   ------------------------

   procedure Put_Subsystem_Name
     (Name : String; X : Natural; Y : Natural; Width : Positive := 27)
   is
      Title_Width     : constant Positive := Name'Length;
      Padding_Needed  : constant Positive := Width - Title_Width;
      LPadding_Needed : constant Positive := (Padding_Needed / 2) - 1;
      RPadding_Needed : constant Positive := ((Padding_Needed + 1) / 2) - 1;
   begin

      --  Put the hat
      Move_Cursor_To (X + LPadding_Needed + 1, Y);
      Put ("╭");
      for j in 1 .. Title_Width loop
         Put ("─");
      end loop;
      Put ("╮");

      --  Put the subsystem title
      Move_Cursor_To (X, Y + 1);
      Put ("╭");
      for j in 1 .. LPadding_Needed loop
         Put ("─");
      end loop;
      Put ("╯");
      Set_Text_Colour (Cyan);
      Put (Name);
      Set_Text_Colour (Default);
      Put ("╰");
      for j in 1 .. RPadding_Needed loop
         Put ("─");
      end loop;
      Put ("╮");
   end Put_Subsystem_Name;

   ---------------------
   -- Put_BIT_Status --
   ---------------------

   procedure Put_BIT_Status (Status : Monitored_Tasking.BIT_Pass_Fail_Type) is
   begin
      Set_Text_Colour (BIT_Status_Colour (Status));
      Put (Status'Image);
      Set_Text_Colour (Default);
   end Put_BIT_Status;

   -------------------------------
   -- Put_Countermeasure_Status --
   -------------------------------

   procedure Put_Countermeasure_Status
     (Status : Countermeasures_Subsystem.Status_Kind)
   is
   begin
      Set_Text_Colour (Countermeasures_Status_Colour (Status));
      Put (Status'Image);
      Set_Text_Colour (Default);
   end Put_Countermeasure_Status;

   -----------------------
   -- Put_Stores_Status --
   -----------------------

   procedure Put_Stores_Status (Status : Stores_Subsystem.Status_Kind) is
   begin
      Set_Text_Colour (Stores_Status_Colour (Status));
      Put (Status'Image);
      Set_Text_Colour (Default);
   end Put_Stores_Status;

   ------------------
   -- Display_Task --
   ------------------

   task body Display_Task is
      Next_Time : Time := Clock;
      IGNORE_Continue : Character;

      -----------------------------------------
      -- Display_Static_Main_Screen_Elements --
      -----------------------------------------

      procedure Display_Static_Main_Screen_Elements is
      begin
         Clear;
         Put_AdaCore_Logo;
         Put_Countermeasures_Data_Static;
         Put_Radar_Data_Static;
         Put_Flight_Data_Static;
         Put_Fuel_Data_Static;
         Put_Navigation_Data_Static;
         Put_Targeting_Data_Static;
         Put_Stores_Data_Static;
         Put_Built_In_Test_Data_Static;
         Put_Commands;
         Ada.Text_IO.New_Line;
      end Display_Static_Main_Screen_Elements;

      ----------------------------------------------------
      -- Display_Static_Flight_Recorder_Screen_Elements --
      ----------------------------------------------------

      procedure Display_Static_Flight_Recorder_Screen_Elements is
      begin
         Clear;
         Put_Flight_Recorder_Information_Static;
         Put_Commands;
         Ada.Text_IO.New_Line;
      end Display_Static_Flight_Recorder_Screen_Elements;

   begin

      --  Switch off the cursor and clear the screen
      Hide_Cursor;
      Clear;

      loop

         declare
            --  Check if we need to switch displays
            Page_Switch : constant Boolean :=
              Display_Control_Data.Get_Requested_Display /=
                Display_Control_Data.Get_Current_Display;
         begin

            if Page_Switch then
               Clear;
               Display_Control_Data.Set_Current_Display
                 (Display_Control_Data.Get_Requested_Display);

               case Display_Control_Data.Get_Current_Display is
                  when Main_Avionics_Screen =>

                     --  Display the static elements of the main screen
                     Display_Static_Main_Screen_Elements;

                  when Flight_Recorder_Screen =>
                     --  Display the static elements of the flight recorder
                     --  screen
                     Display_Static_Flight_Recorder_Screen_Elements;

                  when Main_Splash_Screen =>
                     --  Display the splash screen
                     Splash_Screen.Display_Splash_Screen;
               end case;
            end if;

            --  Now display the dynamic elements
            case Display_Control_Data.Get_Current_Display is
               when Main_Avionics_Screen =>

                  --  Now update the dynamic elements
                  Put_Countermeasures_Data_Dynamic (Page_Switch);
                  Put_Radar_Data_Dynamic (Page_Switch);
                  Put_Flight_Data_Dynamic (Page_Switch);
                  Put_Fuel_Data_Dynamic (Page_Switch);
                  Put_Navigation_Data_Dynamic (Page_Switch);
                  Put_Targeting_Data_Dynamic (Page_Switch);
                  Put_Stores_Data_Dynamic (Page_Switch);
                  Put_Built_In_Test_Data_Dynamic (Page_Switch);
                  Ada.Text_IO.New_Line;

               when Flight_Recorder_Screen =>

                  --  Now update the dynamic elements
                  Put_Flight_Recorder_Information_Dynamic (Page_Switch);

               when Main_Splash_Screen =>

                  --  Animate the splash screen
                  Splash_Screen.Display_Dynamic_Splash_Screen;
            end case;
         end;

         Next_Time := Next_Time + Update_Period;
         delay until Next_Time;
      end loop;
   end Display_Task;

   --------------------------
   -- Display_Control_Data --
   --------------------------

   protected body Display_Control_Data is

      -------------------------------
      -- Show_Main_Avionics_Screen --
      -------------------------------

      procedure Show_Main_Avioics_Screen is
      begin
         Requested_Display := Main_Avionics_Screen;
      end Show_Main_Avioics_Screen;

      ---------------------------------
      -- Show_Flight_Recorder_Screen --
      ---------------------------------

      procedure Show_Flight_Recorder_Screen is
      begin
         Requested_Display := Flight_Recorder_Screen;
      end Show_Flight_Recorder_Screen;

      ------------------------
      -- Show_Splash_Screen --
      ------------------------

      procedure Show_Splash_Screen is
      begin
         Requested_Display := Main_Splash_Screen;
      end Show_Splash_Screen;

      ---------------------------
      -- Get_Requested_Display --
      ---------------------------

      function Get_Requested_Display return Requested_Display_Type is
         (Requested_Display);

      -------------------------
      -- Set_Current_Display --
      -------------------------

      procedure Set_Current_Display (Display : Requested_Display_Type) is
      begin
         Current_Display := Display;
      end Set_Current_Display;

      -------------------------
      -- Get_Current_Display --
      -------------------------

      function Get_Current_Display return Requested_Display_Type is
         (Current_Display);

   end Display_Control_Data;

end Display;
