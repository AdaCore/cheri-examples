--
--  Copyright (C) 2023, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--
with Ada.Characters.Latin_1; use Ada.Characters.Latin_1;
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
with Integrated_Monitoring_And_Recording_Subsystem;
use Integrated_Monitoring_And_Recording_Subsystem;

package body Display is

   Update_Period : constant Time_Span := Milliseconds (500);

   ------------------------------
   -- Show_Main_Avioics_Screen --
   ------------------------------

   procedure Show_Main_Avioics_Screen is
   begin
      Display_Control_Data.Show_Main_Avioics_Screen;
   end Show_Main_Avioics_Screen;

   ---------------------------------
   -- Show_Flight_Recorder_Screen --
   ---------------------------------

   procedure Show_Flight_Recorder_Screen is
   begin
      Display_Control_Data.Show_Flight_Recorder_Screen;
   end Show_Flight_Recorder_Screen;

   procedure Clear;
   --  Clear the terminal

   procedure Home;
   --  Move the cursor to the home position (top left)

   procedure Move_Cursor_To (X, Y : Natural);
   --  Move the cursor to a position on the screen. (0, 0) is top left

   type Text_Colour is
     (Default, Green, Red, Yellow, Blue, Magenta, Cyan, White);

   procedure Set_Text_Colour (Colour : Text_Colour);
   --  Set the colour of the text

   Task_Status_Colour : constant
     array (Monitored_Tasking.Task_Status_Kind)
     of Text_Colour :=
       (PBIT => Yellow,
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

   --  Subprograms for printing different parts of the UI:

   procedure Put_Radar_Data;
   procedure Put_Flight_Data;
   procedure Put_Fuel_Data;
   procedure Put_Navigation_Data;
   procedure Put_Targeting_Data;
   procedure Put_Stores_Data;
   procedure Put_IMRS_Data;
   procedure Put_Countermeasures_Data;
   procedure Put_Commands;
   procedure Put_Flight_Recorder_Information;

   procedure Put_Task_Status (EXB : in out Monitored_Tasking.Task_Control);

   procedure Put_BIT_Status (Status : Monitored_Tasking.BIT_Pass_Fail_Type);

   procedure Put_Subsystem_Name (Name : String; X : Natural; Y : Natural);

   --  procedure Put_Task_Exception
   --    (EXB : in out Monitored_Tasking.Task_Control);

   procedure Put_Countermeasure_Status
     (Status : Countermeasures_Subsystem.Status_Kind);

   procedure Put_Stores_Status (Status : Stores_Subsystem.Status_Kind);

   ---------------
   -- UI Layout --
   ---------------

   --  Screen coordinates of the UI elements. (0, 0) is the top-left.

   Countermeasures_X       : constant := 27;
   Countermeasures_Y       : constant := 0;
   Countermeasures_RSide_X : constant := 55;

   COl1_RSide_X : constant := 28;
   COl2_RSide_X : constant := 82;

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

   IMRS_X       : constant := 27;
   IMRS_Y       : constant := 22;
   IMRS_RSide_X : constant := 55;

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

   -----------
   -- Clear --
   -----------

   procedure Clear is
   begin
      Put (ESC & "[2J");
   end Clear;

   ----------
   -- Home --
   ----------

   procedure Home is
   begin
      Put (ESC & "[H");
   end Home;

   --------------------
   -- Move_Cursor_To --
   --------------------

   procedure Move_Cursor_To (X, Y : Natural) is
      X_Str : constant String := X'Image;
      Y_Str : constant String := Y'Image;
   begin
      Home;

      if X > 0 then
         Put (ESC & "[" & X_Str (2 .. X_Str'Last) & "C");
      end if;

      if Y > 0 then
         Put (ESC & "[" & Y_Str (2 .. Y_Str'Last) & "B");
      end if;
   end Move_Cursor_To;

   ---------------------
   -- Set_Text_Colour --
   ---------------------

   procedure Set_Text_Colour (Colour : Text_Colour) is
   begin

      case Colour is
         when Default =>
            Put (ESC & "[0m");

         when Red =>
            Put (ESC & "[31m");

         when Green =>
            Put (ESC & "[32m");

         when Yellow =>
            Put (ESC & "[33m");

         when Blue =>
            Put (ESC & "[34m");

         when Magenta =>
            Put (ESC & "[35m");

         when Cyan =>
            Put (ESC & "[36m");

         when White =>
            Put (ESC & "[37m");

      end case;
   end Set_Text_Colour;

   --------------------
   -- Put_Radar_Data --
   --------------------

   procedure Put_Radar_Data is
   begin
      pragma Style_Checks (Off);
      Put_Subsystem_Name ("Radar", Radar_X, Radar_Y);
      Move_Cursor_To (Radar_X, Radar_Y + 2);
      Put ("│ Status: ");
      Put_Task_Status (Radar_Subsystem.Radar_Task_Control);
      Move_Cursor_To (COl1_RSide_X, Radar_Y + 2);
      Put ("│");


      Move_Cursor_To (Radar_X, Radar_Y + 3);
      Put ("│ Radar:  ");
      if Radar_Subsystem.Radar_Data.Is_Operational then
         Put ("On");
      else
         Put ("Off");
      end if;
      Move_Cursor_To (COl1_RSide_X, Radar_Y + 3);
      Put ("│");

      Move_Cursor_To (Radar_X, Radar_Y + 4);
      Put ("│ Tracks:");
      Put (Radar_Subsystem.Radar_Data.Tracks'Length'Image);
      Move_Cursor_To (COl1_RSide_X, Radar_Y + 4);
      Put ("│");

      Move_Cursor_To (Radar_X, Radar_Y + 4);
      Put_Line ("╰───────────────────────────╯");
      pragma Style_Checks (On);
   end Put_Radar_Data;

   ---------------------
   -- Put_Flight_Data --
   ---------------------

   procedure Put_Flight_Data is
   begin
      pragma Style_Checks (Off);
      Put_Subsystem_Name ("Flight Data", Flight_X, Flight_Y);

      Move_Cursor_To (Flight_X, Flight_Y + 2);
      Put ("│ Status: ");
      Put_Task_Status (Flight_Subsystem.Flight_Task_Control);
      Move_Cursor_To (COl2_RSide_X, Flight_Y + 2);
      Put ("│");


      Move_Cursor_To (Flight_X, Flight_Y + 3);
      Put ("│ Altitude:");
      Put (Flight_Subsystem.Flight_Data.Get_Altitude'Image);
      Put (" ft");
      Move_Cursor_To (COl2_RSide_X, Flight_Y + 3);
      Put ("│");

      Move_Cursor_To (Flight_X, Flight_Y + 4);
      Put ("│ Airspeed:");
      Put (Flight_Subsystem.Flight_Data.Get_Airspeed'Image);
      Put (" KIAS");
      Move_Cursor_To (COl2_RSide_X, Flight_Y + 4);
      Put ("│");

      Move_Cursor_To (Flight_X, Flight_Y + 5);
      Put_Line ("╰───────────────────────────╯");
      pragma Style_Checks (On);
   end Put_Flight_Data;

   -------------------
   -- Put_Fuel_Data --
   -------------------

   procedure Put_Fuel_Data is
      Bingo_Minutes : Duration;
   begin
      pragma Style_Checks (Off);
      Put_Subsystem_Name ("Fuel", Fuel_X, Fuel_Y);

      Move_Cursor_To (Fuel_X, Fuel_Y + 2);
      Put ("│ Status: ");
      Put_Task_Status (Fuel_Subsystem.Fuel_Task_Control);
      Move_Cursor_To (COl2_RSide_X, Fuel_Y + 2);
      Put ("│");

      Move_Cursor_To (Fuel_X, Fuel_Y + 3);
      Put ("│ FQI:   ");
      Put (Fuel_Subsystem.Fuel_Data.Fuel_Quantity'Image);
      Put (" kg");
      Move_Cursor_To (COl2_RSide_X, Fuel_Y + 3);
      Put ("│");


      Move_Cursor_To (Fuel_X, Fuel_Y + 4);
      Put ("│ Bingo: ");
      Bingo_Minutes := Fuel_Subsystem.Fuel_Data.Time_To_Minimum_Fuel / 60.0;
      Put (Integer (Bingo_Minutes)'Image);
      Put (" minutes");
      Move_Cursor_To (COl2_RSide_X, Fuel_Y + 4);
      Put ("│");


      Move_Cursor_To (Fuel_X, Fuel_Y + 5);
      Put_Line ("╰───────────────────────────╯");
      pragma Style_Checks (On);
   end Put_Fuel_Data;

   -------------------------
   -- Put_Navigation_Data --
   -------------------------

   procedure Put_Navigation_Data is
      Position : constant Navigation_Subsystem.Coordinate :=
                   Navigation_Subsystem.Navigation_Data.Get_Position;
   begin
      pragma Style_Checks (Off);
      Put_Subsystem_Name ("Navigation", Navigation_X, Navigation_Y);

      Move_Cursor_To (Navigation_X, Navigation_Y + 2);
      Put ("│ Status:    ");
      Put_Task_Status (Navigation_Subsystem.Navigation_Task_Control);
      Move_Cursor_To (COl2_RSide_X, Navigation_Y + 2);
      Put ("│");

      Move_Cursor_To (Navigation_X, Navigation_Y + 3);
      Put ("│ Latitude:  ");
      Put (Position.Lat'Image);
      Move_Cursor_To (COl2_RSide_X, Navigation_Y + 3);
      Put ("│");

      Move_Cursor_To (Navigation_X, Navigation_Y + 4);
      Put ("│ Longitude: ");
      Put (Position.Long'Image);
      Move_Cursor_To (COl2_RSide_X, Navigation_Y + 4);
      Put ("│");

      Move_Cursor_To (Navigation_X, Navigation_Y + 5);
      Put ("│ Heading:   ");
      Put (Navigation_Subsystem.Navigation_Data.Get_Heading'Image);
      Put ("Â°");
      Move_Cursor_To (COl2_RSide_X, Navigation_Y + 5);
      Put ("│");

      Move_Cursor_To (Navigation_X, Navigation_Y + 6);
      Put_Line ("╰───────────────────────────╯");
      pragma Style_Checks (On);
   end Put_Navigation_Data;

   ------------------------
   -- Put_Targeting_Data --
   ------------------------

   procedure Put_Targeting_Data is
   begin
      pragma Style_Checks (Off);
      Put_Subsystem_Name ("Targeting", Targeting_X, Targeting_Y);

      Move_Cursor_To (Targeting_X, Targeting_Y + 2);
      Put ("│ Status:  ");
      Put_Task_Status (Targeting_Subsystem.Targeting_Task_Control);
      Move_Cursor_To (COl1_RSide_X, Targeting_Y + 2);
      Put ("│");

      Move_Cursor_To (Targeting_X, Targeting_Y + 3);
      Put ("│ Targets: ");
      Put (Targeting_Subsystem.Targeting_Data.Get_Targets'Length'Image);
      Move_Cursor_To (COl1_RSide_X, Targeting_Y + 3);
      Put ("│");

      Move_Cursor_To (Targeting_X, Targeting_Y + 4);
      Put_Line ("╰───────────────────────────╯");
      pragma Style_Checks (On);
   end Put_Targeting_Data;

   ---------------------
   -- Put_Stores_Data --
   ---------------------

   procedure Put_Stores_Data is
      Offset : Natural;
   begin
      pragma Style_Checks (Off);
      Put_Subsystem_Name ("Stores", Stores_X, Stores_Y);

      Move_Cursor_To (Stores_X, Stores_Y + 2);
      Put ("│ Status:  ");
      Put_Task_Status (Stores_Subsystem.Stores_Task_Control);
      Move_Cursor_To (COl1_RSide_X, Stores_Y + 2);
      Put ("│");

      Offset := 3;
      for I in Stores_Subsystem.Light_Index loop
         Move_Cursor_To (Stores_X, Stores_Y + Offset);
         Put ("│ Light");
         Put (I'Image);
         Put (": ");
         Put_Stores_Status (Stores_Subsystem.Stores_Data.Get_Light_Status (I));
         Move_Cursor_To (COl1_RSide_X, Stores_Y + Offset);
         Put ("│");

         Offset := Offset + 1;
      end loop;

      for I in Stores_Subsystem.Heavy_Index loop
         Move_Cursor_To (Stores_X, Stores_Y + Offset);
         Put ("│ Heavy");
         Put (I'Image);
         Put (": ");
         Put_Stores_Status (Stores_Subsystem.Stores_Data.Get_Heavy_Status (I));
         Move_Cursor_To (COl1_RSide_X, Stores_Y + Offset);
         Put ("│");

         Offset := Offset + 1;
      end loop;

      Move_Cursor_To (Stores_X, Stores_Y + Offset);
      Put_Line ("╰───────────────────────────╯");
      pragma Style_Checks (On);
   end Put_Stores_Data;

   -------------------
   -- Put_IMRS_Data --
   -------------------

   procedure Put_IMRS_Data is
   begin
      pragma Style_Checks (Off);
      Put_Subsystem_Name ("IMRS", IMRS_X, IMRS_Y);

      Move_Cursor_To (IMRS_X, IMRS_Y + 2);
      Put ("│  PBIT Status: ");
      Put_BIT_Status
        (Integrated_Monitoring_And_Recording_Subsystem.Get_PBIT_Status);
      Move_Cursor_To (IMRS_RSide_X, IMRS_Y + 2);
      Put ("│");

      Move_Cursor_To (IMRS_X, IMRS_Y + 3);
      Put ("│  CBIT Status: ");
      Put_BIT_Status
        (Integrated_Monitoring_And_Recording_Subsystem.Get_CBIT_Status);
      Move_Cursor_To (IMRS_RSide_X, IMRS_Y + 3);
      Put ("│");

      Move_Cursor_To (IMRS_X, IMRS_Y + 4);
      Put_Line ("╰───────────────────────────╯");
      pragma Style_Checks (On);
   end Put_IMRS_Data;

   -------------------------------------
   -- Put_Flight_Recorder_Information --
   -------------------------------------

   procedure Put_Flight_Recorder_Information is
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
      Put_BIT_Status
        (Integrated_Monitoring_And_Recording_Subsystem.Get_CBIT_Status);
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
           (Integrated_Monitoring_And_Recording_Subsystem.Get_CBIT_Status
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
      Put_BIT_Status
        (Integrated_Monitoring_And_Recording_Subsystem.Get_PBIT_Status);

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
           (Integrated_Monitoring_And_Recording_Subsystem.Get_PBIT_Status
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

      --  Add the individual entries
      declare
         BIT_Historic_Report : constant Historic_BIT_Entries_Data :=
           Integrated_Monitoring_And_Recording_Subsystem.
             Get_BIT_Historic_Report;
      begin

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
      end;

      pragma Style_Checks (On);
   end Put_Flight_Recorder_Information;
   ------------------------------
   -- Put_Countermeasures_Data --
   ------------------------------

   procedure Put_Countermeasures_Data is
   begin
      pragma Style_Checks (Off);
      Put_Subsystem_Name
        ("Cyber-Countermeasures", Countermeasures_X, Countermeasures_Y);

      Move_Cursor_To (Countermeasures_X, Countermeasures_Y + 2);
      Put ("│ Status:          ");
      Put_Task_Status (Countermeasures_Subsystem.Countermeasures_Task_Control);
      Move_Cursor_To (Countermeasures_RSide_X, Countermeasures_Y + 2);
      Put ("│");

      Move_Cursor_To (Countermeasures_X, Countermeasures_Y + 3);
      Put ("│ Countermeasures: ");
      Put_Countermeasure_Status
        (Countermeasures_Subsystem.Countermeasures_Control.Get_Status);
      Move_Cursor_To (Countermeasures_RSide_X, Countermeasures_Y + 3);
      Put ("│");

      Move_Cursor_To (Countermeasures_X, Countermeasures_Y + 4);
      Put_Line ("╰───────────────────────────╯");
      pragma Style_Checks (On);
   end Put_Countermeasures_Data;

   ------------------
   -- Put_Commands --
   ------------------

   procedure Put_Commands is
   begin

      if Display_Control_Data.Get_Requested_Display = Main_Avioics_Screen then
         Move_Cursor_To
           (Commands_Main_Screen_X_Left,
            Commands_Main_Screen_Y_Left);

         Put_Line ("Key commands:");
         Put_Line (" [A] Send valid radar packet");
         Put_Line (" [C] Send truncated radar packet");
         Put_Line (" [D] Deploy cyber countermeasures");
         Put_Line (" [H] Launch heavy armament");

         Move_Cursor_To
           (Commands_Main_Screen_X_Right,
            Commands_Main_Screen_Y_Right);

         Put_Line (" [L] Launch light armament");

         Move_Cursor_To
           (Commands_Main_Screen_X_Right,
            Commands_Main_Screen_Y_Right + 1);

         Put_Line (" [F] Display flight recorder information");

         Move_Cursor_To
           (Commands_Main_Screen_X_Right,
            Commands_Main_Screen_Y_Right + 2);

         Put_Line (" [M] Display main avionics screen");

         Move_Cursor_To
           (Commands_Main_Screen_X_Right,
            Commands_Main_Screen_Y_Right + 3);

         Put_Line (" [X] Exit");
      else
         Move_Cursor_To
           (Commands_Flight_Recorder_Screen_X,
            Commands_Flight_Recorder_Screen_Y);

         Put_Line ("Key commands:");
         Put_Line (" [M] Display main avionics screen");
         Put_Line (" [X] Exit");
      end if;
   end Put_Commands;

   ---------------------
   -- Put_Task_Status --
   ---------------------

   procedure Put_Task_Status (EXB : in out Monitored_Tasking.Task_Control)
   is
      Status : constant Task_Status_Kind := EXB.Status;
   begin
      Set_Text_Colour (Task_Status_Colour (Status));
      Put (Status'Image);
      Set_Text_Colour (Default);
   end Put_Task_Status;

   ------------------------
   -- Put_Subsystem_Name --
   ------------------------

   procedure Put_Subsystem_Name (Name : String; X : Natural; Y : Natural) is
      Width           : constant Positive := 27;
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
   begin
      loop
         if Display_Control_Data.Get_Requested_Display = Main_Avioics_Screen
         then
            Clear;
            Put_Countermeasures_Data;
            Put_Radar_Data;
            Put_Flight_Data;
            Put_Fuel_Data;
            Put_Navigation_Data;
            Put_Targeting_Data;
            Put_Stores_Data;
            Put_IMRS_Data;
            Put_Commands;
            Ada.Text_IO.New_Line;
         else
            Clear;
            Put_Flight_Recorder_Information;
            Put_Commands;
            Ada.Text_IO.New_Line;
         end if;

         Next_Time := Next_Time + Update_Period;
         delay until Next_Time;
      end loop;
   end Display_Task;

   --------------------------
   -- Display_Control_Data --
   --------------------------

   protected body Display_Control_Data is

      ------------------------------
      -- Show_Main_Avioics_Screen --
      ------------------------------

      procedure Show_Main_Avioics_Screen is
      begin
         Requested_Display := Main_Avioics_Screen;
      end Show_Main_Avioics_Screen;

      ---------------------------------
      -- Show_Flight_Recorder_Screen --
      ---------------------------------

      procedure Show_Flight_Recorder_Screen is
      begin
         Requested_Display := Flight_Recorder_Screen;
      end Show_Flight_Recorder_Screen;

      function Get_Requested_Display return Requested_Display_Type is
         (Requested_Display);

   end Display_Control_Data;

end Display;
