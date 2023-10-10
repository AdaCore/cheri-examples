--
--  Copyright (C) 2023, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--
with Ada.Characters.Latin_1; use Ada.Characters.Latin_1;
with Ada.Exceptions;
with Ada.Real_Time;          use Ada.Real_Time;
with Ada.Text_IO;            use Ada.Text_IO;

with Countermeasures_Subsystem; use Countermeasures_Subsystem;
with Flight_Subsystem;
with Fuel_Subsystem;
with Monitored_Tasking;         use Monitored_Tasking;
with Navigation_Subsystem;
with Radar_Subsystem;
with Stores_Subsystem;          use Stores_Subsystem;
with Targeting_Subsystem;

package body Display is

   Update_Period : constant Time_Span := Milliseconds (500);

   procedure Clear;
   --  Clear the terminal

   procedure Home;
   --  Move the cursor to the home position (top left)

   procedure Move_Cursor_To (X, Y : Natural);
   --  Move the cursor to a position on the screen. (0, 0) is top left

   type Text_Colour is (Default, Green, Red, Yellow);

   procedure Set_Text_Colour (Colour : Text_Colour);
   --  Set the colour of the text

   Task_Status_Colour : constant
     array (Monitored_Tasking.Task_Status_Kind)
     of Text_Colour :=
       (Normal      => Green,
        Degraded    => Yellow,
        Compromised => Red,
        Resetting   => Yellow);
   --  Lookup table for the text colour of each task status

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
   procedure Put_Countermeasures_Data;
   procedure Put_Commands;
   procedure Put_Errors;

   procedure Put_Task_Status (EXB : in out Monitored_Tasking.Task_Control);

   procedure Put_Task_Exception
     (EXB : in out Monitored_Tasking.Task_Control);

   procedure Put_Countermeasure_Status
     (Status : Countermeasures_Subsystem.Status_Kind);

   procedure Put_Stores_Status (Status : Stores_Subsystem.Status_Kind);

   ---------------
   -- UI Layout --
   ---------------

   --  Screen coordinates of the UI elements. (0, 0) is the top-left.

   Countermeasures_X : constant := 27;
   Countermeasures_Y : constant := 0;

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

   Commands_X : constant := 0;
   Commands_Y : constant := 23;

   Errors_X : constant := 54;
   Errors_Y : constant := 23;

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
      end case;
   end Set_Text_Colour;

   --------------------
   -- Put_Radar_Data --
   --------------------

   procedure Put_Radar_Data is
   begin
      Move_Cursor_To (Radar_X, Radar_Y);

      Put ("+----------Radar-----------+");

      Move_Cursor_To (Radar_X, Radar_Y + 1);
      Put (" Status: ");
      Put_Task_Status (Radar_Subsystem.Radar_Task_Control);

      Move_Cursor_To (Radar_X, Radar_Y + 2);
      Put (" Radar:  ");
      if Radar_Subsystem.Radar_Data.Is_Operational then
         Put ("On");
      else
         Put ("Off");
      end if;

      Move_Cursor_To (Radar_X, Radar_Y + 3);
      Put (" Tracks:");
      Put (Radar_Subsystem.Radar_Data.Tracks'Length'Image);

      Move_Cursor_To (Radar_X, Radar_Y + 4);
      Put_Line ("+--------------------------+");
   end Put_Radar_Data;

   ---------------------
   -- Put_Flight_Data --
   ---------------------

   procedure Put_Flight_Data is
   begin
      Move_Cursor_To (Flight_X, Flight_Y);
      Put ("+-------Flight Data--------+");

      Move_Cursor_To (Flight_X, Flight_Y + 1);
      Put (" Status: ");
      Put_Task_Status (Flight_Subsystem.Flight_Task_Control);

      Move_Cursor_To (Flight_X, Flight_Y + 2);
      Put (" Altitude:");
      Put (Flight_Subsystem.Flight_Data.Get_Altitude'Image);
      Put (" ft");

      Move_Cursor_To (Flight_X, Flight_Y + 3);
      Put (" Airspeed:");
      Put (Flight_Subsystem.Flight_Data.Get_Airspeed'Image);
      Put (" KIAS");

      Move_Cursor_To (Flight_X, Flight_Y + 4);
      Put_Line ("+--------------------------+");
   end Put_Flight_Data;

   -------------------
   -- Put_Fuel_Data --
   -------------------

   procedure Put_Fuel_Data is
      Bingo_Minutes : Duration;
   begin
      Move_Cursor_To (Fuel_X, Fuel_Y);

      Put ("+-----------Fuel-----------+");

      Move_Cursor_To (Fuel_X, Fuel_Y + 1);
      Put (" Status: ");
      Put_Task_Status (Fuel_Subsystem.Fuel_Task_Control);

      Move_Cursor_To (Fuel_X, Fuel_Y + 2);
      Put (" FQI:   ");
      Put (Fuel_Subsystem.Fuel_Data.Fuel_Quantity'Image);
      Put (" kg");

      Move_Cursor_To (Fuel_X, Fuel_Y + 3);
      Put (" Bingo: ");
      Bingo_Minutes := Fuel_Subsystem.Fuel_Data.Time_To_Minimum_Fuel / 60.0;
      Put (Integer (Bingo_Minutes)'Image);
      Put (" minutes");

      Move_Cursor_To (Fuel_X, Fuel_Y + 4);
      Put_Line ("+--------------------------+");
   end Put_Fuel_Data;

   -------------------------
   -- Put_Navigation_Data --
   -------------------------

   procedure Put_Navigation_Data is
      Position : constant Navigation_Subsystem.Coordinate :=
                   Navigation_Subsystem.Navigation_Data.Get_Position;
   begin
      Move_Cursor_To (Navigation_X, Navigation_Y);
      Put ("+--------Navigation--------+");

      Move_Cursor_To (Navigation_X, Navigation_Y + 1);
      Put (" Status:    ");
      Put_Task_Status (Navigation_Subsystem.Navigation_Task_Control);

      Move_Cursor_To (Navigation_X, Navigation_Y + 2);
      Put (" Latitude:  ");
      Put (Position.Lat'Image);

      Move_Cursor_To (Navigation_X, Navigation_Y + 3);
      Put (" Longitude: ");
      Put (Position.Long'Image);

      Move_Cursor_To (Navigation_X, Navigation_Y + 4);
      Put (" Heading:   ");
      Put (Navigation_Subsystem.Navigation_Data.Get_Heading'Image);
      Put ("°");

      Move_Cursor_To (Navigation_X, Navigation_Y + 5);
      Put_Line ("+--------------------------+");
   end Put_Navigation_Data;

   ------------------------
   -- Put_Targeting_Data --
   ------------------------

   procedure Put_Targeting_Data is
   begin
      Move_Cursor_To (Targeting_X, Targeting_Y);

      Put ("+--------Targeting---------+");

      Move_Cursor_To (Targeting_X, Targeting_Y + 1);
      Put (" Status:  ");
      Put_Task_Status (Targeting_Subsystem.Targeting_Task_Control);

      Move_Cursor_To (Targeting_X, Targeting_Y + 2);
      Put (" Targets: ");
      Put (Targeting_Subsystem.Targeting_Data.Get_Targets'Length'Image);

      Move_Cursor_To (Targeting_X, Targeting_Y + 3);
      Put_Line ("+--------------------------+");
   end Put_Targeting_Data;

   ---------------------
   -- Put_Stores_Data --
   ---------------------

   procedure Put_Stores_Data is
      Offset : Natural;
   begin
      Move_Cursor_To (Stores_X, Stores_Y);

      Put ("+----------Stores----------+");

      Move_Cursor_To (Stores_X, Stores_Y + 1);
      Put (" Status:  ");
      Put_Task_Status (Stores_Subsystem.Stores_Task_Control);

      Offset := 2;
      for I in Stores_Subsystem.Light_Index loop
         Move_Cursor_To (Stores_X, Stores_Y + Offset);
         Put (" Light");
         Put (I'Image);
         Put (": ");
         Put_Stores_Status (Stores_Subsystem.Stores_Data.Get_Light_Status (I));

         Offset := Offset + 1;
      end loop;

      for I in Stores_Subsystem.Heavy_Index loop
         Move_Cursor_To (Stores_X, Stores_Y + Offset);
         Put (" Heavy");
         Put (I'Image);
         Put (": ");
         Put_Stores_Status (Stores_Subsystem.Stores_Data.Get_Heavy_Status (I));

         Offset := Offset + 1;
      end loop;

      Move_Cursor_To (Stores_X, Stores_Y + Offset);
      Put_Line ("+--------------------------+");
   end Put_Stores_Data;

   ------------------------------
   -- Put_Countermeasures_Data --
   ------------------------------

   procedure Put_Countermeasures_Data is
   begin
      Move_Cursor_To (Countermeasures_X, Countermeasures_Y);

      Put ("+---Cyber-Countermeasures--+");

      Move_Cursor_To (Countermeasures_X, Countermeasures_Y + 1);
      Put (" Status:          ");
      Put_Task_Status (Countermeasures_Subsystem.Countermeasures_Task_Control);

      Move_Cursor_To (Countermeasures_X, Countermeasures_Y + 2);
      Put (" Countermeasures: ");
      Put_Countermeasure_Status
        (Countermeasures_Subsystem.Countermeasures_Control.Get_Status);

      Move_Cursor_To (Countermeasures_X, Countermeasures_Y + 3);
      Put_Line ("+--------------------------+");
   end Put_Countermeasures_Data;

   ------------------
   -- Put_Commands --
   ------------------

   procedure Put_Commands is
   begin
      Move_Cursor_To (Commands_X, Commands_Y);
      Put_Line ("Key commands:");
      Put_Line (" [A] Send valid radar packet");
      Put_Line (" [C] Send truncated radar packet");
      Put_Line (" [D] Deploy cyber countermeasures");
      Put_Line (" [H] Launch heavy armament");
      Put_Line (" [L] Launch light armament");
   end Put_Commands;

   ----------------
   -- Put_Errors --
   ----------------

   procedure Put_Errors is
   begin
      Move_Cursor_To (Errors_X, Errors_Y);
      Put ("CHERI exceptions detected:");

      Move_Cursor_To (Errors_X, Errors_Y + 1);
      Put (" Radar:           ");
      Put_Task_Exception (Radar_Subsystem.Radar_Task_Control);

      Move_Cursor_To (Errors_X, Errors_Y + 2);
      Put (" Targeting:       ");
      Put_Task_Exception (Targeting_Subsystem.Targeting_Task_Control);

      Move_Cursor_To (Errors_X, Errors_Y + 3);
      Put (" Stores:          ");
      Put_Task_Exception (Stores_Subsystem.Stores_Task_Control);

      Move_Cursor_To (Errors_X, Errors_Y + 4);
      Put (" Flight           ");
      Put_Task_Exception (Flight_Subsystem.Flight_Task_Control);

      Move_Cursor_To (Errors_X, Errors_Y + 5);
      Put (" Fuel:            ");
      Put_Task_Exception (Fuel_Subsystem.Fuel_Task_Control);

      Move_Cursor_To (Errors_X, Errors_Y + 6);
      Put (" Navigation:      ");
      Put_Task_Exception (Navigation_Subsystem.Navigation_Task_Control);

      Move_Cursor_To (Errors_X, Errors_Y + 7);
      Put (" Countermeasures: ");
      Put_Task_Exception
        (Countermeasures_Subsystem.Countermeasures_Task_Control);
   end Put_Errors;

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
   -- Put_Task_Exception --
   ------------------------

   procedure Put_Task_Exception
     (EXB : in out Monitored_Tasking.Task_Control)
   is
      Last_Ex : Ada.Exceptions.Exception_Occurrence;
   begin
      if EXB.Exception_Occurred then
         EXB.Last_Exception (Last_Ex);
         Put (Ada.Exceptions.Exception_Name (Last_Ex));
      else
         Put ("None");
      end if;
   end Put_Task_Exception;

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
         Clear;
         Put_Countermeasures_Data;
         Put_Radar_Data;
         Put_Flight_Data;
         Put_Fuel_Data;
         Put_Navigation_Data;
         Put_Targeting_Data;
         Put_Stores_Data;
         Put_Commands;
         Put_Errors;

         Next_Time := Next_Time + Update_Period;
         delay until Next_Time;
      end loop;
   end Display_Task;

end Display;
