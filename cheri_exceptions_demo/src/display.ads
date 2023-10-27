--
--  Copyright (C) 2023, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

--  This package is responsible for periodically displaying the status and
--  data from the various simulated subsystems.

with Ada.Real_Time;

package Display is

   procedure Show_Main_Avioics_Screen;

   procedure Show_Flight_Recorder_Screen;

   task Display_Task;

private

   Start_Time : Ada.Real_Time.Time := Ada.Real_Time.Clock;

   type Requested_Display_Type is
     (Main_Avioics_Screen, Flight_Recorder_Screen);

   protected Display_Control_Data is

      procedure Show_Main_Avioics_Screen;

      procedure Show_Flight_Recorder_Screen;

      function Get_Requested_Display return Requested_Display_Type;

   private

     Requested_Display : Requested_Display_Type;

   end Display_Control_Data;

end Display;
