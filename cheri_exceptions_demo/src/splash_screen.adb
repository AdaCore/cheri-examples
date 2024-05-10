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

with Ada.Text_IO; use Ada.Text_IO;
with Display;

package body Splash_Screen is

   Splash_Screen_X : constant := 2;
   Splash_Screen_Y : constant := 5;

   ---------------------------
   -- Display_Splash_Screen --
   ---------------------------

   procedure Display_Splash_Screen
   is

      Rolling_Splash_Screen_Y : Integer := Splash_Screen_Y;

      -----------------
      -- Put_Rolling --
      -----------------

      procedure Put_Rolling (Output : String) is
      begin
         Move_Cursor_To (Splash_Screen_X, Rolling_Splash_Screen_Y);
         Put (Output);
         Rolling_Splash_Screen_Y := Rolling_Splash_Screen_Y + 1;
      end Put_Rolling;
   begin

      --  Set the next time to animate
      Time_To_Next_Animate := Clock + Animate_Time;

      pragma Style_Checks (Off);
      Set_Text_Colour (Colour_1);
      Put_Rolling ("╭───────────────────────────────────────────────────────────────────────────────────────────────────────────────╮");
      Put_Rolling ("│                 ██                 ██                    ██████                                               │  ");
      Put_Rolling ("│                ████                ██                  ██      █                                              │  ");
      Put_Rolling ("│               ██  ██               ██                ██                                                       │  ");
      Put_Rolling ("│              ██    ██      ██████████   ████████    ██              ████████    ██ ████  ████████             │  ");
      Put_Rolling ("│              ██    ██     ██       ██          ██  ██             ███      ███  ████    ██      ██            │  ");
      Put_Rolling ("│             ██      ██   ██        ██     ███   ██ ██            ███        ███ ██     ██   ███████           │  ");
      Put_Rolling ("│            ██   ███████  ██        ██   ███     ██  ██           ███        ███ ██     ██                     │  ");
      Put_Rolling ("│            ██        ██   ██       ██  ██      ███   ██        █  ███      ███  ██     ███      ██            │  ");
      Put_Rolling ("│           ██          ██   ██████████   ███████ ██    █████████     ████████    ██      █████████             │  ");
      Put_Rolling ("╰───────────────────────────────────────────────────────────────────────────────────────────────────────────────╯");
      Rolling_Splash_Screen_Y := Rolling_Splash_Screen_Y + 1;
      Set_Text_Colour (Colour_2);
      Rolling_Splash_Screen_Y := Rolling_Splash_Screen_Y + 1;
      Put_Rolling ("                            ✈  Ｅｄｇｅ Ａｖｉｏｎｉｃｓ Ｄｅｍｏｎｓｔｒａｔｏｒ  ✈");
      Rolling_Splash_Screen_Y := Rolling_Splash_Screen_Y + 1;
      Put_Rolling ("         ✈  ＣＨＥＲＩ ＰｕｒｅーＣａｐａｂｉｌｉｔｙ ＢａｒｅーＭｅｔａｌ Ａｄａ ＲｕｎーＴｉｍｅ  ✈");
      Rolling_Splash_Screen_Y := Rolling_Splash_Screen_Y + 1;
      Put_Rolling ("                              ✈  Ｅｘｅｃｕｔｉｎｇ Ｏｎ Ａｒｍ Ｍｏｒｅｌｌｏ  ✈");
      Rolling_Splash_Screen_Y := Rolling_Splash_Screen_Y + 1;
      Put_Rolling ("         ✈  ＣＨＥＲＩ Ｆａｕｌｔ Ｐｒｏｐａｇａｔｉｏｎ ｔｏ Ａｄａ Ｅｘｃｅｐｔｉｏｎ Ｈａｎｄｌｅｒｓ  ✈");
      Rolling_Splash_Screen_Y := Rolling_Splash_Screen_Y + 1;
      Set_Text_Colour (Default);
      Set_Text_Colour (Default);
      Put_Rolling ("                                             𝗔𝗱𝗮𝗖𝗼𝗿𝗲  ② ⓪ ② ④");
      Put_Rolling ("──────────────────────────────────────────────────────────────────────────────────────────────────────────────");
      Rolling_Splash_Screen_Y := Rolling_Splash_Screen_Y + 1;
      Set_Text_Colour (Green);
      Put_Rolling ("                                         𝘗𝘳𝘦𝘴𝘴 𝘢𝘯𝘺 𝘬𝘦𝘺 𝘵𝘰 𝘤𝘰𝘯𝘵𝘪𝘯𝘶𝘦                                           ");
      Set_Text_Colour (Default);
      Rolling_Splash_Screen_Y := Rolling_Splash_Screen_Y + 1;
      Put_Rolling ("──────────────────────────────────────────────────────────────────────────────────────────────────────────────");
      Rolling_Splash_Screen_Y := Rolling_Splash_Screen_Y + 1;
      Set_Text_Colour (Colour_1);
      Put_Rolling ("                                              🅓 ⓘ ⓖ ⓘ ⓣ ⓐ ⓛ");
      Put_Rolling ("                                              🅢 ⓔ ⓒ ⓤ ⓡ ⓘ ⓣ ⓨ");
      Put_Rolling ("                                              🅑 ⓨ");
      Put_Rolling ("                                              🅓 ⓔ ⓢ ⓘ ⓖ ⓝ");
      Put_Rolling ("                                                                                                              ");
      pragma Style_Checks (On);

   end Display_Splash_Screen;

   -----------------------------
   -- Display_Splash_Screen_2 --
   -----------------------------

   procedure Display_Splash_Screen_2 is
      Rolling_Splash_Screen_Y : Integer := Splash_Screen_Y;

      -----------------
      -- Put_Rolling --
      -----------------

      procedure Put_Rolling (Output : String) is
      begin
         Move_Cursor_To (Splash_Screen_X, Rolling_Splash_Screen_Y);
         Put (Output);
         Rolling_Splash_Screen_Y := Rolling_Splash_Screen_Y + 1;
      end Put_Rolling;
   begin
      Set_Text_Colour (Colour_1);
      pragma Style_Checks (Off);
      Put_Rolling ("                              𝗔𝗱𝗮𝗖𝗼𝗿𝗲 𝟮𝟬𝟮𝟰");

      Put_Rolling ("    🍒 Scan the QR code to learn more about GNAT Pro for CHERI  🍒    ");
      Put_Rolling ("");
      Put_Rolling ("");
      Put_Rolling ("   ██████████████    ██  ██        ██    ████  ██  ██  ██████████████");
      Put_Rolling ("   ██          ██    ██████    ██████  ██  ██████  ██  ██          ██");
      Put_Rolling ("   ██  ██████  ██  ██████████    ██      ██  ██  ████  ██  ██████  ██");
      Put_Rolling ("   ██  ██████  ██  ██████  ██    ██████      ████████  ██  ██████  ██");
      Put_Rolling ("   ██  ██████  ██    ██      ██  ██        ████████    ██  ██████  ██");
      Put_Rolling ("   ██          ██      ██  ██    ████    ██  ████  ██  ██          ██");
      Put_Rolling ("   ██████████████  ██  ██  ██  ██  ██  ██  ██  ██  ██  ██████████████");
      Put_Rolling ("                       ████  ██  ██    ██  ██    ██                  ");
      Put_Rolling ("         ████  ████        ████  ██████  ████  ████          ████    ");
      Put_Rolling ("     ██  ██████    ████      ██  ██████          ██  ██  ████        ");
      Put_Rolling ("     ██  ████████  ██  ██  ████    ██  ████  ██  ██  ██  ██    ██████");
      Put_Rolling ("   ████████          ████        ██  ██    ██          ██████  ██  ██");
      Put_Rolling ("   ████  ██    ██  ████    ██████      ████  ████  ██  ██  ██    ████");
      Put_Rolling ("         ██          ████    ████████    ██████████████        ████  ");
      Put_Rolling ("   ██    ██████████    ████  ██████    ██  ██          ██  ██        ");
      Put_Rolling ("     ████  ██    ██████  ██      ████████          ████    ██  ████  ");
      Put_Rolling ("               ████  ████████    ██                  ██████  ████    ");
      Put_Rolling ("           ██    ████  ██  ██  ██  ██████  ██  ██  ██  ████████  ████");
      Put_Rolling ("     ████████  ██  ██████    ██  ██    ██      ████      ██  ████  ██");
      Put_Rolling ("   ██  ██████          ██  ██      ████  ████  ████████    ██████  ██");
      Put_Rolling ("   ██  ██  ██████████████  ████  ████████  ████        ████  ██  ██  ");
      Put_Rolling ("   ████████        ████  ████  ██████    ██  ██    ██  ██  ██    ██  ");
      Put_Rolling ("   ██    ██  ████████████  ████        ██  ████████    ████    ██████");
      Put_Rolling ("   ██  ██  ██    ██  ██  ██  ████████████████    ████    ████  ████  ");
      Put_Rolling ("   ██████    ████████████  ████    ████    ██    ████████████      ██");
      Put_Rolling ("                   ████      ████  ██    ██  ████████      ████████  ");
      Put_Rolling ("   ██████████████  ████  ████████  ██████        ████  ██  ████      ");
      Put_Rolling ("   ██          ██    ██  ██    ██          ██████  ██      ████████  ");
      Put_Rolling ("   ██  ██████  ██  ██        ████    ██████████████████████████████  ");
      Put_Rolling ("   ██  ██████  ██  ████  ██    ██  ████  ████████  ████  ██      ██  ");
      Put_Rolling ("   ██  ██████  ██      ██████  ████  ██  ██                ████  ████");
      Put_Rolling ("   ██          ██    ██  ████████  ██████      ████          ██  ████");
      Put_Rolling ("   ██████████████    ██████████████      ████    ██████    ██  ██    ");
      Put_Rolling ("                                                                     ");

      pragma Style_Checks (On);

   end Display_Splash_Screen_2;

   -----------------------------------
   -- Display_Dynamic_Splash_Screen --
   -----------------------------------

   procedure Display_Dynamic_Splash_Screen is
   begin

      --  Check if we need to switch
      if Clock > Time_To_Switch_Splash_Screens then
         Colour_1 := White;
         Colour_2 := Yellow;
         if Showing_Splash_Screen_1 then
            Clear;
            Display_Splash_Screen_2;
         else
            Clear;
            Display_Splash_Screen;
         end if;

         Showing_Splash_Screen_1 := not Showing_Splash_Screen_1;
         Time_To_Switch_Splash_Screens :=
           Clock + Splash_Switch_Time;
      end if;

      --  Check if we need to animate
      if Showing_Splash_Screen_1 then
         if Clock > Time_To_Next_Animate then

            --  Perform the animation
            if Colour_1 = Yellow then
               Colour_1 := White;
               Colour_2 := Yellow;
            else
               Colour_1 := Yellow;
               Colour_2 := White;
            end if;

            Clear;
            Display_Splash_Screen;

            Time_To_Next_Animate := Clock + Animate_Time;
         end if;
      end if;
   end Display_Dynamic_Splash_Screen;

   ---------------
   -- Key_Press --
   ---------------

   procedure Key_Press is
      Time_To_Next_Splash_Screen : constant Time := Clock + Allowed_Idle_Time;
      Cancelled : Boolean;
   begin

      --  Stop the current handler
      Cancel_Handler (Allowed_Idle_Timing_Event, Cancelled);

      --  Create a new event handler to switch back to the splash screen on
      --  idle
      Set_Handler (Event   => Allowed_Idle_Timing_Event,
                   At_Time => Time_To_Next_Splash_Screen,
                   Handler => Protected_Timer_Event.Idle_Timer_Fired'Access);
   end Key_Press;

   ---------------------------
   -- Protected_Timer_Event --
   ---------------------------

   protected body Protected_Timer_Event is

      ----------------------
      -- Idle_Timer_Fired --
      ----------------------

      procedure Idle_Timer_Fired (Event : in out Timing_Event) is
         pragma Unreferenced (Event);
      begin
         Display.Show_Splash_Screen_Screen;
      end Idle_Timer_Fired;

   end Protected_Timer_Event;

end Splash_Screen;
