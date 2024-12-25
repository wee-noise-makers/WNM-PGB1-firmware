-------------------------------------------------------------------------------
--                                                                           --
--                              Wee Noise Maker                              --
--                                                                           --
--                     Copyright (C) 2023 Fabien Chouteau                    --
--                                                                           --
--    Wee Noise Maker is free software: you can redistribute it and/or       --
--    modify it under the terms of the GNU General Public License as         --
--    published by the Free Software Foundation, either version 3 of the     --
--    License, or (at your option) any later version.                        --
--                                                                           --
--    Wee Noise Maker is distributed in the hope that it will be useful,     --
--    but WITHOUT ANY WARRANTY; without even the implied warranty of         --
--    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU       --
--    General Public License for more details.                               --
--                                                                           --
--    You should have received a copy of the GNU General Public License      --
--    along with We Noise Maker. If not, see <http://www.gnu.org/licenses/>. --
--                                                                           --
-------------------------------------------------------------------------------

with Tresses.Macro;
with Tresses.Drums.Sine_Kick;
with Tresses.Drums.Triangle_Kick;
with Tresses.Drums.Chip_Kick;

package body WNM.Voices.Kick_Voice is

   ------------
   -- Engine --
   ------------

   function Engine (This : Instance) return Kick_Engine
   is (This.Engine);

   ----------------
   -- Set_Engine --
   ----------------

   procedure Set_Engine (This : in out Instance; E : Kick_Engine) is
   begin
      if E /= This.Engine then
         This.Engine := E;
         Init (This);
      end if;
   end Set_Engine;

   ----------
   -- Init --
   ----------

   procedure Init (This : in out Instance) is
   begin
      This.Do_Init := True;
   end Init;

   ------------
   -- Render --
   ------------

   procedure Render (This   : in out Instance;
                     Buffer :    out Tresses.Mono_Buffer)
   is
   begin
      case This.Engine is
         when Sine_Kick =>
            Tresses.Drums.Sine_Kick.Render_Kick
              (Buffer,
               Params => This.Params,
               Phase => This.Phase,
               Phase_Increment => This.Phase_Increment,
               Target_Phase_Increment => This.Target_Phase_Increment,
               Env => This.Env0,
               Pitch_Env => This.Env1,
               Pitch => This.Pitch,
               Do_Init => This.Do_Init,
               Do_Strike => This.Do_Strike);

         when Triangle_Kick =>
            Tresses.Drums.Triangle_Kick.Render_Kick
              (Buffer,
               Params => This.Params,
               Phase => This.Phase,
               Phase_Increment => This.Phase_Increment,
               Target_Phase_Increment => This.Target_Phase_Increment,
               Env => This.Env0,
               Pitch_Env => This.Env1,
               Pitch => This.Pitch,
               Do_Init => This.Do_Init,
               Do_Strike => This.Do_Strike);

         when Chip_Kick =>
            Tresses.Drums.Chip_Kick.Render_Kick
              (Buffer,
               Params => This.Params,
               Phase => This.Phase,
               Phase_Increment => This.Phase_Increment,
               Target_Phase_Increment => This.Target_Phase_Increment,
               Env => This.Env0,
               Pitch_Env => This.Env1,
               Pitch => This.Pitch,
               Do_Init => This.Do_Init,
               Do_Strike => This.Do_Strike);

      end case;
   end Render;

   -------------------
   -- Tresse_Engine --
   -------------------

   function Tresse_Engine (E : Kick_Engine) return Tresses.Engines
   is (case E is
          when Sine_Kick => Tresses.Drum_Sine_Kick,
          when Triangle_Kick => Tresses.Drum_Triangle_Kick,
          when Chip_Kick => Tresses.Drum_Chip_Kick);

   ---------
   -- Img --
   ---------

   function Img (E : Kick_Engine) return String
   is (Tresses.Img (Tresse_Engine (E)));

   -----------------
   -- Param_Label --
   -----------------

   overriding
   function Param_Label (This : Instance; Id : Param_Id) return String
   is (Tresses.Macro.Param_Label (Tresse_Engine (This.Engine), Id));

   -----------------------
   -- Param_Short_Label --
   -----------------------

   overriding
   function Param_Short_Label (This : Instance; Id : Param_Id)
                               return Short_Label
   is (Tresses.Macro.Param_Short_Label (Tresse_Engine (This.Engine), Id));

end WNM.Voices.Kick_Voice;
