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
with Tresses.Drums.HH_909_Sampled;
with Tresses.Drums.HH_707_Sampled;

package body WNM.Voices.Hihat_Voice is

   ------------
   -- Engine --
   ------------

   function Engine (This : Instance) return HH_Engine
   is (This.Engine);

   ----------------
   -- Set_Engine --
   ----------------

   procedure Set_Engine (This : in out Instance; E : HH_Engine) is
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
         when Cymbal =>
            Drums.Cymbal.Render_Cymbal (Buffer,
                                        Params  => This.Params,
                                        Filter0 => This.Filter0,
                                        Filter1 => This.Filter1,
                                        Env => This.Env0,
                                        State => This.Cym_State,
                                        Phase => This.Phase,
                                        Pitch => This.Pitch,
                                        Do_Init => This.Do_Init,
                                        Do_Strike => This.Do_Strike);

         when HH909 =>
            Drums.HH_909_Sampled.Render
              (Buffer,
               Params => This.Params,
               Filter => This.Filter0,
               Env => This.Env0,
               Phase =>  This.Phase,
               Do_Init => This.Do_Init,
               Do_Strike => This.Do_Strike);

         when HH707 =>
            Drums.HH_707_Sampled.Render
              (Buffer,
               Params => This.Params,
               Filter => This.Filter0,
               Env => This.Env0,
               Phase =>  This.Phase,
               Do_Init => This.Do_Init,
               Do_Strike => This.Do_Strike);
      end case;
   end Render;

   -------------------
   -- Tresse_Engine --
   -------------------

   function Tresse_Engine (E : HH_Engine) return Tresses.Engines
   is (case E is
          when Cymbal => Tresses.Drum_Cymbal,
          when HH909  => Tresses.Drum_909_Hats,
          when HH707  => Tresses.Drum_707_Hats);

   ---------
   -- Img --
   ---------

   function Img (E : HH_Engine) return String
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

end WNM.Voices.Hihat_Voice;
