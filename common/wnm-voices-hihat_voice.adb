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
with Tresses.Drums.HH_808_Sampled;
with Tresses.Drums.HH_505_Sampled;
with Tresses.Drums.HH_CR78_Sampled;
with Tresses.Drums.HH_LM2_Sampled;
with Tresses.Drums.HH_MRK2_Sampled;
with Tresses.Drums.HH_Acoustic_Sampled;

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
               Rng => This.Rng,
               Phase =>  This.Phase,
               Do_Init => This.Do_Init,
               Do_Strike => This.Do_Strike);

         when HH707 =>
            Drums.HH_707_Sampled.Render
              (Buffer,
               Params => This.Params,
               Filter => This.Filter0,
               Env => This.Env0,
               Rng => This.Rng,
               Phase =>  This.Phase,
               Do_Init => This.Do_Init,
               Do_Strike => This.Do_Strike);

         when HH808 =>
            Drums.HH_808_Sampled.Render
              (Buffer,
               Params => This.Params,
               Filter => This.Filter0,
               Env => This.Env0,
               Rng => This.Rng,
               Phase =>  This.Phase,
               Do_Init => This.Do_Init,
               Do_Strike => This.Do_Strike);

         when HH505 =>
            Drums.HH_505_Sampled.Render
              (Buffer,
               Params => This.Params,
               Filter => This.Filter0,
               Env => This.Env0,
               Rng => This.Rng,
               Phase =>  This.Phase,
               Do_Init => This.Do_Init,
               Do_Strike => This.Do_Strike);

         when HHLM2 =>
            Drums.HH_LM2_Sampled.Render
              (Buffer,
               Params => This.Params,
               Filter => This.Filter0,
               Env => This.Env0,
               Rng => This.Rng,
               Phase =>  This.Phase,
               Do_Init => This.Do_Init,
               Do_Strike => This.Do_Strike);

         when HHMRK2 =>
            Drums.HH_MRK2_Sampled.Render
              (Buffer,
               Params => This.Params,
               Filter => This.Filter0,
               Env => This.Env0,
               Rng => This.Rng,
               Phase =>  This.Phase,
               Do_Init => This.Do_Init,
               Do_Strike => This.Do_Strike);

         when HHCR78 =>
            Drums.HH_CR78_Sampled.Render
              (Buffer,
               Params => This.Params,
               Filter => This.Filter0,
               Env => This.Env0,
               Rng => This.Rng,
               Phase =>  This.Phase,
               Do_Init => This.Do_Init,
               Do_Strike => This.Do_Strike);

         when HH_Acoustic =>
            Drums.HH_Acoustic_Sampled.Render
              (Buffer,
               Params => This.Params,
               Filter => This.Filter0,
               Env => This.Env0,
               Rng => This.Rng,
               Phase =>  This.Phase,
               Do_Init => This.Do_Init,
               Do_Strike => This.Do_Strike);

         when HH909_BP =>
            Drums.HH_909_Sampled.Render
              (Buffer,
               Params => This.Params,
               Filter => This.Filter0,
               Env => This.Env0,
               Rng => This.Rng,
               Phase =>  This.Phase,
               Do_Init => This.Do_Init,
               Do_Strike => This.Do_Strike,
               Filter_Mode => Filters.SVF.Band_Pass);

         when HH707_BP =>
            Drums.HH_707_Sampled.Render
              (Buffer,
               Params => This.Params,
               Filter => This.Filter0,
               Env => This.Env0,
               Rng => This.Rng,
               Phase =>  This.Phase,
               Do_Init => This.Do_Init,
               Do_Strike => This.Do_Strike,
               Filter_Mode => Filters.SVF.Band_Pass);

         when HH808_BP =>
            Drums.HH_808_Sampled.Render
              (Buffer,
               Params => This.Params,
               Filter => This.Filter0,
               Env => This.Env0,
               Rng => This.Rng,
               Phase =>  This.Phase,
               Do_Init => This.Do_Init,
               Do_Strike => This.Do_Strike,
               Filter_Mode => Filters.SVF.Band_Pass);

         when HH505_BP =>
            Drums.HH_505_Sampled.Render
              (Buffer,
               Params => This.Params,
               Filter => This.Filter0,
               Env => This.Env0,
               Rng => This.Rng,
               Phase =>  This.Phase,
               Do_Init => This.Do_Init,
               Do_Strike => This.Do_Strike,
               Filter_Mode => Filters.SVF.Band_Pass);

         when HHLM2_BP =>
            Drums.HH_LM2_Sampled.Render
              (Buffer,
               Params => This.Params,
               Filter => This.Filter0,
               Env => This.Env0,
               Rng => This.Rng,
               Phase =>  This.Phase,
               Do_Init => This.Do_Init,
               Do_Strike => This.Do_Strike,
               Filter_Mode => Filters.SVF.Band_Pass);

         when HHMRK2_BP =>
            Drums.HH_MRK2_Sampled.Render
              (Buffer,
               Params => This.Params,
               Filter => This.Filter0,
               Env => This.Env0,
               Rng => This.Rng,
               Phase =>  This.Phase,
               Do_Init => This.Do_Init,
               Do_Strike => This.Do_Strike,
               Filter_Mode => Filters.SVF.Band_Pass);

         when HHCR78_BP =>
            Drums.HH_CR78_Sampled.Render
              (Buffer,
               Params => This.Params,
               Filter => This.Filter0,
               Env => This.Env0,
               Rng => This.Rng,
               Phase =>  This.Phase,
               Do_Init => This.Do_Init,
               Do_Strike => This.Do_Strike,
               Filter_Mode => Filters.SVF.Band_Pass);

         when HH_Acoustic_BP =>
            Drums.HH_Acoustic_Sampled.Render
              (Buffer,
               Params => This.Params,
               Filter => This.Filter0,
               Env => This.Env0,
               Rng => This.Rng,
               Phase =>  This.Phase,
               Do_Init => This.Do_Init,
               Do_Strike => This.Do_Strike,
               Filter_Mode => Filters.SVF.Band_Pass);

         when HH909_HP =>
            Drums.HH_909_Sampled.Render
              (Buffer,
               Params => This.Params,
               Filter => This.Filter0,
               Env => This.Env0,
               Rng => This.Rng,
               Phase =>  This.Phase,
               Do_Init => This.Do_Init,
               Do_Strike => This.Do_Strike,
               Filter_Mode => Filters.SVF.High_Pass);

         when HH707_HP =>
            Drums.HH_707_Sampled.Render
              (Buffer,
               Params => This.Params,
               Filter => This.Filter0,
               Env => This.Env0,
               Rng => This.Rng,
               Phase =>  This.Phase,
               Do_Init => This.Do_Init,
               Do_Strike => This.Do_Strike,
               Filter_Mode => Filters.SVF.High_Pass);

         when HH808_HP =>
            Drums.HH_808_Sampled.Render
              (Buffer,
               Params => This.Params,
               Filter => This.Filter0,
               Env => This.Env0,
               Rng => This.Rng,
               Phase =>  This.Phase,
               Do_Init => This.Do_Init,
               Do_Strike => This.Do_Strike,
               Filter_Mode => Filters.SVF.High_Pass);

         when HH505_HP =>
            Drums.HH_505_Sampled.Render
              (Buffer,
               Params => This.Params,
               Filter => This.Filter0,
               Env => This.Env0,
               Rng => This.Rng,
               Phase =>  This.Phase,
               Do_Init => This.Do_Init,
               Do_Strike => This.Do_Strike,
               Filter_Mode => Filters.SVF.High_Pass);

         when HHLM2_HP =>
            Drums.HH_LM2_Sampled.Render
              (Buffer,
               Params => This.Params,
               Filter => This.Filter0,
               Env => This.Env0,
               Rng => This.Rng,
               Phase =>  This.Phase,
               Do_Init => This.Do_Init,
               Do_Strike => This.Do_Strike,
               Filter_Mode => Filters.SVF.High_Pass);

         when HHMRK2_HP =>
            Drums.HH_MRK2_Sampled.Render
              (Buffer,
               Params => This.Params,
               Filter => This.Filter0,
               Env => This.Env0,
               Rng => This.Rng,
               Phase =>  This.Phase,
               Do_Init => This.Do_Init,
               Do_Strike => This.Do_Strike,
               Filter_Mode => Filters.SVF.High_Pass);

         when HHCR78_HP =>
            Drums.HH_CR78_Sampled.Render
              (Buffer,
               Params => This.Params,
               Filter => This.Filter0,
               Env => This.Env0,
               Rng => This.Rng,
               Phase =>  This.Phase,
               Do_Init => This.Do_Init,
               Do_Strike => This.Do_Strike,
               Filter_Mode => Filters.SVF.High_Pass);

         when HH_Acoustic_HP =>
            Drums.HH_Acoustic_Sampled.Render
              (Buffer,
               Params => This.Params,
               Filter => This.Filter0,
               Env => This.Env0,
               Rng => This.Rng,
               Phase =>  This.Phase,
               Do_Init => This.Do_Init,
               Do_Strike => This.Do_Strike,
               Filter_Mode => Filters.SVF.High_Pass);

      end case;
   end Render;

   -------------------
   -- Tresse_Engine --
   -------------------

   function Tresse_Engine (E : HH_Engine) return Tresses.Engines
   is (case E is
          when Cymbal => Tresses.Drum_Cymbal,
          when HH909  => Tresses.Drum_909_Hats,
          when HH707  => Tresses.Drum_707_Hats,
          when HH808  => Tresses.Drum_808_Hats,
          when HH505  => Tresses.Drum_505_Hats,
          when HHLM2  => Tresses.Drum_LM2_Hats,
          when HHCR78  => Tresses.Drum_CR78_Hats,
          when HHMRK2  => Tresses.Drum_MRK2_Hats,
          when HH_Acoustic  => Tresses.Drum_Acoustic_Hats,

          when HH909_BP  => Tresses.Drum_909_Hats_BP,
          when HH707_BP  => Tresses.Drum_707_Hats_BP,
          when HH808_BP  => Tresses.Drum_808_Hats_BP,
          when HH505_BP  => Tresses.Drum_505_Hats_BP,
          when HHLM2_BP  => Tresses.Drum_LM2_Hats_BP,
          when HHCR78_BP  => Tresses.Drum_CR78_Hats_BP,
          when HHMRK2_BP  => Tresses.Drum_MRK2_Hats_BP,
          when HH_Acoustic_BP  => Tresses.Drum_Acoustic_Hats_BP,

          when HH909_HP  => Tresses.Drum_909_Hats_HP,
          when HH707_HP  => Tresses.Drum_707_Hats_HP,
          when HH808_HP  => Tresses.Drum_808_Hats_HP,
          when HH505_HP  => Tresses.Drum_505_Hats_HP,
          when HHLM2_HP  => Tresses.Drum_LM2_Hats_HP,
          when HHCR78_HP  => Tresses.Drum_CR78_Hats_HP,
          when HHMRK2_HP  => Tresses.Drum_MRK2_Hats_HP,
          when HH_Acoustic_HP  => Tresses.Drum_Acoustic_Hats_HP);

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
