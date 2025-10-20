with Tresses; use Tresses;
with MIDI; use MIDI;

package WNM.Synth_Engines
with Elaborate_Body
is

   subtype Lead_Engine_Range is MIDI.MIDI_Data range 0 .. 27;

   Tresse_To_MIDI : constant array (Tresses.Engines) of MIDI.MIDI_Data
     := (Voice_Saw_Swarm            => 0,
         Voice_Analog_Buzz          => 1,
         Voice_Analog_Morph         => 2,
         Voice_FM2OP                => 3,
         Voice_Sand                 => 4,
         Voice_Bass_808             => 5,
         Voice_House_Bass           => 6,
         Voice_Pluck_Bass           => 7,
         Voice_Reese                => 8,
         Voice_Screech              => 9,
         Voice_Plucked              => 10,
         Voice_PDR_Sine             => 11,
         Voice_PDR_Triangle         => 12,
         Voice_PDR_Sine_Square      => 13,
         Voice_PDR_Square_Sine      => 14,
         Voice_PDL_Trig_Warp        => 15,
         Voice_PDL_Triangle_Screech => 16,
         Voice_Chip_Glide           => 17,
         Voice_Chip_Phaser          => 18,
         Voice_Chip_Echo_Square     => 19,
         Voice_Chip_Echo_Square_Saw => 20,
         Voice_Chip_Bass            => 21,
         Voice_PDR_Square_Full_Sine => 22,
         Voice_Triangle_Phaser      => 23,
         Voice_Sine_Phaser          => 24,
         Voice_Sine_Pluck           => 25,
         Voice_Triangle_Pluck       => 26,
         Voice_Chip_Pluck           => 27,
         Drum_Kick                  => 28,
         Drum_Sine_Kick             => 29,
         Drum_Triangle_Kick         => 30,
         Drum_Chip_Kick             => 31,
         Drum_Snare                 => 32,
         Drum_Sine_Snare            => 33,
         Drum_Saw_Snare             => 34,
         Drum_Triangle_Snare        => 35,
         Drum_Clap                  => 36,
         Drum_Cymbal                => 37,
         Drum_Percussion            => 38,
         Drum_Bell                  => 39,
         Drum_909_Hats              => 40,
         Drum_707_Hats              => 41,
         others => MIDI_Data'Last);

   function MIDI_To_Tresses (E : MIDI_Data) return Tresses.Engines;

   type Synth_Categories is (Chip, Pluck, Kick, Snare, Hats, Misc_Drums);

   type Cat_List is array (Natural range <>) of Tresses.Engines;

   Kick_List : aliased constant Cat_List :=
     (Drum_Sine_Kick,
      Drum_Triangle_Kick,
      Drum_Chip_Kick,
      Drum_Kick);

   Snare_List : aliased constant Cat_List :=
     (Drum_Snare,
      Drum_Sine_Snare,
      Drum_Saw_Snare,
      Drum_Triangle_Snare,
      Drum_Clap);

   Hats_List : aliased constant Cat_List :=
     (Drum_909_Hats,
      Drum_707_Hats,
      Drum_Cymbal);

   Misc_Drums_List : aliased constant Cat_List :=
     (Drum_Bell,
      Drum_Percussion,
      Drum_Clap,
      Drum_Cymbal);

   Chip_List : aliased constant Cat_List :=
     (Voice_Chip_Glide,
      Voice_Chip_Phaser,
      Voice_Chip_Echo_Square,
      Voice_Chip_Echo_Square_Saw,
      Voice_Chip_Bass,
      Voice_Chip_Pluck,
      Drum_Chip_Kick);

   Pluck_List : aliased constant Cat_List :=
     (Voice_Sine_Pluck,
      Voice_Triangle_Pluck,
      Voice_Chip_Pluck);

   Engines : constant array (Synth_Categories)
     of not null access constant Cat_List :=
       (Chip       => Chip_List'Access,
        Pluck      => Pluck_List'Access,
        Kick       => Kick_List'Access,
        Snare      => Snare_List'Access,
        Hats       => Hats_List'Access,
        Misc_Drums => Misc_Drums_List'Access);

end WNM.Synth_Engines;
