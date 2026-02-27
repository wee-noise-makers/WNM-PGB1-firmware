# Track Mode

Track mode allows you to configure the 16 tracks of the PGB-1. By default,
tracks 1 to 6 are synths tracks (Kick, Snare, Hihat, Bass, Lead, Chords),
tracks 7 and 8 are sample tracks, tracks 9 to 11 are audio FX tracks, and
tracks 12 to 16 are MIDI tracks. Note that all tracks can be configured to MIDI
mode and control external devices ([more details here](../audio-midi/midi.md)).
In Track mode, pressing one of the {kbd}`1` to {kbd}`16` buttons will select the
corresponding track and trigger a "preview" note for this track. The name of
the selected track is shown at the top left of the screen.



```{figure} ../assets/images/track-name-screenshot.png
:alt: Track name on screen
:width: 400px
```



## Entering Track Mode

Press {kbd}`Track` to enter Track Mode. The Track LED will light up.

## Default Track Assignment

| Track | Default Instrument |
|-------|-------------------|
| 1 | Kick |
| 2 | Snare |
| 3 | Hi-Hat |
| 4 | Bass |
| 5 | Lead |
| 6 | Chords |
| 7-8 | Sample |
| 9-11 | Audio FX |
| 12-16 | MIDI |

:::{admonition} Track Flexibility
:class: note

All tracks can be configured for MIDI mode to control external devices.
:::

## Selecting Tracks

In Track Mode:

- Press {kbd}`1` to {kbd}`16` to select a track and trigger a preview note
- The selected track name appears at the top left of the screen

To select a track **without** playing a preview:

- Hold {kbd}`Track` and press {kbd}`1` to {kbd}`16`

## Track Settings Pages

Navigate between pages using {kbd}`←` and {kbd}`→`. Each track has multiple
settings pages, and different pages depending on the track type (synth, FX,
MIDI).

Here is the list of pages for synth tracks:

### Page 1: Synth Engine

Select the sound engine for the track. Use {kbd}`↑` and {kbd}`↓` to scroll
through available engines. See [Synth Engines](../reference/engines.md) for a
complete list.

### Page 2: Engine Parameters

Four parameters specific to the selected synth engine. Parameters vary by engine (e.g., Shape, Decay, Cutoff, Resonance).

### Page 3: LFO

Configure the [Low Frequency Oscillator](../deep-dive/lfo.md):

| Parameter | Description |
|-----------|-------------|
| Rate | Speed of the LFO |
| Amplitude | Intensity of modulation |
| Shape | Waveform shape + Sync (S) and Loop (L) options |
| Target | Which parameter the LFO modulates |

### Page 4: Volume

Set the track volume level.

### Page 5: Pan

Set the stereo position of the track.

### Page 6: Effect

Select a global mixing effect for the track:

- None (bypass)
- Overdrive
- Reverb
- Bitcrusher

### Page 7: Octave

Shift every notes of the track up or down by octaves.

### Page 8: Shuffle

Add swing to the track timing. Higher values delay even-numbered steps.

### Page 9: Arpeggiator Mode

Set how the [arpeggiator](../deep-dive/arpeggiator.md) plays chord notes:

- Up
- Down
- Up/Down
- Random
- Order

### Page 10: Arpeggiator Notes

Configure which notes the [arpeggiator](../deep-dive/arpeggiator.md) uses.

### Page 11: Track Mode

Switch between internal synth mode and external MIDI controller mode.

## Tips

- Use the touch strip for precise parameter adjustment
- Preview sounds while adjusting parameters by pressing track buttons

## See Also

- [Synth Engines](../reference/engines.md) - Complete engine list
- [Mixing](../deep-dive/mixing.md) - Volume, panning, and effects
- [Sound Path](../audio-midi/sound-path.md) - How audio flows through the PGB-1
