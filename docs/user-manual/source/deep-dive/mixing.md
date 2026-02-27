# Mixing

The PGB-1 provides essential mixing controls for each track, allowing you to
balance your sounds and add effects.

## Volume

### Track Volume

Located on page 4 of synth track settings.

- Controls the overall level of the track
- Use {kbd}`↑` / {kbd}`↓` or the touch strip to adjust

## Pan (Stereo Position)

Located on page 5 of synth track settings.

| Value | Position |
|-------|----------|
| Left | Sound in left channel |
| Center | Sound in both channels equally |
| Right | Sound in right channel |

Use panning to create width and separation in your mix.

### Master Volume

Hold {kbd}`Play` and press {kbd}`↑` / {kbd}`↓` to adjust the master output volume.

:::{admonition} Hearing Safety
:class: warning

Maximum volume can be very loud, especially with headphones. Prolonged exposure
to high volume may damage your hearing.
:::

## Track Effects

Located on page 6 of track settings. Each track can have one insert effect
applied. Effects are shared by all tracks.

 - None (Bypass): No effect applied. Use when you want the dry sound.
 - Reverb: Adds space and ambience. Creates a sense of room or hall.
 - Overdrive: Adds harmonic distortion and warmth.
 - Bitcrusher: Reduces bit depth and sample rate for lo-fi effects.
   Creates digital distortion.

## Effect controls

Mixing effects are controlled in [Track mode](../modes/track-mode.md) on
tracks 9 to 11.

Press and release {kbd}`Track` to enter Track mode, then press one of {kbd}`9`,
{kbd}`10`, {kbd}`11` to select an effect track.

Effects parameters can be modulated with a dedicated [LFO (Low Frequency
Oscillator)](lfo.md). Program [steps](../modes/step-mode.md) on effects tracks
to trigger the LFO synchronization.

### Reverb (Track 9)

The reverb is based on the Dattorro plate reverb algorithm, providing lush spatial effects.

| Parameter | Description |
|-----------|-------------|
| Amount | Wet/dry mix. Controls how much reverb is blended with the original signal. At minimum, you hear only the dry signal; at maximum, mostly reverb. |
| Time | Reverb decay time. Controls how long the reverb tail lasts before fading out. Higher values create longer, more sustained reverb. |
| Diffusion | Controls how the reflections are scattered. Low values create distinct echoes; high values create smoother, more diffuse reverb. |
| LP Cutoff | Low-pass filter on the reverb tail. Lower values darken the reverb by removing high frequencies, creating a warmer sound. |

### Overdrive (Track 10)

The overdrive uses waveshaping to add harmonic distortion and warmth to sounds.

| Parameter | Description |
|-----------|-------------|
| Pre-Gain | Input gain applied before distortion. Boosts the signal going into the overdrive, affecting how hard the distortion is driven. |
| Drive | Amount of distortion. Controls the intensity of the waveshaping effect. Higher values create more aggressive, saturated tones. |
| Pan | Stereo distribution of the drive effect. Center applies equal drive to both channels; left/right applies more drive to that channel while keeping the other cleaner. |
| Output Level | Final output gain. Use to compensate for volume changes caused by the drive settings. |

### Bitcrusher (Track 11)

The bitcrusher creates lo-fi digital distortion by reducing bit depth and sample rate.

| Parameter | Description |
|-----------|-------------|
| Depth | Bit depth reduction. Highervalues use fewer bits to represent the audio, creating more quantization noise and digital grit. |
| Downsampling | Sample rate reduction. Higher values skip more samples, creating aliasing artifacts and a more degraded, retro digital sound. |
| Cutoff | Low-pass filter applied to the crushed signal. Use to tame harsh high-frequency aliasing or shape the tone of the effect. |
| Mix | Wet/dry mix. Controls how much crushed signal is blended with the original. At minimum, you hear only the dry signal; at maximum, only the bitcrushed signal. |

Use LFO modulation on the Downsampling parameter to create a robotic sound
effect.

## Mixing Tips

### Creating Space

1. Keep kick and bass centered
2. Pan hi-hats slightly off-center
3. Spread melodic elements across the stereo field
4. Use reverb sparingly on low frequencies

### Balance

1. Start with all tracks at similar volumes
2. Adjust kick and snare as your foundation
3. Bring in bass to complement the kick
4. Add melodic elements at appropriate levels

### Using Effects

- Drive works well on kicks and bass for punch
- Reverb on snares adds depth
- Bitcrusher can make drums sound gritty
- Not every track needs an effect, sometimes dry is best
