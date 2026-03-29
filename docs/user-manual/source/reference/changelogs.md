# Firmware Changelogs

## v1.3.0

### New features

 - Project Mixer menu
   - New audio FX gains and output gain
   - Visualization of peak signal values for all track and FX
   - Common view and control of volume and pan for all track
 - Live FX settings menu
   - Select tracks controlled by auto-fill
   - Set auto-fill probabilities
   - Cutoff and resonance for low-pass, band-pass, and high-pass filters
   - Speed and amplitude of filter sweep
   - Stutter patterns
   - Stutter attack and decay
 - LFO improvements
   - New shape: random
   - Graphical visualization of LFO modulation
   - Use A/B buttons to change amplitude mode and toggle loop/sync

### New sound engines

 - Hihat Track
   - 909 HiHat (low-pass)
   - 707 HiHat (low-pass)
   - 808 HiHat (low-pass)
   - 505 HiHat (low-pass)
   - LM2 HiHat (low-pass)
   - CR78 HiHat (low-pass)
   - MRK2 HiHat (low-pass)
   - Acoustic HiHat (low-pass)
   - 909 HiHat (band-pass)
   - 707 HiHat (band-pass)
   - 808 HiHat (band-pass)
   - 505 HiHat (band-pass)
   - LM2 HiHat (band-pass)
   - CR78 HiHat (band-pass)
   - MRK2 HiHat (band-pass)
   - Acoustic HiHat (band-pass)
   - 909 HiHat (high-pass)
   - 707 HiHat (high-pass)
   - 808 HiHat (high-pass)
   - 505 HiHat (high-pass)
   - LM2 HiHat (high-pass)
   - CR78 HiHat (high-pass)
   - MRK2 HiHat (high-pass)
   - Acoustic HiHat (high-pass)
 - Sampler 1/2 Track
   - Glide (Fast)
   - Glide (Slow)
   - Pitch Down (1-Octave)
   - Pitch Down (2-Octave)
   - Pitch Up (1-Octave)
   - Pitch Up (2-Octave)

### Bug fixes

 - Fix octave offset for note repeats in ARP mode
 - Fix LFO bipolar mode
 
## v1.2.0

[Update video here.](https://www.youtube.com/watch?v=uRXs9qWJlPk)

### New features

The biggest change in 1.2.0 is the ability to draw a single cycle waveform
(Menu -> Custom Waveform) with the addition of many new synth engines using
this user drawn waveform (see below).

### New sound engines

 - Kick Track
    - Sine Click Kick (similar to the Sine Kick with an additional noise transient)
    - Custom Waveform Kick
    - Custom Waveform Click Kick
 - Snare Track
    - Clap (high-pass)
    - Custom Waveform Snare
 - Bass and Lead Tracks
    - Custom Waveform Glide
    - Custom Waveform Phaser
    - Custom Waveform Pluck
    - Custom Waveform Echo
    - Custom Waveform PDR (Phase Distortion Resonance)
  - Chord Tracks
    - Mixed Waveforms (each oscillator has a different waveform)
    - Custom Waveform

### Quality of Life

 - Improved graphical interface with more icons in menus, and additional
 - information for some settings
 - A chromatic keyboard (activated by holding the "Edit" button in track and
 - step mode) to play the selected track and change the notes of the current
 - step when in edit mode

## v1.1.0

### New synth engines:

 - PDR Square Full Sine (Phase Distortion Resonance)
 - Triangle Phaser
 - Sine Phaser
 - Sine Pluck
 - Triangle Pluck
 - Chip Pluck

## Quality of Life

 - Better precision and stability of the touch strip values
 - When a new chord is added to a progression, the view jumps automatically to edit that chord
 - The chord progression hint is now displayed on the CPY/FX page. This is to help to time effects with chord changes.
 - Reduced pop/click at startup

### Bug fixes

 - Fix stereo line-in (only the left channel was enabled)
 - Fix sound quality for sample recording
 - Fix synth engine names going outside the screen




