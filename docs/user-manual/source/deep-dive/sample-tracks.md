# Sample Tracks

The PGB-1 has two dedicated sample tracks (Tracks 7 and 8) that play audio
samples instead of synthesized sounds.

## Track Default Sample

1. Enter Track Mode ({kbd}`Track`)
2. Select track 7 or 8
3. Navigate to the sample selection page (page 2, first parameter)
4. Use {kbd}`↑` / {kbd}`↓` to choose a sample (1-64)

This sets the default sample for the track.

## Per-Step Sample Selection

Using [parameter locks](../modes/step-mode.md), each step can play a different
sample:

1. Enter Step Mode ({kbd}`Step`)
2. Select a step
3. Navigate to the 4th page
4. Choose a sample for this step

Use this technique to maximize the usage of sample tracks.

## Multiple Sounds in One Sample

You can create samples with multiple sounds in them. Either [directly on the
PGB-1](../modes/sample-mode.md) or using a sound editing software and
[importing the sample](importing-samples.md).

Use start point [parameter locks](../modes/step-mode.md) to select different
sounds for each step.

## Extended Sample Length Technique

The PGB-1 samples have a fixed length of 2 seconds. However, you can
effectively double the available length using a speed/pitch trick.

### How It Works

1. Take a sample between 2 and 4 seconds (e.g. a drump loop)
1. On your computer, use an audio software (such as Audacity) to double the
   playback speed of your sample (200% speed or +12 semitones pitch shift with
   time correction disabled). The sped-up sample now fits in half the time/memory.
1. [Import to PGB-1](importing-samples.md)
1. Play one octave down. Set the sample track's Octave setting to -1, or use
   note settings to play at half the original pitch

The result: the sample plays back at its original pitch and length, but uses
only half the sample slot memory.

### Trade-offs

- Advantage: Double the effective sample length. Fit longer loops, vocals, or pads.
- Disadvantage: Reduced audio quality (half the sample rate). High frequencies may sound duller or aliased.

## See Also

- [Recording Samples](../modes/sample-mode.md) - How to record your own samples
- [Importing Samples](importing-samples.md) - Load samples from your computer
- [Parameter Locks](../modes/step-mode.md) - Per-step sample selection
- [Sound Path](../audio-midi/sound-path.md) - How sample audio is processed
