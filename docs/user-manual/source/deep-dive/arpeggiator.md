# Arpeggiator

The arpeggiator automatically plays notes from the current chord in a pattern,
adding melodic movement to your tracks.

## How It Works

When a step is set to use the arpeggiator (in [Step
Mode](../modes/step-mode.md)), instead of playing a fixed note, it plays notes
from the current chord according to the arpeggiator settings.

## Arpeggiator and Chord Progressions

The arpeggiator automatically follows chord progressions:

1. Set up a chord progression in [Song Mode](../modes/song-mode.md)
2. Steps set to Arpeggiator will play notes from whatever chord is currently active
3. Your melody automatically harmonizes with the chords

## Arpeggiator Settings

Located on pages 9 and 10 of track settings.

### Mode (Page 9)

Determines the pattern of notes played:

| Mode | Description |
|------|-------------|
| Up | Plays notes from lowest to highest |
| Down | Plays notes from highest to lowest |
| Up and Down | Alternates between up and down |
| Pattern 1| Plays notes in set order: 1, 4, 3, 4, 2, 4, 3, 4 |
| Pattern 2| Plays notes in set order: 1, 2, 3, 4, 1, 4, 3, 4 |
| Pattern 3| Plays notes in set order: 1, 3, 2, 4, 3, 2, 3 |
| Random | Plays notes in random order |

### Notes (Page 10)

Select which notes the arpeggiator uses. Currently, chord notes are the only
option.

## Using the Arpeggiator

### In Step Settings

Each step can be set to use different note modes:

1. Enter [Step Mode](../modes/step-mode.md)
2. Select a step
3. Navigate to the Note setting
4. Press {kbd}`A` to cycle through modes until "Arpeggiator" is selected

### Default Track Behavior

- **Bass Track**: Steps play the chord root note by default
- **Lead Track**: Steps use the arpeggiator by default
- **Chords Track**: Steps play the full chord

## Creative Uses

### Melodic Leads

1. Select Lead track
2. Set Arpeggiator Mode to Random
3. Create a pattern with multiple steps
4. The melody will vary with each chord change

### Controlled Melodies

Mix arpeggiator steps with fixed note steps:

1. Set important melody notes (step 1, 5, etc.) to specific chord notes
2. Set fill notes to Arpeggiator
3. This creates melodies that always land on key notes

## Tips

- Use Random arpeggiator mode for generative melodies
- Set key melody notes to specific chord notes for control
- The arpeggiator respects the track's octave setting
- Combine with [step conditions](../modes/step-mode.md) for even more variation
