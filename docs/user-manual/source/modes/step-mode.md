# Step Mode

Step mode allows you to configure each of the 16 steps for the selected pattern
of the selected track, giving you detailed control over each note's parameters.
In Step mode, pressing one of the {kbd}`1` to {kbd}`16` buttons will select the
corresponding step and trigger it at the same time.

In step mode, you are always editing steps of the selected pattern (see
[Pattern mode](pattern-mode.md)) The numbers of the selected pattern and step
is shown at the top right of the screen:


```{figure} ../assets/images/current-pattern-and-step-screenshot.png
:alt: Current pattern and step on screen
:width: 400px
```



## Entering Step Mode

Press {kbd}`Step` to enter Step Mode. The Step LED will light up.

## Selecting Steps

In Step Mode:

- Press {kbd}`1` to {kbd}`16` to select a step and trigger it
- The selected pattern and step numbers appear at the top right of the screen

To select a step **without** triggering it:

- Hold {kbd}`Step` and press {kbd}`1` to {kbd}`16`

## Step Settings Pages

Navigate between pages using {kbd}`←` and {kbd}`→`:

### Page 1: Trigger Condition

Determines when the step plays.

| Condition | Description |
|-----------|-------------|
| Never | Step never plays |
| Percent (25%, 50%, 75%) | Random chance to play |
| Always | Step always plays |
| Fill | Plays when Fill FX is active |
| Not Fill | Plays when Fill FX is not active |
| X:Y (1:2, 2:2, 1:3, 2:3, etc.) | Plays on the Xth occurrence within every Y pattern loops |

Some examples of Xth within Y pattern loops:

| Condition | Loop 1 | Loop 2 | Loop 3 | Loop 4 | Loop 5 | Loop 6 |   
|-----------|:------:|:------:|:------:|:------:|:------:|:------:|   
| 1:2       | ✓      | ·      | ✓      | ·      | ✓      | ·      |   
| 2:2       | ·      | ✓      | ·      | ✓      | ·      | ✓      |   
| 1:3       | ✓      | ·      | ·      | ✓      | ·      | ·      |   
| 2:3       | ·      | ✓      | ·      | ·      | ✓      | ·      |   

(✓ = step plays, · = step is skipped)

### Page 2: Note Settings

#### Note

Set what note the step plays:

| Option | Description |
|--------|-------------|
| Fixed Note | Play a specific MIDI note regardless of current chord|
| Note in Chord (1-4) | Play specific note from the currernt chord |
| Arpeggiator | Use the track's arpeggiator setting |
| Chord | Play the full current chord |

Use {kbd}`A` to cycle through note modes.

Default Track Behavior:

- **Bass Track**: Steps play the chord root note by default
- **Lead Track**: Steps use the arpeggiator by default
- **Chords Track**: Steps play the full chord

:::{admonition} Octave Offset
:class: note

In note modes "Note in Chord", "Arpeggiator", and "Chord", the notes can be
shifted up or down by one or multiple octaves. This is shown as, for
example, `+1oct` or `-2oct`.
:::

:::{admonition} Playing full chords
:class: note

From the internal synth engines, only the Chord track can play multiple
notes simultaneously (polyphony). Therefore it's the only internal track
where the "Chord" note mode makes sense. However, you can use "Chord" note
mode with external polyphonic MIDI instruments.
:::

#### Duration

Set how long the note plays.

#### Velocity

Set the note's intensity/volume. Use the touch strip for precise control.

### Page 3: Repeat

Create note repeats (retrigs) within the step:

| Parameter | Description |
|-----------|-------------|
| Count | Number of repeats |
| Rate | Speed of repeats |

### Page 4: Parameter Locks

Parameter locks allow you to set different synth parameters for individual
steps, enabling sound variations within a single pattern. Normally, all steps
in a pattern use the track's synth settings, parameter locks let you override
specific parameters on a per-step basis.

This is especialy powerful for sample tracks where you can select different
samples per step.

Press {kbd}`↑`/{kbd}`↓` or use the touch strip to set the value of the selected
parameter lock. Use {kbd}`B` to disable the selected parameter lock on the step.

## Working with Steps

### Enabling/Disabling Steps

1. Press {kbd}`Edit` to enter edit mode
2. Press {kbd}`1` to {kbd}`16` to toggle steps on/off
3. Press {kbd}`Edit` again to exit edit mode

## Tips

 - Use velocity variations to create groove and dynamics Use parameter locks to
 - play different samples on each step of a [sample track](../deep-dive/sample-tracks.md)
 - Use Fill conditions to add controlled complexity during performance
 - Creates subtle, varying ghost notes:
    - Add hi-hat or snare steps
    - Set condition to 50% or 75%
    - Set velocity low
