# Song Mode

Song Mode is unique because it controls two things: **Song Parts**
(arrangement) and **Chord Progressions** (harmony).

## Entering Song Mode

Press {kbd}`Song` to enter Song Mode. The Song LED will light up.

## Song Parts (Buttons 1-12)

Song Parts are sections (intro, chorus, etc.) of your song where you can
specify which patterns play on which tracks, and which track to mute.

### Selecting Song Parts

- Press {kbd}`1` to {kbd}`12` to select and **queue** a song part
- The queued part will start playing after the current part completes
- Hold {kbd}`Song` and press {kbd}`1` to {kbd}`12` to select **without** queuing

### Song Part Settings

#### Pattern Grid

The screen displays a 16-cell grid representing all tracks:


```{figure} ../assets/images/song-mode-screenshot.png
:alt: Song mode on screen
:width: 400px
```



Each cell shows which pattern will play for that track:

- **Number (1-16)**: The pattern that will play
- **Empty**: Track is muted for this part

Use {kbd}`↑` / {kbd}`↓` to change the pattern number.
Press {kbd}`B` to go to the bottom line of cells, {kbd}`A` to go up

#### Song Part Settings (Bottom of Screen)

Press {kbd}`B` to move focus to the bottom settings:

| Setting | Description |
|---------|-------------|
| Length | How many times the part repeats before allowing change |
| Chords | Which chord progression to use (13-16) |
| Link | Auto-advance to next part when complete, got back to queued part, or loop on the this part |

## Chord Progressions (Buttons 13-16)

The PGB-1 has four chord progression slots (13, 14, 15, 16).

### Selecting Chord Progressions

Press {kbd}`13` to {kbd}`16` to enter chord progression editing.

### Chord Settings

Each chord in the progression has:

| Setting | Description |
|---------|-------------|
| Root | The root note (C, C#, D, etc.) |
| Quality | Chord type (Major, Minor, etc.) |
| Length | Duration in steps |

### Adding Chords

1. Navigate to the last chord in the progression
2. Press {kbd}`→` to go past the last setting
3. Press {kbd}`A` to add a new chord

### Removing Chords

1. Navigate to a chord
2. Go to the add/remove page
3. Press {kbd}`B` to remove the chord

### Random Generation

The last page offers random chord progression generation:

1. Use {kbd}`↑` / {kbd}`↓` to select a scale (Major, Minor, Modal)
2. Press {kbd}`A` to generate a random progression
3. Press {kbd}`A` again to try different progressions

## Chord Progression Indicator

While playing, small dots and an arrow on screen show your position in the
chord progression. This helps you time part changes to land on specific chords.

The indicator also appears in the Cpy/FX menu for timing live effects.

## Tips

- Use the random chord generator for inspiration
- Use different chord progressions for different song sections
- The bass track automatically follows chord root notes
- Lead tracks use the arpeggiator over the current chord
- Chords track is the only polyphonic track that plays full chords
