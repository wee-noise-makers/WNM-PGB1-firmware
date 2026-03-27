# Live FX

The PGB-1 features 16 live effects that can be activated during playback for
dynamic performances.

## Accessing Live FX

Hold {kbd}`Cpy/FX` and press {kbd}`1` to {kbd}`16` to toggle effects.

## Effect Categories

Effects are split into two categories:

- **Sequencing Effects** (left side: {kbd}`1`-{kbd}`4`, {kbd}`9`-{kbd}`12`) -
  Affect pattern playback

- **Mixing Effects** (right side: {kbd}`5`-{kbd}`8`, {kbd}`13`-{kbd}`16`) -
  Affect audio output

## Sequencing Effects

### Roll Effects

Create note repeats at different speeds:

| Button | Effect | Description |
|--------|--------|-------------|
| {kbd}`1` | Roll 16th | Fast rolls (16th notes) |
| {kbd}`2` | Roll 8th | Medium rolls (8th notes) |
| {kbd}`3` | Roll Quarter | Slow rolls (quarter notes) |
| {kbd}`4` | Roll Beat | Slowest rolls (whole beat) |

Press the button to start the effect, press again to stop. When a roll effect
is enable, you can press another roll effect button to directly switch to
another speed.

### Fill Effects

Add or modify steps automatically:

| Button | Effect | Description |
|--------|--------|-------------|
| {kbd}`9`  | Fill Steps | Triggers steps with ["Fill" condition](../modes/step-mode.md) on any tracks |
| {kbd}`10` | Auto-fill Low | Random fills on drum tracks, lower probability |
| {kbd}`11` | Auto-fill High | Random fills on drum tracks, higher probability |
| {kbd}`12` | Auto-fill Build-up | Increasing random fill intensity  on drum tracks |

:::{important}

The auto-fill effects work on selected tracks (Kick, Snare, Hihat by default)
while the Fill Steps can be used on any track.

:::

:::{tip}
You can configure the auto-fill tracks in `Menu -> Live FX Settings`.
:::

## Mixing Effects

### Filter Effects

Apply filters to the master output:

| Button | Effect | Description |
|--------|--------|-------------|
| {kbd}`5` | Low Pass Filter | Removes high frequencies |
| {kbd}`6` | Band Pass Filter | Removes highs and lows |
| {kbd}`7` | High Pass Filter | Removes low frequencies |

:::{tip}
You can configure the cutoff frequency and resonance of each filter in
`Menu -> Live FX Settings`.
:::

### Filter Sweeps

Animated filter effects:

| Button | Effect | Description |
|--------|--------|-------------|
| {kbd}`13` | LP Sweep | Low pass filter that sweeps |
| {kbd}`14` | BP Sweep | Band pass filter that sweeps |
| {kbd}`15` | HP Sweep | High pass filter that sweeps |

:::{tip}
You can configure the frequency and amplitude of filter sweep in
`Menu -> Live FX Settings`.
:::

### Stutter Effects

| Button | Effect | Description |
|--------|--------|-------------|
| {kbd}`8` | Stutter A | Short audio repeat effect |
| {kbd}`16` | Stutter B | Variation of stutter |

:::{tip}
You can configure the stutter patterns in `Menu -> Live FX Settings`.
:::

## Live FX Settings

*Available from firmware 1.3.0*

You can configure various parameters of the live effects in a dedicated menu.
Press {kbd}`menu` and navigate to the `Live FX Settings` page, press {kbd}`A`.

Navigate between the different pages to customize live effects:

 - **Auto-Fill Tracks**: Select which track is triggered by the auto-fill
   effects ({kbd}`10`, {kbd}`11`, {kbd}`12`). Press {kbd}`A` to start editing
   the selected tracks, use {kbd}`←` and {kbd}`→` to navigate between the
   different track, use {kbd}`↑` to enable auto-fill for the selected track,
   use {kbd}`↓` to disable auto-fill on the selected track, press {kbd}`A` or
   {kbd}`B` to end the edit.

 - **Auto-Fill Low*: Change the trigger probability of auto-fill low effect
   ({kbd}`10`)

 - **Auto-Fill High**: Change the trigger probability of auto-fill high effect
   ({kbd}`11`)

 - **Auto-Fill Build-up**: Change the start trigger probability of auto-fill
   build-up effect({kbd}`12`)

 - **Low-Pass**: Change the cutoff frequency and resonance of the low-pass
   ({kbd}`5`) and low-pass-sweep ({kbd}`13`) effects

 - **Band-Pass**: Change the cutoff frequency and resonance of the band-pass
   ({kbd}`6`) and band-pass-sweep ({kbd}`14`) effects

 - **High-Pass**: Change the cutoff frequency and resonance of the high-pass
   ({kbd}`7`) and high-pass-sweep ({kbd}`15`) effects

 - **Auto-Filter Sweep**: Change the rate and amplitude of filter sweeps
   effects ({kbd}`13`, {kbd}`14`, {kbd}`15`)

 - **Stutter A**: Change the pattern of the first stutter effect ({kbd}`8`).
   Press {kbd}`A` to start editing the pattern, use {kbd}`←` and {kbd}`→` to
   navigate between the different steps, use {kbd}`↑` to unmute the selected
   step, use {kbd}`↓` to mute the selected step, press {kbd}`A` or {kbd}`B` to
   end the edit.

 - **Stutter B**: Change the pattern of the second stutter effect ({kbd}`16`).
   See **Stutter A** just above for edit instructions.

 - **Stutter Envelope**: Change the attack (how fast the volume drops) and
   release (how fast sound comes back up) of the two sutter effects ({kbd}`8`
   and {kbd}`16`)

## One Effect Per Category

Only one effect of the same type can be active at a time:

- Enabling a filter cancels other filters
- Enabling a roll cancels other rolls
- Enabling a fill cancels other fills

## Combining Effect Categories

You can combine effects from different categories:

- Roll + Filter
- Fill + Stutter
- Filter Sweep + Roll
- Etc.

## Chord Progression Indicator

While holding {kbd}`Cpy/FX`, the [chord progression](../modes/song-mode.md)
indicator appears on screen. Use this to time your effects to land on specific
chord changes.

## Quick Parameter Control

While holding {kbd}`Cpy/FX`, the touch strip controls a single parameter for a
selected track. This gives you quick and direct access to one parameter for
live performance without having to navigate in the track settings.

With this feature you can control one of:

 - Synth parameter 1
 - Synth parameter 2
 - Synth parameter 3
 - Synth parameter 4
 - LFO Rate
 - LFO Amplitude
 - Volume
 - Stereo Pan
 - Shuffle

To change the selected track, use the {kbd}`←` or {kbd}`→` buttons while
holding {kbd}`Cpy/FX`.

To change the selected parameter, use the {kbd}`↑` or {kbd}`↓` buttons while
holding {kbd}`Cpy/FX`.

Use the touch strip to change the value of the selected parameter.

## Performance Tips

### Plan Your FX

- Map out when to use effects in your arrangement
- Practice transitions between song parts
- Know which effects complement each other

### Less Is More

- Subtle filter movements can be very effective
- Don't overuse rolls - save them for impact moments
- Brief stutters are often more effective than sustained ones

### Build and Release

- Use filters and fills to build tension
- Release effects on strong beats
- Combine with song part changes for maximum impact

## See Also

- [Sound Path](../audio-midi/sound-path.md) - Where Live FX fit in the audio chain
- [Trigger Conditions](../modes/step-mode.md) - Setting up Fill conditions
- [Chord Progressions](../modes/song-mode.md) - Using the chord indicator
- [Shortcuts](shortcuts.md) - Quick reference for all controls
