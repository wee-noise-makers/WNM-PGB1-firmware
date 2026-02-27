# LFO (Low Frequency Oscillator)

Each synth and FX track has its own LFO that can automatically modulate one
parameter over time, adding movement and expression to your sounds.

## Accessing LFO Settings

1. Enter Track Mode (press {kbd}`Track`)
2. Select a track
3. Navigate to page LFO settings page (3rd for synth tracks, 2nd for FX tracks)

## LFO Parameters

### Rate

Controls the speed of the LFO oscillation.

- Lower values = slower modulation
- Higher values = faster modulation

### Amplitude

Controls how much the LFO affects the target parameter.

- 0 = No effect
- Higher values = More dramatic modulation

There are three modes that control the direction and range of modulation:

| Mode        | Direction  | Use Case |
|-------------|------------|----------|
| **Add**     | Base → Max | Filter opening, pitch bend up |
| **Bipolar** | Min <-> Max (around base) | Vibrato, tremolo, classic modulation |
| **Sub**     | Base → Min | Filter closing, ducking effects |

#### Add(Positive)

Modulation adds to the target parameter value. The LFO output ranges from the
base value upward.

```
Parameter
Value
  ^
  |     /\      /\
  |    /  \    /  \
  |   /    \  /    \
  |  /      \/      \
  +-------------------> Time
  Base
```

Use this when you want the parameter to increase from its set value. For
example, if filter cutoff is set to 50%, the LFO will sweep from 50% up toward
100%.

#### Bipolar (Center)

Modulation oscillates around the target parameter value, going both above and
below.

```
Parameter
Value
  ^
  |   /\      /\
  | /   \   /   \
--+------\-/------\-/---> Time
  |       V        V
  Base (center)
```

Use this for classic vibrato and tremolo effects where the parameter swings
symmetrically around its set value. For example, if pitch is set to a note, the
LFO will create vibrato by going sharp and flat around that pitch.

#### Sub (Negative)

Modulation subtracts from the target parameter value. The LFO output ranges
from the base value downward.

```
Parameter
Value
  +-------------------> Time
  Base
  |  \      /\      /
  |   \    /  \    /
  |    \  /    \  /
  |     \/      \/
  v
```

Use this when you want the parameter to decrease from its set value. For
example, if filter cutoff is set to 80%, the LFO will sweep from 80% down
toward 0%.

#### Navigating Between Amplitude Modes

The amplitude mode work together as a single continuous control. Starting in
Add (Positive) mode lower the amplitude to reach zero and then press the
{kbd}`↓` button, this will switch to Bipolar (Center) mode. Continue to press
down to reach zero in Bipolar mode, then press the {kbd}`↓` button again, to
switch to Sub (Negative) mode.

```
<── Increase                                            Decrease ──>

|-------- Add --------|-------- Bipolar --------|-------- Sub -------|
  Strong         Weak   Strong             Weak   Weak        Strong
```

### Shape

Selects the waveform shape used for modulation:

| Shape | Description |
|-------|-------------|
| Sine | Smooth, natural modulation |
| Triangle | Linear rise and fall |
| Saw Up | Rising ramp |
| Saw Down | Falling ramp |
| Expo Up | Rising exponential ramp |
| Expo Down | Falling exponential ramp |

#### Shape Modifiers

The Shape setting also includes two important modifiers indicated by small letters:

**S - Sync**

- When enabled, the LFO resets every time a note is played
- Useful for consistent modulation at note (like an envelope)

**L - Loop**

- When enabled, the LFO continuously cycles
- When disabled, the LFO runs once per note (like an envelope)

### Target

Selects which parameter the LFO modulates:

| Target | Effect |
|--------|--------|
| P1 | Modulates engine parameter 1 |
| P2 | Modulates engine parameter 2 |
| P3 | Modulates engine parameter 3 |
| P4 | Modulates engine parameter 4 |
| Volume | Modulates track volume (tremolo) |
| Pan | Modulates stereo position (auto-pan) |

## LFO Modes

By combining Sync and Loop settings, you can create different behaviors:

| Sync | Loop | Behavior |
|------|------|----------|
| Off | On | Free-running continuous LFO |
| On | On | LFO resets on each note, then loops |
| On | Off | **Envelope mode** - runs once per note |
| Off | Off | Free-running single cycle (LFO is disabled at the end of the running cycle) |

### Envelope Mode

When Sync is ON and Loop is OFF, the LFO acts like an envelope:

- Triggers on each note
- Runs through one cycle
- Stays at final value until next note
