# Sample Mode

Sample Mode allows you to record and edit samples directly on the PGB-1.

## Entering Sample Mode

Hold {kbd}`Cpy/FX` and press {kbd}`Edit` to enter Sample Mode.

## Sample Mode Options

When entering Sample Mode, you can choose:

- **New Sample**: Record a new sample
- **Edit Sample**: Modify an existing sample's start/end points and name

## Recording a New Sample

### Step 1: Select Input Source

Choose which input to record from using the {kbd}`←` and {kbd}`→` buttons:

| Input | Description | Best For |
|-------|-------------|----------|
| Line In | 3.5mm stereo jack input | External devices, synths, phones |
| Internal Mic | Built-in microphone | Voice, ambient sounds |
| Headset Mic | Microphone on connected headset | Voice recording |

Use {kbd}`↑` / {kbd}`↓` to adjust the input volume before recording.

:::{admonition} Best Practice
:class: tip

For line input, keep the PGB-1 gain low and increase the volume on your
source device. This provides more headroom and less noise.
:::

### Step 2: Start Recording

Press {kbd}`A` to begin recording. The PGB-1 uses a **2-second rolling buffer**:

- Recording continuously captures audio
- When buffer is full, new audio overwrites the oldest
- This lets you capture a sound even slightly after it happens

### Step 3: Stop Recording

Press {kbd}`A` again to stop recording. The captured audio is displayed as a waveform.

### Step 4: Edit Start/End Points

- Use {kbd}`←` / {kbd}`→` to move the start point
- Use {kbd}`↑` / {kbd}`↓` to move the end point

The keyboard lights up showing available preview keys:

| Button | Action |
|--------|--------|
| {kbd}`1` | Preview one octave down |
| {kbd}`4` | Preview at original pitch |
| {kbd}`8` | Preview one octave up |
| Other keys | Preview at different pitches |

Press {kbd}`A` to continue, or {kbd}`B` discard the current sample and start
recording again.

### Step 6: Set Name

Enter a name for the sample (up to 15 characters).

Press {kbd}`A` to continue.

### Step 7: Save

2. Select a sample slot (1-64)
3. Press {kbd}`A` to confirm
4. Navigate to **Yes** and confirm

:::{warning}
Saving to a slot will overwrite any existing sample in that slot.
:::

## Editing Existing Samples

1. Enter Sample Mode
2. Select **Edit Sample**
3. Choose the sample to edit
4. Adjust start point, end point, and name
5. Save when finished

## Sample Tips

### Recording Quality

- Use line in when possible for cleaner signals
- For line input, keep PGB-1 gain low and increase volume on the source device.
  This provides better headroom and less noise
- Use the waveform display to check levels

### Multiple Sounds in One Sample

You can record multiple sounds in a single sample:

1. Start recording
2. Play sound 1
3. Wait briefly
4. Play sound 2
5. Stop recording

Then use [parameter locks](step-mode.md) to set different start points per
step, effectively getting multiple samples from one slot. This technique helps
maximize usage of the sample memory.

### Sample Slots

The PGB-1 has 64 sample slots. Samples are shared across projects and persist
when the device is turned off.

## Converting Audio Files

You can import audio files from your computer using the online converter tool.
See [Importing Samples](../deep-dive/importing-samples.md) for details.
