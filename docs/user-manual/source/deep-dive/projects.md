# Projects

A project is a complete musical workspace on the PGB-1. It contains everything
needed to recreate your song or performance.

## What's Inside a Project

| Component | Description |
|-----------|-------------|
| 16 Tracks | Synth engine selection, volume, pan, effects routing, LFO, arpeggiator settings |
| 16 Patterns per track | Step sequences with notes, velocities, conditions, and parameter locks |
| Song arrangement | Chain of song parts that play patterns in sequence |
| Chord progressions | The chords assigned to each song part |
| Tempo | The BPM setting |
| Custom Waveform | A user drawn waveform specific to the project | 

When you power on the PGB-1, you're always working inside a project. Changes
you make (adding notes, adjusting synth parameters, recording samples) happen
in the current project.

The current project is automatically saved when powering off, or entering
update mode.

:::{admonition} Data Loss
:class: warning

The PGB-1 does not auto-save until you power off. If the battery
runs low, the device will shutdown without saving your current project.
Remember to save your project regularly (see below), especially when the
battery is low.
:::

## Managing Projects

Press {kbd}`Menu` and navigate to the `Projects` page (1st), press {kbd}`A` to enter.

| Option | Description |
|--------|-------------|
| Save | Save current project to a slot |
| Load | Load a project from storage |
| New | Create a new empty project |
| Rename | Change the name of a project |
| Delete | Delete a project |

### Saving a Project

1. Select **Save**
2. Choose a project slot
3. Confirm to save

### Loading a Project

1. Select **Load**
2. Use {kbd}`↑` / {kbd}`↓` to browse projects
3. Press {kbd}`A` to select
4. Confirm the action

:::{admonition} Data Loss
:class: warning

Loading a project will erase the current project's patterns, steps, tracks,
and song settings.
:::

### New Project

Creates an empty project with default settings.

:::{admonition} Data Loss
:class: warning

Creating a new project will erase the current project's patterns, steps,
tracks, and song settings.
:::

### Rename Project

Change the name of the project in the project list without any modification to
its content.

### Delete Project

Remove the project from project list and discard all of its content.

:::{admonition} Data Loss
:class: warning

There is no way to recover a project once it is deleted.
:::
