# Navigation Basics

## Button Controls

The PGB-1 uses a consistent navigation scheme across all menus and modes:

| Button | Function |
|--------|----------|
| {kbd}`←` / {kbd}`→` | Navigate between settings pages |
| {kbd}`↑` / {kbd}`↓` | Change the value of the selected setting |
| {kbd}`A` | Positive actions (select, accept, enable, confirm) |
| {kbd}`B` | Negative actions (go back, reject, disable, cancel) |

## Screen Layout

### Page Indicator


```{figure} ../assets/images/navigation-screenshot.png
:alt: Navigation screenshot
:width: 500px
```



At the top of the screen, a **page indicator** shows:

- How many pages of settings are available (dots)
- Which page is currently displayed (larger dot)
- Arrows indicating more pages to the left or right

### Track and Pattern Display

In most modes, the screen displays:

- **Top left**: Current track name
- **Top right**: Current pattern and step numbers


```{figure} ../assets/images/track-name-screenshot.png
:alt: Track name screenshot
:width: 300px
```



```{figure} ../assets/images/current-pattern-and-step-screenshot.png
:alt: Current pattern and step screenshot
:width: 300px
```




## Menu Navigation

### Opening the Main Menu

Press {kbd}`Menu` to open the main menu. Use {kbd}`←` and {kbd}`→` to browse menu items, and press {kbd}`A` to enter a sub-menu.

### Menu Structure

The main menu contains:

- **Projects** - Save, load, and create projects
- **Custom Waveform** - Draw and edit waveforms
- **Inputs** - Configure audio inputs
- **MIDI Settings** - Configure MIDI interface
- **Update Mode** - Enter firmware update mode
- **System Info** - View firmware version

## Mode Selection

### Entering Main Modes

Press and release a mode button to enter that mode:

- {kbd}`Track` - Track Mode
- {kbd}`Step` - Step Mode
- {kbd}`Pattern` - Pattern Mode
- {kbd}`Song` - Song Mode

The corresponding LED will light up to indicate the active mode.

### Quick Selection Without Mode Change

You can select items without changing the current mode by **holding** the mode button:

| Action | Result |
|--------|--------|
| Hold {kbd}`Track` + Press {kbd}`1`-{kbd}`16` | Select track without entering Track Mode |
| Hold {kbd}`Step` + Press {kbd}`1`-{kbd}`16` | Select step without entering Step Mode |
| Hold {kbd}`Pattern` + Press {kbd}`1`-{kbd}`16` | Select pattern without entering Pattern Mode |
| Hold {kbd}`Song` + Press {kbd}`1`-{kbd}`12` | Select song part without entering Song Mode |

## Edit Mode

In Track or Step mode, press {kbd}`Edit` to toggle pattern edit mode. When edit
mode is active:

- The keyboard buttons ({kbd}`1` to {kbd}`16`) toggle steps on/off in the current pattern
- Press {kbd}`Edit` again to exit edit mode

### Chromatic Keyboard

Hold {kbd}`Edit` to access the chromatic keyboard:

- Play notes on the selected track
- Press {kbd}`1` to go down an octave
- Press {kbd}`4` to return to center octave
- Press {kbd}`8` to go up an octave

## Using the Touch Strip

The touch strip provides analog control for parameters:

1. Navigate to a parameter using {kbd}`←` / {kbd}`→`
2. Touch and slide on the touch strip to adjust the value
3. The value changes smoothly as you slide

:::{tip}
Hold {kbd}`Cpy/FX` to access a quick parameter that can be controlled with the touch strip during performance.
:::
