# Pattern Mode

Pattern Mode allows you to configure the length of patterns and link multiple
patterns together for longer sequences.

- Each track has **16 patterns**
- Each pattern has up to **16 steps**
- Patterns can be linked together to create sequences up to **256 steps** (16 patterns x 16 steps)

It's important to keep in mind the selected pattern as it is the one edited in
Step mode and. The selected pattern doesn't change when selecting another
track. For example, if you enter steps in pattern 4 with the kick track
selected and then switch to the bass track, you will now edit pattern 4 of the
bass track.

The numbers of the selected pattern is shown at the top right of the screen:
![Current pattern and step on
screen](../assets/images/current-pattern-and-step-screenshot.png){ width="400" }


## Entering Pattern Mode

Press {kbd}`Pattern` to enter Pattern Mode. The Pattern LED will light up.

## Selecting Patterns

:::{important}
Selecting a pattern does not play it. Which pattern plays for each track is
determined in [Song Mode](song-mode.md). The LED corresponding to currently
playing pattern is blinking in green.
:::

In Pattern Mode:

- Press {kbd}`1` to {kbd}`16` to select a pattern
- The selected pattern number appears at the top right of the screen

To select a pattern without changing mode:

- Hold {kbd}`Pattern` and press {kbd}`1` to {kbd}`16`

## Pattern Settings

### Length

Set how many steps the pattern contains (1-16 steps).

Navigate to the length setting and use {kbd}`↑` / {kbd}`↓` to adjust.

### Link

Link this pattern to the next pattern. When enabled, the next pattern will
automatically play after this one completes.

Use {kbd}`↑`/{kbd}`↓` to enable/ disable linking.

## Pattern Linking Visualization

### On Screen

 - The two lines of small boxes at the top right of the page represent the 16
   patterns.
 - The bigger box represents the selected pattern
 - A line between two boxes is visible when they are linked together
 - The box corresponding to the currently playing pattern is blinking


```{figure} ../assets/images/pattern-link-screenshot.png
:alt: Pattern link on screen
:width: 400px
```



### LED Indicators

- **Blue LED**: Currently selected pattern
- **Cyan LEDs**: Patterns linked to the selected pattern
- **Blinking Green LED**: Currently playing pattern

## Creating Long Patterns

To create patterns longer than 16 steps:

1. Enter Pattern Mode
2. Select the first pattern (e.g., Pattern 1)
3. Set length to 16
4. Enable Link to connect to Pattern 2
5. Select Pattern 2
6. Set its length
7. Continue linking patterns as needed

Link all 16 patterns together for a maximum of 256 steps (16 x 16).

### Example: 24-Step Pattern

1. Pattern 1: Length 16, Link enabled
2. Pattern 2: Length 8, Link disabled

This creates a 24-step sequence that plays Pattern 1 followed by Pattern 2.

