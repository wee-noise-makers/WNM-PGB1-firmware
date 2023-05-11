-----------------------------------------
---- Wee Noise Maker - PS1 Simulator ----
-----------------------------------------

Welcome to the Wee Noise Maker - PS1 Simulator! This application provides an
accurate simulation of a digital hardware sequencer/synthesizer we are currently
designing. The goal of this simulator is to show the capabilities of the device
and get feedback on the user interface and sound synthesis.

The project is still in the development phase, so changes are to be expected in
future versions of the simulator and final device. In particular, new synthesis
engines will be added, but we may also remove some features or effects.

Any feedback (good or bad) is helpful for us. Sharing this project with your
friends is also a good way to help. Please contact us on various platforms to
share your opinion:
* https://github.com/wee-noise-maker/WNM-PS1-firmware
* https://twitter.com/weenoisemaker
* https://mamot.fr/@DesChips
* https://weenoisemaker.com


-- Getting started --

To start the simulator, go to the "bin/" folder and run the "wnm_ps1_simulator"
executable.

To exit the simulator, press [Esc] and use the [Up] and [Down] arrow keys to
navigate to the "exit" option and then press [Enter]. Note that this
menu also allows you to save your project before exiting.

The simulator is controlled with your PC keyboard. A keyboard key is assigned
for each button of the device, the mapping is shown in square brackets [] inside
the black rectangle corresponding to each button. For example, [Q] for the track
button, or [Y] for 5th step.

As of today, only a QWERTY keyboard layout is available. We plan to support more
layouts in the future, but in the meantime temporarily switching to a QWERTY
layout in your operating system settings is the only option.

The device has two encoders to navigate in the menus. On the simulator the
encoders are mapped to arrow keys. [Left] and [Right] to select a different
setting, [Up] and [Down] to change the selected setting. Holding the shift
key will change the setting value faster.

The encoders also have push buttons, mapped [1] and [2] on the keyboard. [1]
corresponds to positive/continue actions such as entering a sub-menu or changing
a value. [2] corresponds to negative/back actions such as leaving a menu.

Some buttons can be hold to open an alternative function:

* Hold Play [\] to open the BPM / Main volume mode and mute tracks
   * Use the [Up] and [Down] keys to change BPM (Beat Per Minute)
   * Use the [Left] and [Right] keys to change the main volume
   * Press one of the number buttons (1 to 16) to mute or unmute the
     corresponding track

* Hold Cpy/FX [=] to open the Copy / Effects mode
   * Press one of the number buttons (1 to 5) to enable or disable the 
     corresponding sequencer effect
   * Press the Track button [Q] to start a copy of the current track
   * Press the Step button [A] to start a copy of the current step
   * Press the Pattern button [-] to start a copy of the current pattern

* Hold Track [Q] to open track select mode
   * Press one of the number buttons (1 to 16) to select the 
     corresponding track

* Hold Step [A] to open step select mode
   * Press one of the number buttons (1 to 16) to select the 
     corresponding step

* Hold Pattern [-] to open pattern select mode
   * Press one of the number buttons (1 to 16) to select the
     corresponding pattern

* Hold Chord [0]: Open chord select mode
   * Press one of the number buttons (1 to 16) to select the corresponding 
     chord


-- Basic workflow --

Enter a pattern:

* Press the Track button [Q] to enter track mode
* Select one of the synthesizer track by pressing one of the number button (1 to 8)
* Press Rec [Enter] to enter edit mode
* Press a few of the number buttons (1 to 16) to enable triggers on the 
  corresponding steps
* Press Rec [Enter] to leave edit mode
* Press Play [\] to play your pattern

Sound design:
* Press the Track button [Q] to enter track mode
* Select one of the synthesizer track by pressing one of the number button (1 to 8)
* Use the [Left] and [Right] keys to navigate between the track settings pages,
  use the [Up] and [Down] keys to change values:
   * Some synthesizers have a "Synth Engine" page (2nd page) setting to select 
     a different sound
   * All synthesizers have a sound design page (2nd or 3rd page) to change 
     the 4 parameters of the sound
   * All synthesizers have an LFO (Low Frequency Oscillator) page 
     (3rd or 4th page) to automatically change one of the sound parameter 
     over time

Step settings:
* Press the Step button [Q] to enter step mode
* Select a the step by pressing one of the number button (1 to 16)
* Use the [Left] and [Right] keys to navigate between the step settings pages, 
  use the [Up] and [Down] keys to change values:
   * The "Condition" setting (1st page) lets you select when a step should be 
     triggered (never, always, randomly, etc.)
   * The "Note" setting (2nd page) lets you select the note to be played. 
     Use [1] to change note mode:
      * Note: plays the single select note
      * Chord: plays the current playing chord (see chord sequencing). Note
        that none of the current synthesizer engines are polyphonic, so this
        mode is not very usable at the moment
      * Note in chord: plays a single note from the current playing chord. 
        Optionally with an octave offset.
      * Arpeggiator: Plays notes from the track arpeggiator. Optionally 
        with an octave offset. The track arpeggiator can be configured in
        the track settings (2 last pages). 
   * The "Duration" setting (2nd page) lets you select the duration of the note to be played
   * The "Velocity" setting (2nd page) lets you select the intensity of the note to be played
   * The "Repeat Count" setting (3rd page) lets you select how many times the note will re-trigger
   * The "Repeat Rate" setting (3rd page) lets you select how fast the note will re-trigger
   * The last page lets you control the synthesizer settings for this step
      * Use the [Up] and [Down] keys to change the value
      * Use the [2] key to disable synth setting for this step

Chord sequencing:

* Press the Chord button [0] to enter chord mode
* Press Rec [Enter] to start recording a chord sequence
* Press any of the number buttons (1 to 16) to to add the corresponding chord 
  to the sequence
* Press Rec [Enter] again to stop recording and play the sequence
