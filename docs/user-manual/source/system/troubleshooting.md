# Troubleshooting

Solutions for common issues with the PGB-1.

## Device Issues

### Device Won't Turn On

**Symptoms:** No response when power switch is moved to On position.

**Solutions:**

1. **Charge the battery**
    - Connect USB cable
    - Look for red charging LED
    - Wait for some charge before trying again

2. **Try reset**
    - Use a paperclip to press the Reset button
    - Located in the leftmost hole on the bottom edge

3. **Check power switch**
    - Ensure switch is fully to the left (On position)

### Device Won't Turn Off

**Symptoms:** Device stays on when power switch is in Off position.

**Cause:** Unrecoverable software malfunction.

**Solution:** Use the Reset Procedure.

### Device is Frozen/Unresponsive

**Symptoms:** Buttons don't respond, display frozen.

**Solution:**

1. Use the Reset Procedure (paperclip in reset hole)
2. Note: Unsaved changes will be lost

## Audio Issues

### No Sound from Speaker

**Solutions:**

1. Check volume - Hold {kbd}`Play` + {kbd}`↑`
2. Ensure no headphones are connected (speaker mutes when headphones plugged in)
3. Check that tracks are not muted
4. Verify patterns have steps enabled

### No Sound from Headphones

**Solutions:**

1. Check headphone connection
2. Check volume level
3. Try different headphones
4. Clean the headphone jack

### Audio is Distorted

**Solutions:**

1. Lower the master volume
2. Lower individual track volumes
3. Reduce drive/effect levels
4. Check if sample levels are too hot

### Audio Inputs Not Working

**Solutions:**

1. Ensure input is enabled in Menu → Inputs
2. Check input gain level
3. Verify cable connection
4. Try different input source

## Sequencer Issues

### Pattern Won't Play

**Solutions:**

1. Check the pattern is selected in Song Mode for the current part
2. Verify the track isn't muted (hold {kbd}`Play` + track button)
3. Ensure steps are enabled in the pattern
4. Check step conditions aren't set to "Never"

### Wrong Pattern Playing

**Cause:** Song Mode controls which patterns play.

**Solution:**

1. Enter Song Mode ({kbd}`Song`)
2. Check which patterns are assigned to the current song part
3. Modify as needed

### Steps Not Triggering

**Solutions:**

1. Check step condition (probability, fill, etc.)
2. Verify step is enabled
3. Check track isn't muted
4. Verify song part has the pattern assigned

## Firmware Issues

### Update Failed

See [Firmware Updates - Troubleshooting](firmware.md#troubleshooting-updates).

### Device Won't Enter Update Mode

**Solutions:**

1. Use the Forced Firmware Update Procedure
2. Hold Boot button while turning on
3. Try different USB cable

## USB Issues

### Computer Doesn't Recognize Device

**Solutions:**

1. Try different USB cable
2. Try different USB port
3. Restart the PGB-1
4. Check computer's USB drivers

### RPI-RP2 Drive Not Appearing

**Solutions:**

1. Ensure device is in Update Mode
2. Try Forced Update procedure
3. Try different cable/port

## Reset Procedure

Use when the device is unresponsive.

1. Locate Reset button (leftmost hole, bottom edge)
2. Insert paperclip and press
3. Device restarts

:::{admonition} Data Loss
:class: warning

Unsaved project changes will be lost.
:::

## Forced Firmware Update Procedure

Use when normal update fails.

1. Turn off device completely
2. Connect USB cable to computer
3. Hold Boot button (rightmost hole) with paperclip
4. While holding, turn on device
5. Release Boot button
6. RPI-RP2 drive should appear
7. Transfer firmware file

## Battery Issues

### Battery Indicator Not Accurate While Charging

**This is normal.** During charging, the on-screen battery indicator doesn't show accurate charge state. Only the red LED indicates charging status.

### Charge Doesn't Complete

**Solutions:**

1. Use a different USB cable
2. Try a different power source
3. Ensure power source provides adequate current

## Getting Help

If problems persist:

1. Join the Discord: [discord.gg/EAmAgsmV5V](https://discord.gg/EAmAgsmV5V)
2. Visit the website: [weenoisemakers.com/pgb-1](https://weenoisemakers.com/pgb-1)
3. Contact support via the website contact form
