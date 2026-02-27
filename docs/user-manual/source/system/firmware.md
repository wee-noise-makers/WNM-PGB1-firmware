# Firmware Updates

Keep your PGB-1 up to date with the latest features and improvements.

## Checking Your Version

1. Press {kbd}`Menu`
2. Navigate to **System Info**
3. Press {kbd}`A` to view
4. Note your current firmware version

## Getting Updates

Check for new firmware at:

- [weenoisemakers.com/pgb-1](https://weenoisemakers.com/pgb-1)
- Discord server: [discord.gg/EAmAgsmV5V](https://discord.gg/EAmAgsmV5V)
- Social media: @weenoisemakers

## Standard Update Procedure

### Prerequisites

- Good quality USB cable
- Computer that won't restart during update
- Latest firmware .uf2 file downloaded

:::{danger}
Never turn off the PGB-1 or unplug the USB cable during a firmware update.
This may leave the device in an invalid and **unrecoverable** state.
:::

### Step 1: Connect and Enter Update Mode

1. Connect PGB-1 to your computer via USB
2. Turn on the device
3. Press {kbd}`Menu`
4. Navigate to **Update Mode**
5. Press {kbd}`A` to enter
6. Press {kbd}`→` then {kbd}`A` to confirm

The device reboots into update mode. The LEDs display "UP" in green.

:::{versionadded} 1.2.0
Starting with firmware version 1.2.0, it is possible to enter Update mode
by holding the {kbd}`Menu` button while powering on the device. The LEDs
display "UP" in red.
:::

### Step 2: Transfer the Firmware

1. A USB drive named **RPI-RP2** appears on your computer
2. Drag and drop the `.uf2` firmware file into this drive
3. Wait for the transfer to complete
4. The PGB-1 automatically reboots when finished

### Step 3: Verify

1. Press {kbd}`Menu` → **System Info**
2. Confirm the new version number

## User Data

:::{note} Data Preserved
Firmware updates do not erase your projects or samples. Your data remains
intact after updating.
:::

## Reset Procedure

Use when the device is unresponsive or won't turn off.

1. Locate the Reset button (leftmost hole on the bottom edge)
2. Use a paperclip to press the button
3. The device will restart

:::{admonition} Data Loss
:class: warning

Changes to the current project will not be saved when resetting.
:::

## Forced Firmware Update Procedure

:::{admonition} Data Loss
:class: warning

Because of a hardware glitch, this procedure is not available for the first
batch of PGB-1. However, starting with firmware version 1.2.0, it is
possible to enter Update mode by holding the {kbd}`Menu` button while powering
on the device.
:::

Use when the device won't boot normally, standard update procedure fails, or
the device is stuck or froze.

### Prerequisites

1. Make sure the device is **off** (power switch to right, cyan LED off)
2. If the device won't turn off, use the [Reset Procedure](#reset-procedure) first
3. Have USB cable and firmware file ready

### Procedure

1. Connect PGB-1 to computer via USB
2. **Hold the Boot button** (rightmost hole on bottom, use a paperclip)
3. While holding Boot, turn on the device (power switch left)
4. Release the Boot button
5. The **RPI-RP2** drive should appear
6. Drag and drop the firmware file
7. Wait for reboot

## Troubleshooting Updates

### RPI-RP2 Drive Doesn't Appear

- Try a different USB cable
- Try a different USB port
- Ensure you're in Update Mode (LEDs show "UP")
- Try the Forced Update procedure

### Update Seems Stuck

- Wait at least 5 minutes
- Do not unplug the cable
- If nothing happens, use Reset then try Forced Update

### Version Doesn't Change After Update

- Verify you used the correct .uf2 file
- Check the file wasn't corrupted during download
- Try downloading the firmware again
- Try the update procedure again

### Device Won't Start After Failed Update

Use the [Forced Firmware Update Procedure](#forced-firmware-update-procedure)
to recover.

## Best Practices

1. **Don't interrupt**: Let the update complete fully
2. **Use good cables**: Poor cables cause failed transfers
3. **Check power**: Ensure laptop is plugged in or has good battery
4. **Close applications**: Minimize computer activity during update
5. **Read release notes**: Know what's new before updating
