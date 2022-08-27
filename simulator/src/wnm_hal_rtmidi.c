#include <stdint.h>
#include <stdio.h>
#include <rtmidi/rtmidi_c.h>

RtMidiOutPtr midi_out;

void
wnm_hal_rtmidi_init(void)
{
  midi_out = rtmidi_out_create(RTMIDI_API_UNSPECIFIED, "WNM Simulator");
  rtmidi_open_port(midi_out, 0, "Output");
  return;
}

void
wnm_hal_rtmidi_send(uint8_t data[], uint32_t len)
{
  int ret = rtmidi_out_send_message(midi_out, data, len);
  return;
}
