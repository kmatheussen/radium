#pragma once

#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif

// Called from the MIDI input thread. Only fader (pitch bend) messages are
// currently queued. byte1/byte2/byte3 are the raw MIDI bytes.
extern void MCU_InputMessageHasBeenReceived(const char *port_name, int byte1, int byte2, int byte3);

// Main thread.
extern void MCU_add_port(const char *port_name);
extern void MCU_remove_port(const char *port_name);
extern void MCU_call_very_often(void);

// Called when keyboard focus changes.
extern void MCU_set_sequencer_has_focus(bool has_focus);

#ifdef __cplusplus
}
#endif
