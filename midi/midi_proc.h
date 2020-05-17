
#ifndef RADIUM_MIDI_MIDI_PROC_H
#define RADIUM_MIDI_MIDI_PROC_H

// implemented in audio/Juce_plugins.cpp (because it uses JUCE to parse midi)
extern LANGSPEC int RT_MIDI_send_msg_to_patch(struct SeqTrack *seqtrack, struct Patch *patch, void *data, int data_size, int64_t seq_time);
extern LANGSPEC int RT_MIDI_send_msg_to_patch_receivers(struct SeqTrack *seqtrack, struct Patch *patch, void *data, int data_size, int64_t seq_time);

static inline uint32_t MIDI_msg_pack3(int a, int b, int c) {
  uint32_t msg = 0;
  msg = a<<16 | b << 8 | c;

  return msg;
}

static inline uint32_t MIDI_msg_pack2(int a, int b) {
  uint32_t msg = 0;
  msg = a<<16 | b << 8;

  return msg;
}

static inline uint32_t MIDI_msg_pack1(int a){
  uint32_t msg = 0;
  msg = a<<16;

  return msg;
}

static inline int MIDI_msg_byte1(uint32_t msg){
  return (msg>>16)&0xff;
}

static inline int MIDI_msg_byte1_remove_channel(uint32_t msg){
  return (msg>>16)&0xf0;
}

static inline int MIDI_msg_byte1_get_channel(uint32_t msg){
  return (msg>>16)&0x0f;
}

static inline int MIDI_msg_byte2(uint32_t msg){
  return (msg>>8)&0xff;
}

static inline int MIDI_msg_byte3(uint32_t msg){
  return msg&0xff;
}

extern LANGSPEC int MIDI_msg_len(uint32_t msg);

#endif // RADIUM_MIDI_MIDI_PROC_H
