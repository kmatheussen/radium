#pragma once

extern LANGSPEC void create_sendreceive_plugins(void);

struct SoundPlugin;
struct SoundPluginType;

extern SoundPluginType *g_receive_type2;
extern SoundPluginType *g_receive_type8;

static inline bool is_receiver_plugin(struct SoundPlugin *plugin){
  return plugin->type==g_receive_type2 || plugin->type==g_receive_type8;
}

extern LANGSPEC const wchar_t *SEND_RECEIVE_get_name(SoundPlugin *plugin);
extern LANGSPEC bool SEND_RECEIVE_set_name(struct SoundPlugin *plugin, const wchar_t *new_name, bool update_patch_name); // returns true if name was changed

void SEND_RECEIVE_set_compensate_latency(SoundPlugin *plugin, bool doit);
bool SEND_RECEIVE_get_compensate_latency(SoundPlugin *plugin);

void SEND_RECEIVE_update_send_receivers(void); // called after loading.

bool SEND_RECEIVE_handle_new_patchname(SoundPlugin *plugin, const char *s_patch_name); // returns true if send/receive name was changed

const char *SEND_RECEIVE_maybe_generate_patch_name(SoundPlugin *plugin);

void RT_SEND_RECEIVE_swap_receiver_send_channels(struct SoundPlugin *plugin);

