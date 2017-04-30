extern LANGSPEC void API_instruments_call_regularly(void);

extern LANGSPEC void API_instrument_call_me_when_instrument_is_deleted(struct Patch *patch);

// In api_gui.cpp (because api_instrument.c is not c++. TODO: fix that)
extern LANGSPEC void API_incSoundPluginRegistryGeneration(void);
extern LANGSPEC void API_clearSoundPluginRegistryCache(void);

extern LANGSPEC void API_blacklist_container(const SoundPluginTypeContainer *container);
extern LANGSPEC void API_unblacklist_container(const SoundPluginTypeContainer *container);
extern LANGSPEC bool API_container_is_blacklisted(const SoundPluginTypeContainer *container);

#ifdef __cplusplus
enum class DiskOpReturn{
  ALL_MAY_FAIL,
  THIS_ONE_FAILED,
  SUCCEEDED
};
  
//DiskOpReturn API_add_disk_entries_from_populated_container(const SoundPluginTypeContainer *container, const QString path);
#endif
