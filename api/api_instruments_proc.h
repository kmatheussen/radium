extern LANGSPEC void API_instruments_call_regularly(void);

extern LANGSPEC void API_instrument_call_me_when_instrument_is_deleted(struct Patch *patch);

// In api_gui.cpp (because api_instrument.c is not c++. TODO: fix that)
extern LANGSPEC void API_incSoundPluginRegistryGeneration(void);
