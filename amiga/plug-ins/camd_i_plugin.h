

struct ChannelSpesific{
	char MSB;
	char LSB;
	char preset;

	bool volumeonoff;
	bool panonoff;

	char volume;
	char pan;

	bool ccsonoff[8];
	char ccvalues[8];

	/* To keep track of how many on-notes that have to be turned off. */
	int num_ons[128];
};

struct MyMidiLinks{
	struct MyMidiLinks *next;
	struct MidiLink *midilink;
	char *name;

	char *ccnames[8];
	char standardccs[8];
	struct ChannelSpesific channelspesific[16];
};

struct PatchData{
	struct MyMidiLinks *mymidilink;

	int channel;
	char LSB;
	char MSB;
	char preset;
};



struct CAMD_FX{
	char *name;
	int min;
	int max;
	int cc;
};

struct UsedTrackMidiCCs{
	struct UsedTrackMidiCCs *next;
	struct CAMD_FX *camd_fx;
};

struct TrackInstrumentData{
	struct UsedTrackMidiCCs *usmf;
};

#define PROGRAMCHANGE_CC 1001
#define CHANNELPREASSURE_CC 1002
#define PITCH7_CC 1003
#define PITCH14_CC 1004
#define OTHER_CC 1006

#define CAMD_NUM_FX 29












