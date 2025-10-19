#pragma once

// #include "TimeData.hpp"


static inline bool FX_when_is_automation(FX_when when){
  return (when != FX_single);
}


struct FX{
	const char *name;
	enum ColorNums color;
	void (*configureFX)(struct FX *fx,struct Tracks *track);
        int min; // can not be equal to INT32_MIN (automation can fail if that is the case)
        int max; // same here

        struct Patch *patch;
  
  	int effect_num; // Set by the instrument plugin. For audio, this is the effect num, while for midi, this is cc (plus some special rules for 14 bit cc and pitch change).

        bool is_enabled;
  
        void (*treatFX)(struct SeqTrack *seqtrack, struct FX *fx,int val,STime time,int skip, FX_when when, double block_reltempo);

        void (*call_me_before_starting_to_play_song_MIDDLE)(struct FX *fx, int val, int64_t abstime, FX_when when);

	void (*closeFX)(struct FX *fx,const struct Tracks *track);
	void *fxdata;	//Free use for the instrument plug-in.
	void (*SaveFX)(struct FX *fx,const struct Tracks *track);
        int (*defaultFXValue)(const struct FX *fx);

  //void (*setFXstring)(struct FX *fx,struct Tracks *track, char *string);
};
#define FX_FAILED 0
#define FX_SUCCESS 1


namespace r{

struct FXNode : NodeId, TimeDataDataType<int> {
  FXNode(const struct FX &fx, Ratio time, int val, int logtype = LOGTYPE_LINEAR)
    : TimeDataDataType<int>(time, R_BOUNDARIES(fx.min, val, fx.max), logtype)
  {}
};

struct FXSeqBlock : RT_TimeData_Player_Cache<decltype(FXNode::_val)> {
};
  
using FXTimeData = TimeData<FXNode, FXSeqBlock>;


struct FXText {
  const struct FX *fx;
#if 0
  Place p;
  struct FXNodeLines *fxnodeline;
#else
  int fxnodenum;
  r::FXNode fxnode;
#endif
  int value;
  int logtype;

  FXText(FXNode fxnode)
    : fxnode(fxnode)
  {}
};

}

#if USE_QT4
typedef QList<r::FXText> FXText_trs;
typedef QMap<int, FXText_trs> FXText_trss;
#endif


struct FXs{
	struct FX *fx;
        r::FXTimeData *_fxnodes;
};

