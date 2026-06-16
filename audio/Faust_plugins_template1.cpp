#include <math.h>
#include <string>

#include <vector>

#include <QList>
#include <QMap>
#include <QString>
#include <QWidget>
#include <QDialog>

#include <faust/gui/QTUI.h>

#include "../common/nsmtracker.h"


/*
#include "../Qt/EditorWidget.h"
#include "../Qt/helpers.h"

*/

#include "../api/api_gui_proc.h"

#include "SoundPlugin.h"
#include "SoundPlugin_proc.h"
#include "SoundPluginRegistry_proc.h"
#include "Faust_plugins_proc.h"
#include "Juce_plugins_proc.h"
#include "Mixer_proc.h"



static inline void RT_fade_out(float *sound, int num_frames){
  float num_frames_plus_1 = 1.0 / (num_frames+1.0f);
  for(int i=0;i<num_frames;i++)
    sound[i] *= (num_frames-i) * num_frames_plus_1;
}


#if 0
static inline float linear2db(float val){
  if(val<=0.0f)
    return 0.0f;

  float db = 20*log10(val);
  if(db<-70)
    return 0.0f;
  else if(db>40)
    return 1.0f;
  else
    return scale(db,-70,40,0,1);
}
#endif

#define MIN_LINEAR_VELOCITY 0.1
static constexpr float g_min_linear_gain = 0.001995; // = powf(10, R_SCALE(MIN_LINEAR_VELOCITY, 0.0, 1.0 ,-40, 20) / 20.0f) / 10.0f;

// input is between 0 and 1.
// output is between 0 and 1.
static inline float velocity2gain(float val){
#if 0
	g_min_linear_gain = powf(10, R_SCALE(MIN_LINEAR_VELOCITY, 0.0, 1.0 ,-40, 20) / 20.0f) / 10.0f;
	printf("%f\n",g_min_linear_gain);
	getchar();
#endif
	
  if(val<=0.0f)
    return 0.0f;
  else if(val>=1.0f)
    return 1.0f;
  else if (val < MIN_LINEAR_VELOCITY)
	  return scale(val,
		       0, MIN_LINEAR_VELOCITY,
		       0, g_min_linear_gain);
  else
    return powf(10, scale(val,0.0, 1.0 ,-40, 20) / 20.0f) / 10.0f;
}


#if 0
// For some reason, it won't compile with the usual min/max macros.
template<typename T> static inline T min(T a,T b){return a<b ? a : b;}
template<typename T> static inline T max(T a,T b){return a>b ? a : b;}
static inline float min(float a,int b){return a<b ? a : b;}
static inline float max(float a,int b){return a>b ? a : b;}
static inline float min(int a,float b){return a<b ? a : b;}
static inline float max(int a,float b){return a>b ? a : b;}
#endif

static inline int 	max (unsigned int a, unsigned int b) { return (a>b) ? a : b; }
static inline int 	max (int a, int b)		{ return (a>b) ? a : b; }

static inline long 	max (long a, long b) 		{ return (a>b) ? a : b; }
static inline long 	max (int a, long b) 		{ return (a>b) ? a : b; }
static inline long 	max (long a, int b) 		{ return (a>b) ? a : b; }

static inline float 	max (float a, float b) 		{ return (a>b) ? a : b; }
static inline float 	max (int a, float b) 		{ return (a>b) ? a : b; }
static inline float 	max (float a, int b) 		{ return (a>b) ? a : b; }
static inline float 	max (long a, float b) 		{ return (a>b) ? a : b; }
static inline float 	max (float a, long b) 		{ return (a>b) ? a : b; }

static inline double 	max (double a, double b) 	{ return (a>b) ? a : b; }
static inline double 	max (int a, double b) 		{ return (a>b) ? a : b; }
static inline double 	max (double a, int b) 		{ return (a>b) ? a : b; }
static inline double 	max (long a, double b) 		{ return (a>b) ? a : b; }
static inline double 	max (double a, long b) 		{ return (a>b) ? a : b; }
static inline double 	max (float a, double b) 	{ return (a>b) ? a : b; }
static inline double 	max (double a, float b) 	{ return (a>b) ? a : b; }


static inline int	min (int a, int b)		{ return (a<b) ? a : b; }

static inline long 	min (long a, long b) 		{ return (a<b) ? a : b; }
static inline long 	min (int a, long b) 		{ return (a<b) ? a : b; }
static inline long 	min (long a, int b) 		{ return (a<b) ? a : b; }

static inline float 	min (float a, float b) 		{ return (a<b) ? a : b; }
static inline float 	min (int a, float b) 		{ return (a<b) ? a : b; }
static inline float 	min (float a, int b) 		{ return (a<b) ? a : b; }
static inline float 	min (long a, float b) 		{ return (a<b) ? a : b; }
static inline float 	min (float a, long b) 		{ return (a<b) ? a : b; }

static inline double 	min (double a, double b) 	{ return (a<b) ? a : b; }
static inline double 	min (int a, double b) 		{ return (a<b) ? a : b; }
static inline double 	min (double a, int b) 		{ return (a<b) ? a : b; }
static inline double 	min (long a, double b) 		{ return (a<b) ? a : b; }
static inline double 	min (double a, long b) 		{ return (a<b) ? a : b; }
static inline double 	min (float a, double b) 	{ return (a<b) ? a : b; }
static inline double 	min (double a, float b) 	{ return (a<b) ? a : b; }

namespace{

class MyUI : public UI
{

 public:

  MyUI()
    : next_peak(NULL)
    , _gate_control(NULL)
    , _freq_control(NULL)
    , _gain_control(NULL)
    , _num_effects(0)
    , _effect_tooltip("")
    , _curr_box_name(NULL)
  { }

  ~MyUI() {	}

  float *next_peak;

  float *_gate_control;
  float *_freq_control;
  float *_gain_control;

  struct Controller{
    struct SoundPlugin *plugin;
    int effect_num;
    
    float* control_port;

    float *peak_port;

    float min_value;
    float default_value;
    float max_value;

    std::string name;
    int type;

    const char *tooltip;
    const char *unit;

    Controller(float *control_port)
      : plugin(NULL)
      , effect_num(-1)
      , control_port(control_port)
      , peak_port(NULL)
      , min_value(0.0f)
      , default_value(0.5f)
      , max_value(1.0f)
      , name("<no name>")
      , type(EFFECT_FORMAT_FLOAT)
      , tooltip("")
      , unit("")
    { }
  };

  std::vector<Controller> _controllers;

  int get_controller_num(float *control_port){
    for(unsigned int i=0;i<_controllers.size();i++){
      if(control_port == _controllers.at(i).control_port)
        return i;
    }

    Controller controller(control_port);

    _controllers.push_back(controller);
    _num_effects++;

    return (int)_controllers.size()-1;
  }

  bool is_instrument(){
    if(_gate_control!=NULL && _freq_control!=NULL && _gain_control!=NULL)
      return true;
    else
      return false;
  }

  // Remove gain/gate/freq sliders for instruments.
  void remove_instrument_notecontrol_effects(){
    if(is_instrument()){
      _controllers.erase(_controllers.begin() + get_controller_num(_gate_control));
      _controllers.erase(_controllers.begin() + get_controller_num(_freq_control));
      _controllers.erase(_controllers.begin() + get_controller_num(_gain_control));
      _num_effects -= 3;
    }
  }

  // We don't use passive items. (it would have been nice to have a passive effect telling when an instrument is finished playing)
  void remove_last_item(){
    _controllers.pop_back();
    _num_effects--;
  }
  

  int _num_effects;

  const char *_effect_tooltip;

  const char* _curr_box_name;

  /**
     volume, volume, delay, volume
     ->
     volume (1), volume (2), delay, volume (3)
   */
  void uniqifyEffectNames(void){
    QList<QString> names;
    QMap<QString, int> counters;
    
    for(int i=0;i<_num_effects;i++)
      names.push_back(_controllers[i].name.c_str());

    for(int i=0;i<_num_effects;i++) {
      QString name = names[i];
      if (counters.contains(name)){
        int counter = counters[name] + 1;
        names[i] = name + " (" + QString::number(counter) + ")";
        counters[name] = counter;
      } else if (i<_num_effects-1 && names.indexOf(name,i+1)>0){ // I.e. are there more names with the same name later in the list
        names[i] = name + " (1)";
        counters[name] = 1;
      }
    }

    for(int i=0;i<_num_effects;i++)
      if (names[i] != _controllers[i].name.c_str())
        _controllers[i].name = names[i].toUtf8().constData(); // <-- Note that '_controllers[i].name' is a std::string, not a char*.
  }
  
  // -- widget's layouts
  
  //void openFrameBox(const char* label) override {_curr_box_name = label;}
  void openTabBox(const char* label) override {_curr_box_name = label;}
  void openHorizontalBox(const char* label) override {_curr_box_name = label;}
  void openVerticalBox(const char* label) override {_curr_box_name = label;}
  void closeBox() override {_curr_box_name = NULL;}
  
  // -- active widgets

private:

  void addEffect(const char *name, float* control_port, int type, float min_value, float default_value, float max_value) {
    int effect_num = get_controller_num(control_port);

    Controller *controller = &_controllers.at(effect_num);

    if(_curr_box_name != NULL && strlen(_curr_box_name) < 10 && strcmp(_curr_box_name, "0x00")){
      controller->name = std::string(_curr_box_name) + ": " + name;
    }else{
      controller->name = name;
    }

    //printf("  %p: addEffect. Controller name: \"%s\". Value: %f\n", this, controller->name.c_str(), *control_port);

    controller->type = type;
    controller->min_value = min_value;
    controller->default_value = default_value;
    controller->max_value = max_value;

    if(next_peak != NULL){
      controller->peak_port = next_peak;
      next_peak = NULL;
    }

    if(!strcmp(name,"gate")){
      //R_ASSERT(_gate_control==NULL || _gate_control==control_port);
      _gate_control = control_port;
    }
    
    if(!strcmp(name,"freq")){
      //R_ASSERT(_freq_control==NULL || _freq_control==control_port);
      _freq_control = control_port;
    }
    
    if(!strcmp(name,"gain")){
      //R_ASSERT(_gain_control==NULL || _gain_control==control_port);
      _gain_control = control_port;
    }
  }

protected:

  void addButton(const char* label, float* zone) override {
    //printf("Add button %s - %p\n", label, zone);
    addEffect(label, zone, EFFECT_FORMAT_BOOL, 0, 0, 1);
  }
  void addToggleButton(const char* label, float* zone) {
    //printf("Add toggle button %s - %p\n", label, zone);
    addEffect(label, zone, EFFECT_FORMAT_BOOL, 0, 0, 1);
  }
  void addCheckButton(const char* label, float* zone) override {
    //printf("Add check button %s - %p\n", label, zone);
    addEffect(label, zone, EFFECT_FORMAT_BOOL, 0, 0, 1);
  }
  void addVerticalSlider(const char* label, float* zone, float init, float min, float max, float step) override {
    //printf("Add vertical slider %s - %p. %f %f %f %f\n", label, zone, init, min, max, step);
    addEffect(label, zone,  equal_floats(step, 1.0f) ? EFFECT_FORMAT_INT : EFFECT_FORMAT_FLOAT, min, init, max);
  }
  void addHorizontalSlider(const char* label, float* zone, float init, float min, float max, float step) override {
    //printf("Add horizontal slider %s - %p. %f %f %f %f\n", label, zone, init, min, max, step);
    addEffect(label, zone,  equal_floats(step, 1.0f) ? EFFECT_FORMAT_INT : EFFECT_FORMAT_FLOAT, min, init, max);
  }
  void addNumEntry(const char* label, float* zone, float init, float min, float max, float step) override {
    //printf("Add num entry %s - %p. %f %f %f %f\n", label, zone, init, min, max, step);
    addEffect(label, zone, equal_floats(step, 1.0f) ? EFFECT_FORMAT_INT : EFFECT_FORMAT_FLOAT, min, init, max); // The INT effect format might not work. Need to go through the code first.
  }
  
  // -- passive widgets

  void addNumDisplay(const char* label, float* zone, int precision) {remove_last_item();}
  void addTextDisplay(const char* label, float* zone, const char* names[], float min, float max) {remove_last_item();}
  void addHorizontalBargraph(const char* label, float* zone, float min, float max) override {
    remove_last_item(); // remove metadata
    next_peak = zone;
  }
  void addVerticalBargraph(const char* label, float* zone, float min, float max) override {
    remove_last_item(); // remove metadata
    next_peak = zone;
  }
  
  // -- soundfiles
  
#if 1 //HEPP
  void addSoundfile(const char* label, const char* filename, Soundfile** sf_zone) override {}
#endif

  // -- metadata declarations
  
  void declare(float* control_port, const char* key, const char* value) override {
    if(control_port==NULL){
      if(!strcmp(key,"tooltip"))
        _effect_tooltip = value;
    } else {
      int effect_num = get_controller_num(control_port);
      Controller *controller = &_controllers.at(effect_num);
      if(!strcmp(key,"tooltip"))
        controller->tooltip = value;
      else if(!strcmp(key,"unit"))
        controller->unit = value;
    }
  }
};


#define MAX_POLYPHONY 32

struct Voice{
  struct Voice *prev;
  struct Voice *next;
  dsp *dsp_instance;
  MyUI myUI;
  float note_num;
  int64_t note_id;
  const struct SeqBlock *seqblock;
  
  int frames_since_stop;

  int delta_pos_at_start; // Within the current block. Set when starting a note.
  int delta_pos_at_end; // Within the current block. Set when stopping a note.

  Voice()
    : prev(NULL)
    , next(NULL)
    , dsp_instance(NULL)
    , note_num(0)
    , note_id(-1)
    , delta_pos_at_start(0)
    , delta_pos_at_end(-1)
  { }
};


struct Data{
  Voice *voices_playing; // not used by effects
  Voice *voices_not_playing; // not used by effects
  Voice voices[MAX_POLYPHONY];   // Only voices[0] is used by effects.
  float samplerate;

  QTGUI *qtgui;
  QDialog *qtgui_parent;

  float *automation_values;
  
  Data()
    : voices_playing(NULL)
    , voices_not_playing(NULL)
    , qtgui(NULL)
    , qtgui_parent(NULL)
    , automation_values(NULL)
  {
  }

  ~Data(){
    delete qtgui_parent;
    V_free(automation_values);
  }
};

} // end anonymous namespace

