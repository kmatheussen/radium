#include <QList>
#include <QMap>
#include <QString>
#include <QWidget>
#include <QDialog>

#include "../Qt/EditorWidget.h"
#include "../Qt/helpers.h"

#include "SoundPlugin.h"
#include "SoundPlugin_proc.h"

#include "Mixer_proc.h"
#include "SoundPluginRegistry_proc.h"

#include "../api/api_gui_proc.h"


#include "Faust_plugins_proc.h"

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

#if defined(CLASSNAME)
static Data *GET_DATA_FROM_PLUGIN(SoundPlugin *plugin){
  return (Data*)plugin->data;
}
#endif


static void RT_add_voice(Voice **root, Voice *voice){
  voice->next = *root;
  if(*root!=NULL)
    (*root)->prev = voice;
  *root = voice;
  voice->prev = NULL;
}

static void RT_remove_voice(Voice **root, Voice *voice){
  if(voice->prev!=NULL)
    voice->prev->next = voice->next;
  else
    *root=voice->next;

  if(voice->next!=NULL)
    voice->next->prev = voice->prev;
}

static bool RT_is_silent(float *sound, int num_frames, const bool use_old_buggy_behavior){
	if (use_old_buggy_behavior)
	{
		for(int i=0;i<num_frames;i++)
			if(sound[i]>0.05f)
				return false;
	}
	else
	{
		for(int i=0;i<num_frames;i++)
			if(sound[i]>0.0005f || sound[i]<-0.0005f)
				return false;
	}
	
	return true;
}

enum VoiceOp{
  VOICE_KEEP,
  VOICE_REMOVE
};

static void RT_process_between(Voice *voice, float **inputs, float **outputs, int start, int end){
  if(end==start)
    return;

  int num_inputs = voice->dsp_instance->getNumInputs();
  float *offsetted_inputs[R_MAX(1, num_inputs)];
  for(int ch=0;ch<num_inputs; ch++)
    offsetted_inputs[ch] = &inputs[ch][start];

  int num_outputs = voice->dsp_instance->getNumOutputs();
  float *offsetted_outputs[R_MAX(1, num_outputs)];
  for(int ch=0;ch<num_outputs; ch++)
    offsetted_outputs[ch] = &outputs[ch][start];

  //printf("Computing Delta start / end: %d / %d\n",start,end);

  voice->dsp_instance->compute(end-start, offsetted_inputs, offsetted_outputs);
}

static VoiceOp RT_play_voice(Data *data, Voice *voice, int num_frames, float **inputs, float **outputs, int *start_process){

  int delta_pos_at_start = voice->delta_pos_at_start;
  int delta_pos_at_end = voice->delta_pos_at_end;

  if(delta_pos_at_start==0 && delta_pos_at_end==-1){

    voice->dsp_instance->compute(num_frames, inputs, outputs);

    *start_process = 0;

    if(equal_floats(*(voice->myUI._gate_control), 0.0f)){
	    
	    const bool use_old_buggy_behavior = root->song->RT_use_old_buggy_faust_note_release_behavior;

	    const int num_seconds_safety = use_old_buggy_behavior ? 1 : 5;
		    
	    if(false
	       || RT_is_silent(outputs[0], num_frames, use_old_buggy_behavior)
	       || voice->frames_since_stop > data->samplerate*num_seconds_safety) // Safety mechanism. Force voice to stop after about 5 second.
	    {
		    //printf("----removing voice: %d %d\n", RT_is_silent(outputs[0], num_frames), voice->frames_since_stop > data->samplerate);
		    
		    return VOICE_REMOVE;
	    }
	    
	    voice->frames_since_stop += num_frames;
    }

  }else if(delta_pos_at_start>0 && delta_pos_at_end==-1){

    RT_process_between(voice, inputs, outputs, delta_pos_at_start, num_frames);

    *start_process = delta_pos_at_start;
    voice->delta_pos_at_start = 0;

  }else{
    //printf("Delta start / end: %d / %d\n",delta_pos_at_start,delta_pos_at_end);

    RT_process_between(voice, inputs, outputs, delta_pos_at_start, delta_pos_at_end);
    {
	    //printf("---Setting gate to 0\n");
	    *(voice->myUI._gate_control)=0.0f;
    }
    RT_process_between(voice, inputs, outputs, delta_pos_at_end, num_frames);

    voice->frames_since_stop = num_frames-delta_pos_at_end;

    *start_process = delta_pos_at_start;
    voice->delta_pos_at_start = 0;
    voice->delta_pos_at_end = -1;

  }

  return VOICE_KEEP;
}

static void RT_process_instrument2(int num_outputs, Data *data, int64_t time, int num_frames, float **inputs, float **outputs){
  for(int i=0;i<num_outputs;i++)
    memset(outputs[i],0,num_frames*sizeof(float));

  float *tempsounds[num_outputs];
  float tempdata[num_outputs][num_frames];
  for(int i=0;i<num_outputs;i++)
    tempsounds[i] = &tempdata[i][0];

  Voice *voice = data->voices_playing;
  //printf("Voices? %s\n",voice==NULL?"No":"Yes");

  while(voice!=NULL){
    Voice *next = voice->next;
    int start_process;

    if(RT_play_voice(data,voice,num_frames,inputs,tempsounds,&start_process)==VOICE_REMOVE)
    {
	    
	    RT_remove_voice(&data->voices_playing, voice);
	    RT_add_voice(&data->voices_not_playing, voice);
	    
	    R_ASSERT_NON_RELEASE(start_process == 0);
	    
	    for(int ch=0;ch<num_outputs;ch++)
		    RT_fade_out(tempsounds[ch], num_frames-start_process);
    }

    for(int ch=0;ch<num_outputs;ch++){
      float *source = tempsounds[ch];
      float *target = outputs[ch];
      for(int i=start_process;i<num_frames;i++)
        target[i] += source[i];
    }

    voice=next;
  }
}

static void RT_process_instrument(SoundPlugin *plugin, int64_t time, int num_frames, float **inputs, float **outputs){
  Data *data = (Data*)plugin->data;
  int num_outputs = plugin->type->num_outputs;
  RT_process_instrument2(num_outputs, data, time, num_frames, inputs, outputs);
}

static void play_note2(Data *data, int time, note_t note){
  //printf("Playing %f\n",note.pitch);

  Voice *voice = data->voices_not_playing;

  if(voice==NULL)
  {
	  RT_message("Instrument \"%s\" has no more free voices. (max polyphony is %d)", DSP_NAME, MAX_POLYPHONY);
	  return;
  }

  RT_remove_voice(&data->voices_not_playing, voice);
  RT_add_voice(&data->voices_playing, voice);

  *(voice->myUI._gate_control) = 1.0f;
  *(voice->myUI._freq_control) = midi_to_hz(note.pitch);
  *(voice->myUI._gain_control) = velocity2gain(note.velocity);

  //printf("        Setting freq to %f. gate: %f. gain: %f\n", *(voice->myUI._freq_control), *(voice->myUI._gate_control), *(voice->myUI._gain_control));

  voice->note_num = note.pitch;
  voice->note_id = note.id;
  voice->seqblock = note.seqblock;
  
  voice->frames_since_stop = 0;
  voice->delta_pos_at_start = time;
  voice->delta_pos_at_end = -1;
}

static void play_note(struct SoundPlugin *plugin, int time, note_t note){
  Data *data = (Data*)plugin->data;
  play_note2(data, time, note);
}

static void set_note_volume2(Data *data, int time, note_t note){
  Voice *voice = data->voices_playing;
  //printf("Setting volume %f / %f\n",note.velocity,velocity2gain(note.velocity));
  while(voice!=NULL){
    if(is_note(note, voice->note_id, voice->seqblock))
      *(voice->myUI._gain_control) = velocity2gain(note.velocity);
    voice=voice->next;
  }
}

static void set_note_volume(struct SoundPlugin *plugin, int time, note_t note){
  Data *data = (Data*)plugin->data;
  set_note_volume2(data, time, note);
}

static void set_note_pitch2(Data *data, int time, note_t note){
  Voice *voice = data->voices_playing;
  //printf("Setting volume %f / %f\n",volume,velocity2gain(volume));
  while(voice!=NULL){
    if(is_note(note, voice->note_id, voice->seqblock))
      *(voice->myUI._freq_control) = midi_to_hz(note.pitch);
    voice=voice->next;
  }
}
static void set_note_pitch(struct SoundPlugin *plugin, int time, note_t note){
  Data *data = (Data*)plugin->data;
  set_note_pitch2(data, time, note);
}

static void stop_note2(Data *data, int time, note_t note){
  Voice *voice = data->voices_playing;
  while(voice!=NULL){
    if(is_note(note, voice->note_id, voice->seqblock)){
      voice->delta_pos_at_end = time;
      //printf("        STOP! freq to %f. gate: %f. gain: %f\n", *(voice->myUI._freq_control), *(voice->myUI._gate_control), *(voice->myUI._gain_control));
    }
    voice=voice->next;
  }
}
static void stop_note(struct SoundPlugin *plugin, int time, note_t note){
  Data *data = (Data*)plugin->data;
  stop_note2(data, time, note);
}

static void RT_process_effect2(Data *data, int64_t time, int num_frames, float **inputs, float **outputs){
  data->voices[0].dsp_instance->compute(num_frames, inputs, outputs);
  //printf("in00: %f, in10: %f\n",inputs[0][0],inputs[1][0]);
  //printf("out00: %f, out10: %f\n",outputs[0][0],outputs[1][0]);
}

static void RT_process_effect(SoundPlugin *plugin, int64_t time, int num_frames, float **inputs, float **outputs){
  Data *data = (Data*)plugin->data;
  RT_process_effect2(data, time, num_frames, inputs, outputs);
}

#if 1
static void faust_gui_zone_callback(float val, void* arg){
  MyUI::Controller *controller = (MyUI::Controller*)arg;

  float min = controller->min_value;
  float max = controller->max_value;

  SoundPlugin *plugin = controller->plugin;
  int effect_num = controller->effect_num;
  
  Data *data = GET_DATA_FROM_PLUGIN(plugin);
  if (fabs(val - data->automation_values[effect_num]) < fabs((max-min)/100.0)) // approx.
    return;

  float stored_value;
  stored_value = PLUGIN_get_effect_value(plugin, effect_num, VALUE_FROM_STORAGE);

  //printf("\n  Callback called %f. controller: %p\n      val/auto/stored: %f %f %f\n\n", val, controller, val, data->automation_values[effect_num], stored_value);
  
  if (equal_floats(val, stored_value))
    return;

  // We are now pretty certain that this update was caused by a user interaction in the faust gui, and not a roundtrip from radium.
  
  PLUGIN_set_effect_value(plugin, -1, effect_num, val, STORE_VALUE, FX_single, EFFECT_FORMAT_NATIVE);
  
  volatile struct Patch *patch = plugin->patch;
  ATOMIC_SET(patch->widget_needs_to_be_updated, true);
}
#endif

static float get_effect_value2(Data *data, int effect_num, enum ValueFormat value_format);

static void create_gui(QDialog *parent, Data *data, SoundPlugin *plugin){

  Voice &voice = data->voices[0];
  
  dsp *dsp = voice.dsp_instance;

  data->qtgui = new QTGUI(parent);
  printf("     Created new QtGui %p\n", data->qtgui);

  /*
  QStyle *style = QStyleFactory::create("plastique");
  if (style!=NULL)
    data->qtgui->setStyle(style);
  */
  
  //FAUST_set_qtguistyle(parent);
  
  dsp->buildUserInterface(data->qtgui);

#if 1
  int num_effects = voice.myUI._num_effects;
  for(int i=0 ; i < num_effects ; i++){
    MyUI::Controller *controller = &voice.myUI._controllers.at(i);
    controller->effect_num = i;
    controller->plugin = plugin;
    data->qtgui->addCallback(controller->control_port, faust_gui_zone_callback, controller);
  }
#endif
  
  if (parent->layout()==NULL){
    QLayout *layout = new QGridLayout(parent);
    parent->setLayout(layout);
  }
  
  parent->layout()->addWidget(data->qtgui);
}

static void create_automation_values(Data *data){
  int num_effects = data->voices[0].myUI._num_effects;
  data->automation_values = (float*)V_malloc(sizeof(float) * num_effects);
  for(int i=0;i<num_effects;i++){
    data->automation_values[i] = -100000;
  }
}

// May be called from any thread
static Data *create_effect_plugin_data2(float samplerate, dsp *initialized_dsp){
  Data *data = new Data;
  data->samplerate = samplerate;

  Voice *voice = &data->voices[0];
  voice->dsp_instance = initialized_dsp;
  //printf("Creating %s / %s. samplerate: %d\n",plugin_type->type_name,plugin_type->name,(int)samplerate);
  voice->dsp_instance->buildUserInterface(&voice->myUI);
  voice->myUI.uniqifyEffectNames();

  create_automation_values(data);
  
  return data;
}

#if defined(CLASSNAME)
static void *create_effect_plugin_data(const SoundPluginType *plugin_type, struct SoundPlugin *plugin, hash_t *state, float samplerate, int blocksize, bool is_loading){
  dsp *dsp = new CLASSNAME;
  dsp->instanceInit(samplerate);
  Data *data = create_effect_plugin_data2(samplerate, dsp);

#ifndef FAUST_SYSTEM_EFFECT
  if (plugin != NULL) { // plugin==NULL during instrument type initialization, when we create test data. (took a long time to hunt down this bug)
    data->qtgui_parent = FAUST_create_qdialog(plugin);
    create_gui(data->qtgui_parent, data, plugin);
  }
#endif
  
  return data;
}
#endif

// May be called from any thread
static void convert_effect_data_to_instrument_data(Data *data, dsp *initialized_dsps[MAX_POLYPHONY]){
  for(int i=0;i<MAX_POLYPHONY;i++){
    Voice *voice = &data->voices[i];
    voice->dsp_instance = initialized_dsps[i];
    voice->dsp_instance->buildUserInterface(&voice->myUI);
    voice->myUI.remove_instrument_notecontrol_effects();
    voice->myUI.uniqifyEffectNames();
    
    RT_add_voice(&data->voices_not_playing, voice);
  }  
}

// May be called from any thread
static Data *create_instrument_plugin_data2(float samplerate, dsp *initialized_dsps[MAX_POLYPHONY]){ // [NO_STATIC_ARRAY_WARNING]
  Data *data = new Data;
  data->samplerate = samplerate;

  convert_effect_data_to_instrument_data(data, initialized_dsps);
  
  return data;
}

#if defined(CLASSNAME)
static void *create_instrument_plugin_data(const SoundPluginType *plugin_type, struct SoundPlugin *plugin, hash_t *state, float samplerate, int blocksize, bool is_loading){
  dsp *dsps[MAX_POLYPHONY];
  for(int i=0;i<MAX_POLYPHONY;i++){
    dsps[i] = new CLASSNAME;
    dsps[i]->instanceInit(samplerate);
  }

  Data *data = create_instrument_plugin_data2(samplerate, dsps);

  create_automation_values(data);

  data->qtgui_parent = FAUST_create_qdialog(plugin);

  create_gui(data->qtgui_parent, data, plugin);

  return data;
}
#endif

// need to add include path of loaded faust source

// must be called from main thread
static void delete_dsps_and_data1(Data *data){
  R_ASSERT_RETURN_IF_FALSE(data!=NULL);

  if (data->qtgui==NULL)
    return;

  //printf("          Deleting data->qtgui %p\n", data->qtgui);
  delete data->qtgui;
  data->qtgui = NULL;
}

// May be called from any thread
static void delete_dsps_and_data2(Data *data){

  for(int i=0;i<MAX_POLYPHONY;i++){
    Voice *voice = &data->voices[i];
    if(voice->dsp_instance==NULL) // an effect
      break;
    else
      delete voice->dsp_instance;
  }
  
  delete data;
}

// must be called from main thread
static void delete_dsps_and_data(Data *data){
  delete_dsps_and_data1(data);
  delete_dsps_and_data2(data);
}

static void cleanup_plugin_data(SoundPlugin *plugin){
  Data *data = (Data*)plugin->data;
  delete_dsps_and_data(data);
}

#ifdef FAUST_THAT_ONE
float *FAUST_get_peak_value_pointer(SoundPlugin *plugin, int effect_num){
  Data *data = (Data*)plugin->data;
  MyUI::Controller *controller = &data->voices[0].myUI._controllers.at(effect_num);
  if(controller->peak_port!=NULL)
    return controller->peak_port;
  else
    return NULL;
}
#endif

static int get_effect_format2(const Data *data, int effect_num){
  const Voice *voice = &data->voices[0];
  const MyUI::Controller *controller = &voice->myUI._controllers.at(effect_num);
  return controller->type;
}

static int get_effect_format(struct SoundPlugin *plugin, int effect_num){
  Data *data = (Data*)plugin->data;
  return get_effect_format2(data, effect_num);
}

static const char *get_effect_name2(Data *data, int effect_num){
  Voice *voice = &data->voices[0];
  MyUI::Controller *controller = &voice->myUI._controllers.at(effect_num);
  return controller->name.c_str();
}

static const char *get_effect_name(const struct SoundPlugin *plugin, int effect_num){
  Data *data = (Data*)plugin->data;
  return get_effect_name2(data, effect_num);
}

static void set_effect_value2(Data *data, int effect_num, float value, enum ValueFormat value_format, FX_when when){
  float scaled_value;

  //printf("    Setting %d: %f\n", effect_num, value);

#ifdef FAUST_SYSTEM_EFFECT
  scaled_value = value;
#else
  if(value_format==EFFECT_FORMAT_SCALED){
    MyUI::Controller *controller = &data->voices[0].myUI._controllers.at(effect_num);
    float min = controller->min_value;
    float max = controller->max_value;
    scaled_value = scale(value,0,1,min,max);
  }else{
    scaled_value = value;
  }
#endif

  if (when==FX_start || when==FX_middle || when==FX_end)
    data->automation_values[effect_num] = scaled_value;
      

  //printf("Setting effect %d to %f. input: %f\n",effect_num,scaled_value,value);
  
  for(int i=0;i<MAX_POLYPHONY;i++){
    Voice *voice = &data->voices[i];
    if(voice->dsp_instance==NULL) // an effect
      break;
    MyUI::Controller *controller = &voice->myUI._controllers.at(effect_num);
    safe_float_write(controller->control_port, scaled_value);
  }
}

static void set_effect_value(struct SoundPlugin *plugin, int time, int effect_num, float value, enum ValueFormat value_format, FX_when when){
  Data *data = (Data*)plugin->data;
  set_effect_value2(data, effect_num, value, value_format, when);
}

static float get_effect_value2(Data *data, int effect_num, enum ValueFormat value_format){
  Voice *voice = &data->voices[0];
  MyUI::Controller *controller = &voice->myUI._controllers.at(effect_num);

  //printf("   Getting effect from controller %p\n", controller);
   
#ifdef FAUST_SYSTEM_EFFECT
  return safe_float_read(controller->control_port);
#else
  if(value_format==EFFECT_FORMAT_SCALED){
    float min = controller->min_value;
    float max = controller->max_value;
    return scale(safe_float_read(controller->control_port),min,max,0.0f,1.0f);
  }else{
    return safe_float_read(controller->control_port);
  }
#endif
}

static float get_effect_value(struct SoundPlugin *plugin, int effect_num, enum ValueFormat value_format){
  Data *data = (Data*)plugin->data;
  return get_effect_value2(data, effect_num, value_format);
}

static void get_display_value_string2(Data *data, int effect_num, char *buffer, int buffersize){
  Voice *voice = &data->voices[0];
  MyUI::Controller *controller = &voice->myUI._controllers.at(effect_num);

  if(controller->type==EFFECT_FORMAT_INT)
    snprintf(buffer,buffersize-1,"%d %s",(int)safe_float_read(controller->control_port), controller->unit);
  else
    snprintf(buffer,buffersize-1,"%.2f %s",safe_float_read(controller->control_port), controller->unit);
}

static void get_display_value_string(struct SoundPlugin *plugin, int effect_num, char *buffer, int buffersize){
  Data *data = (Data*)plugin->data;
  get_display_value_string2(data, effect_num, buffer, buffersize);
}

static const char *get_effect_description2(const Data *data, int effect_num){
  const Voice *voice = &data->voices[0];
  const MyUI::Controller *controller = &voice->myUI._controllers.at(effect_num);

  return controller->tooltip;
}

static const char *get_effect_description(struct SoundPlugin *plugin, int effect_num){
  Data *data = (Data*)plugin->data;
  return get_effect_description2(data, effect_num);
}


static bool show_gui2(Data* data, SoundPlugin *plugin, int64_t parentgui){
  //printf("   Showing gui %p\n",data->qtgui);
  
  int64_t guinum = API_get_gui_from_existing_widget(data->qtgui_parent);
  gui_setParent(guinum, -1); // We let main window be parent. Other windows might not work very well as parent, for some reason.
  gui_show(guinum);

  data->qtgui->run();

  return true;
}

static bool show_gui(struct SoundPlugin *plugin, int64_t parentgui){
  Data *data = (Data*)plugin->data;
  return show_gui2(data, plugin, parentgui);
}

static void hide_gui2(Data *data){
  //printf("   Hiding gui %p\n",data->qtgui);

  int64_t guinum = API_get_gui_from_existing_widget(data->qtgui_parent);
  gui_hide(guinum);

  data->qtgui->stop();
}

static void hide_gui(struct SoundPlugin *plugin){
  Data *data = (Data*)plugin->data;
  hide_gui2(data);
}

static bool gui_is_visible(struct SoundPlugin *plugin){
  Data *data = (Data*)plugin->data;
  if (data->qtgui_parent==NULL)
    return false;
  else
    return data->qtgui_parent->isVisible();
}

#if defined(CLASSNAME)

static void fill_type(SoundPluginType *type){
 type->type_name                = "Faust";
 type->note_handling_is_RT      = false;
 type->get_effect_format        = get_effect_format;
 type->get_effect_name          = get_effect_name;
 type->effect_is_RT             = NULL;
 type->cleanup_plugin_data      = cleanup_plugin_data;

 type->play_note       = play_note;
 type->set_note_volume = set_note_volume;
 type->set_note_pitch  = set_note_pitch;

 type->stop_note       = stop_note;

 type->set_effect_value         = set_effect_value;
 type->get_effect_value         = get_effect_value;
 type->get_display_value_string = get_display_value_string;
 type->get_effect_description   = get_effect_description;

#ifndef FAUST_SYSTEM_EFFECT
 if (strcmp(DSP_NAME, "Multiband Compressor")){   
   type->show_gui = show_gui;
   type->hide_gui = hide_gui; 
   type->gui_is_visible = gui_is_visible;    
 }
#endif
 
 type->data                     = NULL;
};

static SoundPluginType faust_type = {};  // c++ way of zero-initialization without getting missing-field-initializers warning.

void CREATE_NAME (void){
  static bool has_inited = false;

  if (has_inited==false) {
    
    fill_type(&faust_type);
  
    CLASSNAME::classInit(MIXER_get_sample_rate());

    // TODO: Don't do this. Just dispatch during runtime which rt_process to call.
    //
    Data *data = (Data*)create_effect_plugin_data(&faust_type, NULL, NULL, MIXER_get_sample_rate(), MIXER_get_buffer_size(), false);
  
    faust_type.name = DSP_NAME;

    faust_type.num_inputs = data->voices[0].dsp_instance->getNumInputs();
    faust_type.num_outputs = data->voices[0].dsp_instance->getNumOutputs();

    if(data->voices[0].myUI.is_instrument()){

      faust_type.is_instrument      = true;
      
      faust_type.RT_process         = RT_process_instrument;
      faust_type.create_plugin_data = create_instrument_plugin_data;

      data->voices[0].myUI.remove_instrument_notecontrol_effects();
      
    }else{

      faust_type.is_instrument      = false;
      
      faust_type.RT_process         = RT_process_effect;
      faust_type.create_plugin_data = create_effect_plugin_data;
      
      faust_type.play_note          = NULL;
      faust_type.set_note_volume    = NULL;
      faust_type.stop_note          = NULL;
      
    }
    
    faust_type.num_effects = data->voices[0].myUI._num_effects;

    has_inited = true;

  }
  
  PR_add_plugin_type(&faust_type);
}

#endif
