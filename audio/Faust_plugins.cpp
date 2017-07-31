

#include <math.h>

#include <unistd.h>

// We use faust2 here.
//#define QTGUI FAUST2_QTGUI

#include "../bin/packages/faust2/compiler/generator/llvm/llvm-dsp.h"

#if __GNUC__ >= 5
#  pragma GCC diagnostic push
#  pragma GCC diagnostic ignored "-Wsuggest-override"
#endif
#include "../bin/packages/faust2/compiler/libfaust.h"

#include "../bin/packages/faust2/architecture/faust/dsp/dsp.h"
#include "../bin/packages/faust2/architecture/faust/gui/UI.h"
#include "../bin/packages/faust2/architecture/faust/gui/GUI.h"
#if __GNUC__ >= 5
#  pragma GCC diagnostic pop
#endif

#include "../common/nsmtracker.h"

#include "../Qt/FocusSniffers.h"
//}

#if __GNUC__ >= 5
#  pragma GCC diagnostic push
#  pragma GCC diagnostic ignored "-Wsuggest-override"
#endif
#include "../bin/packages/faust2/architecture/faust/gui/faustqt.h"
#if __GNUC__ >= 5
#  pragma GCC diagnostic pop
#endif

#include "mfaustqt2.cpp"

std::list<GUI*>  GUI::fGuiList;




#include "../common/visual_proc.h"
#include "../common/patch_proc.h"

#include "../Qt/Qt_instruments_proc.h"
#include "../Qt/EditorWidget.h"

#include "SoundPlugin.h"
#include "SoundPlugin_proc.h"

#include "SoundPluginRegistry_proc.h"

namespace{
struct Data;
}

static Data *GET_DATA_FROM_PLUGIN(SoundPlugin *plugin);
#include "Faust_plugins_template1.cpp"
#include "Faust_plugins_template2.cpp"

#include "Faust_plugins_proc.h"


#define MAX_CHANNELS 16
#define MAX_EFFECTS 1024

namespace{

  static radium::Vector<QDialog*> g_faustqdialogs;
  static QString g_qtgui_stylesheet;

  class FaustQDialog : public RememberGeometryQDialog{

  public:
    FaustQDialog(QWidget *parent)
      : RememberGeometryQDialog(parent, radium::NOT_MODAL)
    {
      //QStyle *style = QStyleFactory::create("plastique");
      //if (style!=NULL)
      //  setStyle(style);

      if(g_qtgui_stylesheet=="")
        FAUST_change_qtguistyle(getFaustGuiStyle());

      if (g_qtgui_stylesheet != "")
        setStyleSheet(g_qtgui_stylesheet);
      
      g_faustqdialogs.push_back(this);
    }

    ~FaustQDialog(){
      g_faustqdialogs.remove(this);
    }
  };
}

QDialog *FAUST_create_qdialog(void){
  return new FaustQDialog(g_main_window);
}

void FAUST_change_qtguistyle(const char *style_name){

  QString filename = OS_get_full_program_file_path("packages/faust2/architecture/faust/gui/Styles/" + QString(getFaustGuiStyle()) + ".qss");
  disk_t *disk = DISK_open_for_reading(filename);
  if (disk==NULL){
      
    GFX_Message2(NULL, true, "File not found (%s)", filename.toUtf8().constData());
    
  } else {
    
    QString stylesheet = DISK_read_qstring_file(disk);
    if (DISK_close_and_delete(disk)==false) {
      GFX_Message2(NULL, true, "Unable to read from %s", filename.toUtf8().constData());
      stylesheet = "";
    }

    if (stylesheet!="")
      g_qtgui_stylesheet = stylesheet;
  }

  if (g_qtgui_stylesheet != "")
    for(auto faustqdialog : g_faustqdialogs)
      faustqdialog->setStyleSheet(g_qtgui_stylesheet);
  
}

#if 0
void FAUST_set_qtguistyle(QDialog *gui){
  /*
  if(g_qtgui_stylesheet=="")
    FAUST_change_qtguistyle(getFaustGuiStyle());

  if (g_qtgui_stylesheet != "")
    gui->setStyleSheet(g_qtgui_stylesheet);  
  */
}
#endif



#ifdef WITH_FAUST_DEV




#include "Faust_factory_factory.cpp"


static int64_t g_id = 0;

namespace{
struct Devdata{
  int64_t id;
  
  QString code;
  QString options;
  
  FFF_Reply reply;

  bool is_compiling; // <-- Can only be trusted if sending one request at a time. (used by the Faust_Plugin_widget constructor)

  QDialog *qtgui_parent;
  
  Devdata()
    : id(g_id++)
    , options("-I\n%radium_path%/packages/faust2/architecture")
    , reply(fff_empty_reply)
    , is_compiling(false)
    , qtgui_parent(NULL)
  {
  }

  ~Devdata(){
    delete qtgui_parent;
  }
};
}

static Data *GET_DATA_FROM_PLUGIN(SoundPlugin *plugin){
  Devdata *devdata = (Devdata*)plugin->data;
  Data *data = devdata->reply.data;
  return data;
}

static void dev_RT_process(SoundPlugin *plugin, int64_t time, int num_frames, float **inputs, float **outputs){
  Devdata *devdata = (Devdata*)plugin->data;
  Data *data = devdata->reply.data;

  int num_inputs = 0;
  int num_outputs = 0;

  if (data != NULL) {
    num_inputs = data->voices[0].dsp_instance->getNumInputs();
    num_outputs = data->voices[0].dsp_instance->getNumOutputs();

    if (num_inputs > MAX_CHANNELS || num_outputs > MAX_CHANNELS){
      for(int ch = 0 ; ch < MAX_CHANNELS ; ch++)
        memset(outputs[ch], 0, num_frames*sizeof(float));
      return;
    }

    if (devdata->reply.is_instrument)
      RT_process_instrument2(num_outputs, data, time, num_frames, inputs, outputs);
    else
      RT_process_effect2(data, time, num_frames, inputs, outputs);
  }

  // clear unused channels
  for(int ch = num_outputs ; ch < MAX_CHANNELS ; ch++)
    memset(outputs[ch], 0, num_frames*sizeof(float));

}

static void dev_play_note(struct SoundPlugin *plugin, int time, note_t note){
  Devdata *devdata = (Devdata*)plugin->data;
  Data *data = devdata->reply.data;
  
  if (data != NULL)
    if (devdata->reply.is_instrument)
      play_note2(data, time, note);
}

static void dev_set_note_volume(struct SoundPlugin *plugin, int time, note_t note){
  Devdata *devdata = (Devdata*)plugin->data;
  Data *data = devdata->reply.data;
  
  if (data != NULL)
    if (devdata->reply.is_instrument)
      set_note_volume2(data, time, note);
}

static void dev_set_note_pitch(struct SoundPlugin *plugin, int time, note_t note){
  Devdata *devdata = (Devdata*)plugin->data;
  Data *data = devdata->reply.data;
  
  if (data != NULL)
    if (devdata->reply.is_instrument)
      set_note_pitch2(data, time, note);
}

static void dev_stop_note(struct SoundPlugin *plugin, int time, note_t note){
  Devdata *devdata = (Devdata*)plugin->data;
  Data *data = devdata->reply.data;
  
  if (data != NULL)
    if (devdata->reply.is_instrument)
      stop_note2(data, time, note);
} 

static int get_num_effects(const Data *data) {
  return data->voices[0].myUI._num_effects;
}

static void dev_set_effect_value(struct SoundPlugin *plugin, int time, int effect_num, float value, enum ValueFormat value_format, FX_when when){
  Devdata *devdata = (Devdata*)plugin->data;
  Data *data = devdata->reply.data;
  
  if (data != NULL)
    if (effect_num < get_num_effects(data))
      set_effect_value2(data, effect_num, value, value_format, when);
}

static float dev_get_effect_value(struct SoundPlugin *plugin, int effect_num, enum ValueFormat value_format){
  Devdata *devdata = (Devdata*)plugin->data;
  Data *data = devdata->reply.data;
  
  if (data==NULL || effect_num >= get_num_effects(data))
    return 0.5;
  
  return get_effect_value2(data, effect_num, value_format);
}

static void dev_get_display_value_string(SoundPlugin *plugin, int effect_num, char *buffer, int buffersize){
  Devdata *devdata = (Devdata*)plugin->data;
  Data *data = devdata->reply.data;
  
  if (data==NULL || effect_num >= get_num_effects(data)){
    snprintf(buffer,buffersize," ");
    return;
  }

  get_display_value_string2(data, effect_num, buffer, buffersize);
}

static void create_state(struct SoundPlugin *plugin, hash_t *state){
  printf("\n\n\n ********** CREATE_STATE ************* \n\n\n");
  Devdata *devdata = (Devdata*)plugin->data;

  HASH_put_string(state, "code", STRING_toBase64(STRING_create(devdata->code)));
  HASH_put_string(state, "options", STRING_toBase64(STRING_create(devdata->options)));
}

static void *dev_create_plugin_data(const SoundPluginType *plugin_type, SoundPlugin *plugin, hash_t *state, float sample_rate, int block_size, bool is_loading){
  Devdata *devdata = new Devdata;

  plugin->data = devdata;

  if (state!=NULL) {
    devdata->code = STRING_get_qstring(STRING_fromBase64(HASH_get_string(state, "code")));
    devdata->options = STRING_get_qstring(STRING_fromBase64(HASH_get_string(state, "options")));
  } else
    devdata->code = DEFAULT_FAUST_DEV_PROGRAM;


  if (is_loading==false) {
    
    FAUST_start_compilation(plugin);
  
  } else {
  
    if (FAUST_compile_now(plugin, is_loading)==false){
      GFX_Message(NULL, "Something went wrong when compiling: %s", devdata->reply.error_message.toUtf8().constData());
      plugin->data = NULL;
      delete devdata;
      return NULL;
    }
    
  }
  
  return devdata;
}

static void dev_cleanup_plugin_data(SoundPlugin *plugin){
  fprintf(stderr,">>>>>>>>>>>>>> Cleanup_plugin_devdata called for %p\n",plugin);

  Devdata *devdata = (Devdata*)plugin->data;

  FFF_free_now(devdata->reply);

  // We can lose memory by not receiving replies in the queue, but that won't crash the program or make it unstable.
  // This should be so rare though, plus that the only consequency is to waste a little bit of memory, that it's probably not worth fixing.
  delete devdata;
}

static int dev_get_effect_format(struct SoundPlugin *plugin, int effect_num){
  Devdata *devdata = (Devdata*)plugin->data;
  Data *data = devdata->reply.data;
  
  if (data == NULL || effect_num >= get_num_effects(data))
    return EFFECT_FORMAT_FLOAT;
  
  return get_effect_format2(data, effect_num);
}

static const char *dev_get_effect_name(struct SoundPlugin *plugin, int effect_num){
  Devdata *devdata = (Devdata*)plugin->data;
  Data *data = devdata->reply.data;
  
  if (data==NULL || effect_num >= get_num_effects(data))
    return NOTUSED_EFFECT_NAME;
  
  return get_effect_name2(data, effect_num);
}

static const char *dev_get_effect_description(struct SoundPlugin *plugin, int effect_num){
  Devdata *devdata = (Devdata*)plugin->data;
  Data *data = devdata->reply.data;
  
  if (data==NULL || effect_num >= get_num_effects(data))
    return "";
  
  return get_effect_description2(data, effect_num);
}


static bool dev_effect_is_visible(struct SoundPlugin *plugin, int effect_num){
  Devdata *devdata = (Devdata*)plugin->data;
  Data *data = devdata->reply.data;
  
  if (data==NULL || effect_num >= get_num_effects(data))
    return false;
  else
    return true;
}

static void dev_show_gui(struct SoundPlugin *plugin){
  Devdata *devdata = (Devdata*)plugin->data;
  Data *data = devdata->reply.data;

  if (data!=NULL) {
    safeShow(devdata->qtgui_parent);
    data->qtgui->run();
  }
}

static void dev_hide_gui(struct SoundPlugin *plugin){
  Devdata *devdata = (Devdata*)plugin->data;
  Data *data = devdata->reply.data;

  if (devdata->qtgui_parent != NULL){
    devdata->qtgui_parent->hide();
  }
  
  if (data!=NULL)
    data->qtgui->stop();
}

static bool dev_gui_is_visible(struct SoundPlugin *plugin){
  Devdata *devdata = (Devdata*)plugin->data;
  if (devdata->qtgui_parent==NULL)
    return false;
  else
    return devdata->qtgui_parent->isVisible();
}

void create_faust_plugin(void){
  SoundPluginType *plugin_type = (SoundPluginType*)V_calloc(1,sizeof(SoundPluginType));

  plugin_type->type_name                = "Faust Dev";
  plugin_type->name                     = "Faust Dev";
  plugin_type->num_inputs               = MAX_CHANNELS;
  plugin_type->num_outputs              = MAX_CHANNELS;
  plugin_type->is_instrument            = true;
  plugin_type->note_handling_is_RT      = false;
  plugin_type->num_effects              = MAX_EFFECTS;
  plugin_type->get_effect_format        = dev_get_effect_format;
  plugin_type->get_effect_name          = dev_get_effect_name;
  plugin_type->effect_is_RT             = NULL;
  plugin_type->create_state             = create_state;
  plugin_type->create_plugin_data       = dev_create_plugin_data;
  plugin_type->cleanup_plugin_data      = dev_cleanup_plugin_data;

  plugin_type->RT_process       = dev_RT_process;
  plugin_type->play_note        = dev_play_note;
  plugin_type->set_note_volume  = dev_set_note_volume;
  plugin_type->set_note_pitch   = dev_set_note_pitch;
  plugin_type->stop_note        = dev_stop_note;
  plugin_type->set_effect_value = dev_set_effect_value;
  plugin_type->get_effect_value = dev_get_effect_value;
  plugin_type->get_display_value_string = dev_get_display_value_string;
  plugin_type->get_effect_description   = dev_get_effect_description;
  plugin_type->effect_is_visible = dev_effect_is_visible;

  plugin_type->show_gui = dev_show_gui;
  plugin_type->hide_gui = dev_hide_gui;
  plugin_type->gui_is_visible = dev_gui_is_visible;

  //plugin_type->plugin_takes_care_of_savable_values = true;
    
  plugin_type->info =
    "FAUST (Functional Audio Stream) is a functional programming language specifically designed for real-time signal processing and synthesis. FAUST targets high-performance signal processing applications and audio plug-ins for a variety of platforms and standards. More info <A href=\"http://faust.grame.fr\">here</a>."
    "<p>"
    "Hints:\n"
    "<UL>"
    "<LI> To zoom, either the editor or a diagram, press CTRL while scrolling the mouse wheel."
    "<LI> To search for a string in the source code, press Ctrl + F."
    "<LI> Running full size window (by pressing the \"Full\" button) can be very convenient when developing."
    "</UL"
    ;

  PR_add_plugin_type(plugin_type);
}

/*
void FAUST_inform_about_instrument_gui(SoundPlugin *plugin, QWidget *instrument_gui){
  Devdata *devdata = (Devdata*)plugin->data;
  devdata->instrument_gui = instrument_gui;
}
*/

void FAUST_set_code(SoundPlugin *plugin, QString code){
  Devdata *devdata = (Devdata*)plugin->data;  
  devdata->code = code;
}

void FAUST_set_options(SoundPlugin *plugin, QString options){
  Devdata *devdata = (Devdata*)plugin->data;  
  devdata->options = options;
}


// Can only be trusted if sending one request at a time.
bool FAUST_is_compiling(const struct SoundPlugin *plugin){
  Devdata *devdata = (Devdata*)plugin->data;
  return devdata->is_compiling;
}
  
QString FAUST_get_code(const struct SoundPlugin *plugin){
  Devdata *devdata = (Devdata*)plugin->data;
  return devdata->code;
}

QString FAUST_get_options(const struct SoundPlugin *plugin){
  Devdata *devdata = (Devdata*)plugin->data;
  return devdata->options;
}

QString FAUST_get_cpp_code(const struct SoundPlugin *plugin){
  Devdata *devdata = (Devdata*)plugin->data;
  if (devdata->reply.svg_dir==NULL || devdata->reply.svg_dir->isValid()==false)
    return "";

  QString filename = devdata->reply.svg_dir->path() + QDir::separator() + "cppsource.cpp";
  disk_t *disk = DISK_open_for_reading(filename);

  if (disk==NULL){
    GFX_Message(NULL, "File not found (%s)", filename.toUtf8().constData());
    return "";
  }

  QString cpp_code = DISK_read_qstring_file(disk);
      
  if (DISK_close_and_delete(disk)==false) {
    GFX_Message(NULL, "Unable to read from %s", filename.toUtf8().constData());
    return "";
  }

  return cpp_code;
}

QString FAUST_get_error_message(const struct SoundPlugin *plugin){
  Devdata *devdata = (Devdata*)plugin->data;
  return devdata->reply.error_message;
}

QString FAUST_get_svg_path(const struct SoundPlugin *plugin){
  Devdata *devdata = (Devdata*)plugin->data;
  if (devdata->reply.svg_dir==NULL || devdata->reply.svg_dir->isValid()==false)
    return "";
  else
    return devdata->reply.svg_dir->path() + QDir::separator() + "FaustDev-svg" + QDir::separator() + "process.svg";
}




static bool FAUST_handle_fff_reply(struct SoundPlugin *plugin, const FFF_Reply &reply, bool is_initializing){
  Devdata *devdata = (Devdata*)plugin->data;
  
  if (reply.data==NULL){    
    fprintf(stderr,"Error-message: -%s-\n", devdata->reply.error_message.toUtf8().constData());
    devdata->reply.error_message = reply.error_message;
    return false;
  }

  FFF_Reply old_reply = devdata->reply;

  // handle gui
  {
    if (devdata->qtgui_parent == NULL)
      devdata->qtgui_parent = FAUST_create_qdialog();
    
    if (old_reply.data != NULL && old_reply.data->qtgui!=NULL){
      old_reply.data->qtgui->stop();
      devdata->qtgui_parent->layout()->removeWidget(old_reply.data->qtgui);
    }

    create_gui(devdata->qtgui_parent, reply.data, plugin);

    if (devdata->qtgui_parent->isVisible())
      reply.data->qtgui->run();    
  }
  
  hash_t *effects_state = is_initializing ? NULL : PLUGIN_get_effects_state(plugin);

  PLAYER_lock();{
    devdata->reply = reply;
  }PLAYER_unlock();

  if (effects_state != NULL)
    PLUGIN_set_effects_from_state(plugin, effects_state);

  if (old_reply.data != NULL){
    struct Patch *patch = (struct Patch*)plugin->patch;
    PATCH_handle_fxs_when_fx_names_have_changed(patch, true);
  }

  FFF_request_free(devdata->id, old_reply);

  return true;
}

FAUST_calledRegularlyByParentReply FAUST_calledRegularlyByParent(struct SoundPlugin *plugin){
  Devdata *devdata = (Devdata*)plugin->data;
  
  FFF_Reply reply = FFF_get_reply(devdata->id);
  
  if (reply.is_empty())
    return Faust_No_New_Reply;

  devdata->is_compiling = false;
  
  if (FAUST_handle_fff_reply(plugin, reply, false))
    return Faust_Success;
  else
    return Faust_Failed;
}



void FAUST_start_compilation(struct SoundPlugin *plugin){
  Devdata *devdata = (Devdata*)plugin->data;

  devdata->is_compiling = true;
  FFF_request_reply(devdata->id, devdata->code, devdata->options); 
}


bool FAUST_compile_now(struct SoundPlugin *plugin, bool is_initializing){
  Devdata *devdata = (Devdata*)plugin->data;

  return FAUST_handle_fff_reply(plugin, FFF_get_reply_now(devdata->code, devdata->options), is_initializing);
}


#endif
