/* Copyright 2012 Kjetil S. Matheussen

This program is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License
as published by the Free Software Foundation; either version 2
of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA. */



#include <stdlib.h>
#include <string.h>
#include <stdio.h>

#include <vector>

#include <QString>
#include <QFileDialog>
#include <QAbstractButton>

#include "../common/nsmtracker.h"
#include "../common/settings_proc.h"
#include "../common/vector_proc.h"
#include "../common/visual_proc.h"

#include "../Qt/helpers.h"

#include "SoundPlugin.h"
#include "Jack_plugin_proc.h"
#include "VST_plugins_proc.h"

#include "../mixergui/QM_MixerWidget.h"

#include "../api/api_instruments_proc.h"

#include "SoundPluginRegistry_proc.h"


static radium::Vector<SoundPluginType*> g_plugin_types;
static radium::Vector<SoundPluginTypeContainer*> g_plugin_type_containers;

static QVector<PluginMenuEntry> g_plugin_menu_entries;


int PR_get_num_plugin_types(void){
  return g_plugin_types.size();
}

static bool get_hepp_from_config_line(const char *line_c, NumUsedPluginEntry &hepp){

  // Example input: "plugin_usage_Sample Player_-_Click = 0"

  QString line = line_c;
  
  line = line.trimmed();
  
  line = line.right(line.size() - QString("plugin_usage_(").size() + 1);



  //////////
  
  int pos = line.lastIndexOf("=");
  if (pos==-1)
    return false;
  
  QString numstring = line.right(line.size() - pos - 1);
  hepp.num_uses = (int)atoll(numstring.trimmed().toUtf8().constData());
  if (hepp.num_uses == 0)
    return false;
  
  line = line.left(pos-1);
  


  
  ///////////
  
  pos = line.indexOf("_-_");
  if (pos==-1)
    return false;
  
  hepp.type_name = line.left(pos);
  line = line.right(line.size() - pos - 3);
  
  
  ///////////
  
  pos = line.indexOf("_-_");
  if (pos==-1)
    return false;
  
  hepp.container_name = line.left(pos);
  line = line.right(line.size() - pos - 3);

  
  //////////
  
  hepp.name = QString(line);

  /*
  if (hepp.name.startsWith("STK "))
    hepp.menu_text = "STK:" + hepp.name.right(hepp.name.size()-3);
  else
    hepp.menu_text = hepp.type_name + ": " + hepp.name;
  
  hepp.menu_text += " (" + QString::number(hepp.num_uses) + ")";
  */
  
  //printf("hepp. %d: -%s- / -%s- / -%s- (%s)\n",hepp.num_uses, hepp.container_name.toUtf8().constData(), hepp.type_name.toUtf8().constData(), hepp.name.toUtf8().constData(),hepp.menu_text.toUtf8().constData());
  
  return true;
}

static bool compare_hepps(const NumUsedPluginEntry &h1, const NumUsedPluginEntry &h2){
  if (h1.num_uses > h2.num_uses)
    return true;
  else
    return false;
}
    

namespace{
struct Favourites{
  
  QVector<NumUsedPluginEntry> hepps;
  QHash<QString, QHash<QString, int> > num_uses_normal;
  QHash<QString, QHash<QString, int> > num_uses_container;
  
  Favourites(){

    vector_t *lines = SETTINGS_get_all_lines_starting_with("plugin_usage_");

    VECTOR_FOR_EACH(const char *, line_c, lines){
      NumUsedPluginEntry hepp;
      if (get_hepp_from_config_line(line_c, hepp)){
        
        hepps.push_back(hepp);
        
        if (hepp.container_name!=""){
          int old = num_uses_container[hepp.container_name][hepp.type_name];
          num_uses_container[hepp.container_name][hepp.type_name] = old+hepp.num_uses;
          //if (hepp.num_uses > 0)
          //  printf("    CONT ADDING %d to %s\n", hepp.num_uses, hepp.container_name.toUtf8().constData());
        }
        
        num_uses_normal[hepp.type_name][hepp.name] = hepp.num_uses;
        //if (hepp.num_uses > 0)
        //  printf("    NORM ADDING %d to %s / %s\n", hepp.num_uses, hepp.type_name.toUtf8().constData(), hepp.name.toUtf8().constData());
      }
    }END_VECTOR_FOR_EACH;

    qSort(hepps.begin(), hepps.end(), compare_hepps);
  }

  void set_num_uses(SoundPluginType *type){
    type->num_uses = num_uses_normal[type->type_name][type->name];
  }

  void set_num_uses(SoundPluginTypeContainer *container){
    container->num_uses = num_uses_container[container->name][container->type_name];
      
    if (container->is_populated){
      for(int i=0 ; i < container->num_types ; i++)
        set_num_uses(container->plugin_types[i]);
    }
  }

  void set_num_uses(const PluginMenuEntry &entry){
    if (entry.type==PluginMenuEntry::IS_NORMAL)
      set_num_uses(entry.plugin_type);
    else if (entry.type==PluginMenuEntry::IS_CONTAINER)
      set_num_uses(entry.plugin_type_container);
  }
};
}

static Favourites *g_favourites = NULL;

static void recreate_favourites(bool set_num_uses_questionmark){
  delete g_favourites;
  g_favourites = new Favourites;

  if (set_num_uses_questionmark)
    for(const PluginMenuEntry &entry : g_plugin_menu_entries)
      g_favourites->set_num_uses(entry);
}

static Favourites *get_favourites(void){
  if (g_favourites == NULL)
    recreate_favourites(true);

  return g_favourites;
}

void PR_inc_plugin_usage_number(SoundPluginType *type){
  char *settings_name = talloc_format("plugin_usage_%s_-_%s_-_%s", type->type_name, type->container==NULL ? "" : type->container->name, type->name);
  int new_num_uses = SETTINGS_read_int32(settings_name, 0) + 1;
  SETTINGS_write_int(settings_name, new_num_uses);
  type->num_uses = new_num_uses;
  recreate_favourites(false);
}

enum PopulateResult{
  PR_ALREADY_POPULATED,
  PR_POPULATED,
  PR_POPULATE_CANCELLED
};

static enum PopulateResult populate(SoundPluginTypeContainer* container){
  R_ASSERT_RETURN_IF_FALSE2(container!=NULL, PR_POPULATE_CANCELLED);

  if (API_container_is_blacklisted(container)){
    
    vector_t v = {};
    int load_it = VECTOR_push_back(&v, "Open anyway");
    int cancel = VECTOR_push_back(&v, "Cancel");
    (void)cancel;

    int hmm=GFX_Message(&v, "Warning: The plugin file \"%s\" crashed last time we tried to open it.", STRING_get_chars(container->filename));

    if (hmm != load_it)
      return PR_POPULATE_CANCELLED;
    
  } else {
    
    API_blacklist_container(container);
    
  }

  enum PopulateContainerResult populate_container_result = container->populate(container);
  
  recreate_favourites(false);
  
  g_favourites->set_num_uses(container);
  
  API_incSoundPluginRegistryGeneration();
  //API_add_disk_entries_from_populated_container(container);

  if (populate_container_result != POPULATE_RESULT_PLUGIN_MUST_BE_BLACKLISTED)
    API_unblacklist_container(container); // Wait as long as possible to unblacklist.

  return PR_POPULATED;
}

static SoundPluginTypeContainer *find_first_populated_container_or_NULL(const QVector<SoundPluginTypeContainer*> &containers){
  for(auto *container : containers){
    if(container->is_populated)
      return container;
  }

  return NULL;
}


static QVector<SoundPluginTypeContainer*> PR_get_all_containers_matching(const char *container_name, const char *type_name){
  QVector<SoundPluginTypeContainer*> ret;
  
  for(auto container : g_plugin_type_containers)
    if(!strcmp(container->type_name,type_name))
      if(!strcmp(container->name,container_name))
        ret.push_back(container);

  return ret;
}

// Will populate container if it isn't already populated
SoundPluginTypeContainer *PR_get_populated_container(const char *container_name, const char *type_name){

  QVector<SoundPluginTypeContainer*> containers = PR_get_all_containers_matching(container_name, type_name);

  if (containers.size()==0)
    return NULL;

  // 1. A populated container with plugins?
  //
  for(auto *container : containers){
    if (container->is_populated && container->num_types > 0){
      return container;
    }
  }


  // 2. No populated container with plugins.
  // Now: Populate all containers that are not populated, <strike>not blacklisted,</strike> and not known to have zero plugins.
  //
  QVector<SoundPluginTypeContainer*> populated_containers;

  bool user_has_cancelled_scanning = false;

  for(auto *container : containers){
    //printf(" ---- CHECKING container \"%s / %s\". Is populated: %d\n", container->type_name, container->name, container->is_populated);
    if (!container->is_populated){ // && !API_container_is_blacklisted(container)){
      int num_previously_recorded_entries = API_get_num_entries_in_disk_container(container);
      if (num_previously_recorded_entries > 0 || num_previously_recorded_entries==-1){
        populate(container);
        //printf("  Able to populate? -%d-. Num types: %d\n", container->is_populated, container->num_types);
        if (container->is_populated)
          populated_containers.push_back(container);
        else
          user_has_cancelled_scanning = true;
      }
    }
  }

  
  if (populated_containers.size()==0)
    return find_first_populated_container_or_NULL(containers);
  
  // Among the populated containers, find all the usable ones, and return the first one.
  //
  QVector<SoundPluginTypeContainer*> usable_containers;

  for(auto *container : populated_containers){
    if (container->is_populated && container->num_types > 0){
      usable_containers.push_back(container);
    }
  }

  if(usable_containers.size()==0){
    if (user_has_cancelled_scanning==false)
      GFX_addMessage(
                     "Could not find a usable plugin file for %s.\n"
                     "It might help to rescan plugins in the plugin manager and try again.",
                     container_name
                     );

    return find_first_populated_container_or_NULL(containers);
  }
  
  if(usable_containers.size() > 1 && !GFX_Message_ignore_questionmark()){
    QString x = QString("Warning: ") + QString::number(usable_containers.size()) + " different usable plugin files for " + container_name + " was loaded.<br><br>";
    
    x += QString("If there are overlaps in the plugins that these files provide,<br>") +
      "plugins from \"" + STRING_get_qstring(usable_containers[0]->filename) + "\" will be used.<br>";

    x += "<UL>Files:";
    for(auto *container : containers)
      x += "<LI>" + STRING_get_qstring(container->filename);

    x += "</UL>";

    GFX_addMessage("%s", x.toUtf8().constData());
  }

  return usable_containers[0];
}

bool PR_ensure_container_is_populated(const char *container_name, const char *type_name){
  SoundPluginTypeContainer *container = PR_get_populated_container(container_name, type_name);
  if (container==NULL)
    return false;
  
  R_ASSERT(container->is_populated);
  return true;
}

static SoundPluginType *PR_get_plugin_type_by_name(const char *type_name, const char *plugin_name){
  return PR_get_plugin_type_by_name(NULL, type_name, plugin_name);
}

SoundPluginType *PR_get_plugin_type_by_name2(const char *container_name, const char *type_name, const char *plugin_name){
  for(SoundPluginType *plugin_type : g_plugin_types)
    if(!strcmp(plugin_type->type_name,type_name))
      if(!strcmp(plugin_type->name,plugin_name))
        return plugin_type;
  return NULL;
}

SoundPluginType *PR_get_plugin_type_by_name(const char *container_name, const char *type_name, const char *plugin_name){

  // Compatibility with older songs
  if (!strcmp(type_name, "Patchbay") && !strcmp(plugin_name, "Patchbay"))
    plugin_name = "Patchbay 8x8";

  
  {
    if (container_name != NULL)
      PR_ensure_container_is_populated(container_name, type_name);

    auto *type = PR_get_plugin_type_by_name2(container_name, type_name, plugin_name);
    if (type != NULL)
      return type;
  }
    
  
  // Older songs didn't store vst container names (because there were no containers). Try to set container_name to plugin_name and try again.
  if(!strcmp(type_name,"VST") && container_name==NULL){
    return PR_get_plugin_type_by_name(plugin_name,type_name,plugin_name);
  }

  if(!strcmp(type_name,"VST") || !strcmp(type_name,"VST3") || !strcmp(type_name,"AudioUnit")){
    while(true){
      vector_t v = {}; // c++ way of zero-initialization without getting missing-field-initializers warning.

      int select_plugin_file = VECTOR_push_back(&v, "Select plugin file");
      //int use_different_plugin = VECTOR_push_back(&v, "Use different plugin");
      int replace_with_pipe = VECTOR_push_back(&v, "Replace with pipe");
      (void)replace_with_pipe; // default
      
      int ret = GFX_Message(&v, "%s", ("VST Plugin " + QString(plugin_name) + " not found.").toUtf8().constData());

      if (ret==select_plugin_file) {

        QString filename;
        
        {
          R_ASSERT(g_radium_runs_custom_exec==false);

          radium::ScopedExec scopedExec(false);
          filename = QFileDialog::getOpenFileName(NULL,
                                                  plugin_name,
                                                  QString(),
                                                  QString(),
                                                  0,
                                                  QFileDialog::DontUseCustomDirectoryIcons | (useNativeFileRequesters() ? (QFileDialog::Option)0 : QFileDialog::DontUseNativeDialog)
                                                  );
        }

        QFileInfo info(filename);

        QString basename = info.fileName();
        basename.resize(basename.size()-strlen(VST_SUFFIX)-1);

        //        if(basename==QString(plugin_name) && info.exists()){
        VST_add_path(info.dir().path());
        PR_init_plugin_types();
        return PR_get_plugin_type_by_name(container_name, type_name, plugin_name);
        //}
        /*
      } else if (ret==use_different_plugin) {
        SoundPluginType *plugintype = MW_popup_plugin_type_selector(false, false);
        if (plugintype==NULL)
          break;
        else
          return plugintype;
        */
      } else
        break;
    }

  }else if(!strcmp(type_name,"Pd") && strcmp(plugin_name, "")){ // type_name doesn't mean anything for already saved files. Without this excpetion, plugins would be replaced by pipes if a pd patch file was renamed.
    return PR_get_plugin_type_by_name("Pd", "");

  }else{

    QString message = "Plugin " + QString(type_name) + " / " + QString(plugin_name) + " not found. Replacing with a Pipe.";
  
    addMessage(message.toUtf8().constData());
    /*
    ScopedQPointer<MyQMessageBox> msgBox(MyQMessageBox::create(true));
    msgBox->setText(
    if(!strcmp(type_name,"Ladspa") && getenv("LADSPA_PATH")==NULL)
      msgBox->setInformativeText("(LADSPA_PATH is not set)");
    */
    EVENTLOG_add_event(strdup(message.toUtf8().constData()));

    /*
    msgBox->setDefaultButton(QMessageBox::Ok);
    safeExec(msgBox, false);
    */
  }

  return PR_get_plugin_type_by_name("Pipe","Pipe");
}

SoundPluginType *PR_get_plugin_type(int num){
  return g_plugin_types[num];
}

const QVector<PluginMenuEntry> PR_get_menu_entries(void){

  Favourites *favourites = get_favourites();
  
  QVector<PluginMenuEntry> ret(g_plugin_menu_entries);

  // Add favourite menu entries. (i.e. most used plugins)
  if (favourites->hepps.size() > 0) {
    ret.push_back(PluginMenuEntry::separator());
    
    bool has_next = false;
    
    int num_added = 0;
    
    for (const auto &hepp : favourites->hepps){
      
      if (hepp.type_name == "VST" ||
          hepp.type_name == "VST3" ||
          hepp.type_name == "AU" ||
          hepp.type_name == "Ladspa" ||
          hepp.type_name == "Pd" ||
          hepp.name.startsWith("STK ")
          )
        {
          if (hepp.name != "Calf MultiChorus LADSPA" &&
              hepp.name.trimmed() != "" // <-- TODO. Custom pd patches doesn't seem to have name.
              )
            {
              if (num_added == 10){
                ret.push_back(PluginMenuEntry::level_up("next"));
                has_next=true;
              }

              ret.push_back(PluginMenuEntry::num_used_plugin(hepp));
              num_added++;
            }
        }
      
    }
    
    if (has_next)
      ret.push_back(PluginMenuEntry::level_down());
  }

  return ret;
}

void PR_add_menu_entry(PluginMenuEntry entry){
  g_plugin_menu_entries.push_back(entry);
}

void PR_add_plugin_type_no_menu(SoundPluginType *type){
  if(type->name==NULL || type->type_name==NULL)
    RError("Can not be null. type_name: %s, name: %s\n",type->type_name,type->name);
  
  if(!strcmp(type->type_name,"Faust") && !strcmp(type->name,"Zita Reverb"))
    type->info = "Zita Reverb is a reverb designed by Fons Adriaensen.\n"
      "The Faust version is implemented by Julius O. Smith III.\n"
      "\n"
      "Julius O. Smith III writes: "
      "\"Internal 8x8 late-reverberation FDN used in the FOSS Linux reverb zita-rev1 "
      "by Fons Adriaensen <fons@linuxaudio.org>.  This is an FDN reverb with "
      "allpass comb filters in each feedback delay in addition to the "
      "damping filters.\"\n"
      "\n"
      "Note that both the original version of Zita Reverb, and the Faust version (which the Radium version is based on), "
      "have two additional equalizers plus a slider to adjust dry/wet. "
      "These controllers are not needed, and have been removed, since Radium provides this functionality for all audio instruments and audio effects.\n"
      ;

  if(!strcmp(type->type_name,"Faust") && !strcmp(type->name,"Multiband Compressor"))
    type->info =
      "Multiband compression makes the spectrum of a sound more audible. "
      "Multiband compression is normally performed as the last step in the mix of a piece, applied after the final equalization. "
      "\n\n"
      "This Multiband Compressor is assembled using various audio signal processing bricks implemented by Julius O. Smith III in the Faust language (http://faust.grame.fr). "
      "Homepage of Julius O. Smith III is https://ccrma.stanford.edu/~jos/"
      "\n\n"
      "First the separate bands are created using Butterworth band-splits. Then each band is sent through a standard compressor."
      "Finally, the three bands are mixed together and sent through a limiter."
      ;
      
      

  if(!strcmp(type->type_name,"Faust") && type->name[0]=='S' && type->name[1]=='T' && type->name[2]=='K')
    type->info = 
      "The FAUST Synthesis ToolKit is a set virtual musical instruments written in the FAUST "
      "programming language and based on waveguide algorithms and modal synthesis. Most of them "
      "were inspired by instruments implemented in the Synthesis ToolKit and the program SynthBuilder.\n"
      "\n"
      "The STK is developed since 1996 by P. R. Cook and G. P. Scavone. It is a set of open  "
      "source audio signal processing and algorithmic synthesis classes written in the C++ programming "
      "language that can be used in the development of music synthesis and audio processing "
      "software (https://ccrma.stanford.edu/software/stk/).\n"
      "\n"
      "Romain Michon";

  if(!strcmp(type->type_name,"Faust") && !strcmp(type->name,"Tapiir"))
    type->info = "The tapiir program is a multitap delay program. It has a stereo input and a stereo output. Each channel of the stereo signal is sent to 6 independant delays, not necessarily with the same gain for each delay. Then, each delay output is sent to the 5 other delay input, and also to its own input (to provide feedback possibilities), and all volumes can be changed independantly. Finally, each delay output, and each input of the tapiir is connected to both channels of the stereo output of the tapiir, still with configurable volumes. A wide range of effect can be constructed out of this complex system of delays (feedback, echos, filters). (This text is copied from http://faust.grame.fr)\n\nThe first Tapiir was made by Maarten de Boer's. This Faust version is implemented by the Faust team.";

  g_plugin_types.push_back(type);
}

void PR_add_plugin_type(SoundPluginType *type){
  PR_add_plugin_type_no_menu(type);

  if(!strcmp(type->type_name,"Faust")){
    if(!strcmp(type->name,"System Eq"))
      return;
    if(!strcmp(type->name,"System Lowpass"))
      return;
    if(!strcmp(type->name,"System Highpass"))
      return;
    if(!strcmp(type->name,"System Lowshelf"))
      return;
    if(!strcmp(type->name,"System Highshelf"))
      return;
    if(!strcmp(type->name,"System Delay"))
      return;
    if(!strcmp(type->name,"System Tremolo"))
      return;
  }

  if(!strcmp(type->type_name,"Bus"))
    return;

  PR_add_menu_entry(PluginMenuEntry::normal(type));
}

void PR_add_plugin_container(SoundPluginTypeContainer *container){
#if 0
  SoundPluginTypeContainer *existing = PR_get_container_by_name(container->name, container->type_name);
  
  if (existing != NULL){
    if (!STRING_equals2(container->filename, existing->filename))
      GFX_Message(NULL, "Warning: Two different plugin files with the same name has been added to the registry:<br><UL><LI>%s<LI>%s</UL>Only the first file will be used.",
                  STRING_get_chars(existing->filename),
                  STRING_get_chars(container->filename)
                  );
    container = existing;
  } else
#endif
    
    g_plugin_type_containers.push_back(container);
    
  PR_add_menu_entry(PluginMenuEntry::container(container));
}

//extern "C" void create_sine_plugin(void);
extern void create_bus_plugins(bool only_pipe);
extern void create_seqtrack_plugin(void);
extern void create_timeskew_plugin(void);
extern void create_patchbay_plugin(void);
#include "VST_plugins_proc.h"
extern void create_juce_plugins(void);
extern void create_ladspa_plugins(void);
extern void create_sample_plugin(void);
extern "C" void create_fluidsynth_plugin(void);
extern void create_pd_plugin(void);
extern void create_modulator_plugin(void);

extern "C" void create_midimessages_plugin(void);
  
extern void create_zita_rev_plugin(void);
extern void create_faust_tapiir_plugin(void);
extern void create_faust_multibandcomp_plugin(void);

#ifdef WITH_FAUST_DEV
extern void create_faust_plugin(void);
#endif

extern void create_faust_system_eq_plugin(void);
extern void create_faust_system_tremolo_plugin(void);
extern void create_faust_system_lowpass_plugin(void);
extern void create_faust_system_highpass_plugin(void);
extern void create_faust_system_lowshelf_plugin(void);
extern void create_faust_system_highshelf_plugin(void);
//extern void create_faust_system_delay_plugin(void);

extern void create_stk_bass_plugin(void);
extern void create_stk_bowed_plugin(void);
extern void create_stk_blow_bottle_plugin(void);
extern void create_stk_blow_hole_plugin(void);
extern void create_stk_brass_plugin(void);
extern void create_stk_clarinet_plugin(void);
extern void create_stk_flute_plugin(void);
extern void create_stk_flute_stk_plugin(void);
extern void create_stk_glass_harmonica_plugin(void);
extern void create_stk_harpsi_plugin(void);
extern void create_stk_modal_bar_plugin(void);
extern void create_stk_NLF_eks_plugin(void);
extern void create_stk_NLF_fm_plugin(void);
extern void create_stk_piano_plugin(void);
extern void create_stk_saxophony_plugin(void);
extern void create_stk_sitar_plugin(void);
extern void create_stk_tibetan_bowl_plugin(void);
extern void create_stk_tuned_bar_plugin(void);
extern void create_stk_uni_bar_plugin(void);
extern void create_stk_voice_form_plugin(void);


void PR_set_init_vst_first(void){
  SETTINGS_write_bool("init_vst_first", true);
}

void PR_set_init_ladspa_first(void){
  SETTINGS_write_bool("init_vst_first", false);
}

bool PR_is_initing_vst_first(void){
  return SETTINGS_read_bool("init_vst_first", false);
}



void PR_init_plugin_types(void){
  API_incSoundPluginRegistryGeneration();
  API_clearSoundPluginRegistryCache();
  
  g_plugin_type_containers.clear();
  g_plugin_types.clear();
  g_plugin_menu_entries.clear();

  PR_add_menu_entry(PluginMenuEntry::load_preset());
  
  PR_add_menu_entry(PluginMenuEntry::paste_preset());
  
  PR_add_menu_entry(PluginMenuEntry::separator());


  if (true || PR_is_initing_vst_first()){

    create_vst_plugins(true);

    PR_add_menu_entry(PluginMenuEntry::separator());
    
    create_ladspa_plugins();

  } else {

    create_ladspa_plugins();
    PR_add_menu_entry(PluginMenuEntry::separator());

    create_vst_plugins(true);  
  }

  /*
  PR_add_menu_entry(PluginMenuEntry::level_up("VST"));{
    create_vst_plugins(false);
  }PR_add_menu_entry(PluginMenuEntry::level_down());
  */
  
  PR_add_menu_entry(PluginMenuEntry::separator());
  
  PR_add_menu_entry(PluginMenuEntry::level_up("Physical Modelling"));
  {
    create_stk_bass_plugin();
    create_stk_blow_bottle_plugin();
    create_stk_bowed_plugin();
    create_stk_blow_hole_plugin();
    create_stk_brass_plugin();
    create_stk_clarinet_plugin();
    create_stk_flute_plugin();
    create_stk_flute_stk_plugin();
    create_stk_glass_harmonica_plugin();
    create_stk_harpsi_plugin();
    create_stk_modal_bar_plugin();
    create_stk_NLF_eks_plugin();
    create_stk_NLF_fm_plugin();
    create_stk_piano_plugin();
    create_stk_saxophony_plugin();
    create_stk_sitar_plugin();
    create_stk_tibetan_bowl_plugin();
    create_stk_tuned_bar_plugin();
    create_stk_uni_bar_plugin();
    create_stk_voice_form_plugin();
  }
  PR_add_menu_entry(PluginMenuEntry::level_down());


  
  PR_add_menu_entry(PluginMenuEntry::separator());

  create_bus_plugins(true);

  PR_add_menu_entry(PluginMenuEntry::level_up("Pipes, inputs and outputs"));
  create_jack_plugins();
  PR_add_menu_entry(PluginMenuEntry::separator());
  create_bus_plugins(false);
  
  PR_add_menu_entry(PluginMenuEntry::level_down());


  
  PR_add_menu_entry(PluginMenuEntry::separator());

  create_seqtrack_plugin();
  create_timeskew_plugin();
  create_patchbay_plugin();
  create_midimessages_plugin();
  create_modulator_plugin();
  PR_add_menu_entry(PluginMenuEntry::separator());

  //create_sine_plugin();
  create_sample_plugin();
  create_fluidsynth_plugin();
  create_pd_plugin();
  create_zita_rev_plugin();
  create_faust_tapiir_plugin();
  create_faust_multibandcomp_plugin();

#ifdef WITH_FAUST_DEV
  create_faust_plugin();
#endif
  
  create_faust_system_eq_plugin();
  create_faust_system_tremolo_plugin();
  create_faust_system_lowpass_plugin();
  create_faust_system_highpass_plugin();
  create_faust_system_lowshelf_plugin();
  create_faust_system_highshelf_plugin();
  //create_faust_system_delay_plugin();

  recreate_favourites(true);

  // Update cache.
  getSoundPluginRegistry(false);
}

