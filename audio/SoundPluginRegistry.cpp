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

#include <QMessageBox>
#include <QString>
#include <QFileDialog>

#include "../common/nsmtracker.h"
#include "../common/settings_proc.h"
#include "../common/vector_proc.h"
#include "../common/visual_proc.h"

#include "../Qt/helpers.h"

#include "SoundPlugin.h"
#include "Jack_plugin_proc.h"
#include "VST_plugins_proc.h"

#include "../mixergui/QM_MixerWidget.h"

#include "SoundPluginRegistry_proc.h"


static radium::Vector<SoundPluginType*> g_plugin_types;
static radium::Vector<SoundPluginTypeContainer*> g_plugin_type_containers;

int PR_get_num_plugin_types(void){
  return g_plugin_types.size();
}

static SoundPluginType *PR_get_plugin_type_by_name(const char *type_name, const char *plugin_name){
  return PR_get_plugin_type_by_name(NULL, type_name, plugin_name);
}

SoundPluginType *PR_get_plugin_type_by_name(const char *container_name, const char *type_name, const char *plugin_name){

  // Compatibility with older songs
  if (!strcmp(type_name, "Patchbay") && !strcmp(plugin_name, "Patchbay"))
    plugin_name = "Patchbay 8x8";
  
  for(SoundPluginType *plugin_type : g_plugin_types)
    if(!strcmp(plugin_type->type_name,type_name))
      if(!strcmp(plugin_type->name,plugin_name))
        return plugin_type;

  // check if the container needs to be populated.
  if (container_name != NULL)
    for(auto container : g_plugin_type_containers)
      if (!container->is_populated)
        if(!strcmp(container->type_name,type_name))
          if(!strcmp(container->name,container_name)){
            container->populate(container);
            return PR_get_plugin_type_by_name(container_name, type_name, plugin_name);
          }

  // Older songs didn't store vst container names (because there were no containers). Try to set container_name to plugin_name and try again.
  if(!strcmp(type_name,"VST") && container_name==NULL){
    return PR_get_plugin_type_by_name(plugin_name,type_name,plugin_name);
  }

  if(!strcmp(type_name,"VST")){
    while(true){
      vector_t v = {}; // c++ way of zero-initialization without getting missing-field-initializers warning.

      int select_plugin_file = VECTOR_push_back(&v, "Select plugin file");
      int use_different_plugin = VECTOR_push_back(&v, "Use different plugin");
      int replace_with_pipe = VECTOR_push_back(&v, "Replace with pipe");
      (void)replace_with_pipe; // default
      
      int ret = GFX_Message(&v, ("VST Plugin " + QString(plugin_name) + " not found.").toUtf8().constData());

      if (ret==select_plugin_file) {
        QString filename = QFileDialog::getOpenFileName(NULL,
                                                        plugin_name,
                                                        QString(),
                                                        QString(),
                                                        0,
                                                        useNativeFileRequesters() ? (QFileDialog::Option)0 : QFileDialog::DontUseNativeDialog
                                                        );
        QFileInfo info(filename);

        QString basename = info.fileName();
        basename.resize(basename.size()-strlen(VST_SUFFIX)-1);

        //        if(basename==QString(plugin_name) && info.exists()){
        VST_add_path(info.dir().path());
        PR_init_plugin_types();
        return PR_get_plugin_type_by_name(container_name, type_name, plugin_name);
        //}
      } else if (ret==use_different_plugin) {
        SoundPluginType *plugintype = MW_popup_plugin_type_selector();
        if (plugintype==NULL)
          break;
        else
          return plugintype;
      } else
        break;
    }

  }else if(!strcmp(type_name,"Pd") && strcmp(plugin_name, "")){ // type_name doesn't mean anything for already saved files. Without this excpetion, plugins would be replaced by pipes if a pd patch file was renamed.
    return PR_get_plugin_type_by_name("Pd", "");

  }else{

    QMessageBox msgBox;
    msgBox.setText("Plugin " + QString(type_name) + " / " + QString(plugin_name) + " not found. Replacing with a Pipe.");
    if(!strcmp(type_name,"Ladspa") && getenv("LADSPA_PATH")==NULL)
      msgBox.setInformativeText("(LADSPA_PATH is not set)");

    msgBox.setDefaultButton(QMessageBox::Ok);
    safeExec(msgBox);
  }

  return PR_get_plugin_type_by_name("Pipe","Pipe");
}

SoundPluginType *PR_get_plugin_type(int num){
  return g_plugin_types[num];
}

static QVector<PluginMenuEntry> g_plugin_menu_entries;


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
  hepp.num_uses = atoll(numstring.trimmed().toUtf8().constData());
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

  if (hepp.name.startsWith("STK "))
    hepp.menu_text = "STK:" + hepp.name.right(hepp.name.size()-3);
  else
    hepp.menu_text = hepp.type_name + ": " + hepp.name;
  
  hepp.menu_text += " (" + QString::number(hepp.num_uses) + ")";

  printf("hepp. %d: -%s- / -%s- / -%s- (%s)\n",hepp.num_uses, hepp.container_name.toUtf8().constData(), hepp.type_name.toUtf8().constData(), hepp.name.toUtf8().constData(),hepp.menu_text.toUtf8().constData());
  

  
  return true;
}

static bool compare_hepps(const NumUsedPluginEntry &h1, const NumUsedPluginEntry &h2){
  if (h1.num_uses > h2.num_uses)
    return true;
  else
    return false;
}
    
const QVector<PluginMenuEntry> PR_get_menu_entries(void){
  printf("start. PR_get_menu_entries called\n");
  QVector<PluginMenuEntry> ret(g_plugin_menu_entries);
  printf("end. PR_get_menu_entries called\n");
  
  vector_t *lines = SETTINGS_get_all_lines_starting_with("plugin_usage_");

  QList<NumUsedPluginEntry> hepps;
  
  VECTOR_FOR_EACH(const char *, line_c, lines){
    NumUsedPluginEntry hepp;
    if (get_hepp_from_config_line(line_c, hepp))
      hepps.push_back(hepp);
  }END_VECTOR_FOR_EACH;
  
  if (hepps.size() > 0) {
    ret.push_back(PluginMenuEntry::separator());
    
    qSort(hepps.begin(), hepps.end(), compare_hepps);
    
    bool has_next = false;
    
    int num_added = 0;
    
    for (auto hepp : hepps){
      
      if (hepp.type_name == "VST" ||
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

  g_plugin_types.add(type);
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
  g_plugin_type_containers.add(container);
  PR_add_menu_entry(PluginMenuEntry::container(container));
}

//extern "C" void create_sine_plugin(void);
extern void create_bus_plugins(void);
extern void create_timeskew_plugin(void);
extern void create_patchbay_plugin(void);
#include "VST_plugins_proc.h"
extern void create_juce_plugins(void);
extern void create_ladspa_plugins(void);
extern "C" void create_sample_plugin(void);
extern "C" void create_fluidsynth_plugin(void);
extern void create_pd_plugin(void);

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
  g_plugin_types.clear();
  g_plugin_menu_entries.clear();

  if (PR_is_initing_vst_first()){

    PR_add_menu_entry(PluginMenuEntry::level_up("VST"));{    
      create_vst_plugins(true);
    }PR_add_menu_entry(PluginMenuEntry::level_down());

    PR_add_menu_entry(PluginMenuEntry::separator());
    
    create_ladspa_plugins();

  } else {

    create_ladspa_plugins();
    PR_add_menu_entry(PluginMenuEntry::separator());

    PR_add_menu_entry(PluginMenuEntry::level_up("VST"));{    
      create_vst_plugins(true);
    }PR_add_menu_entry(PluginMenuEntry::level_down());
  
  }

  /*
  PR_add_menu_entry(PluginMenuEntry::level_up("VST"));{
    create_vst_plugins(false);
  }PR_add_menu_entry(PluginMenuEntry::level_down());
  */
  
  PR_add_menu_entry(PluginMenuEntry::separator());
  
  PR_add_menu_entry(PluginMenuEntry::load_preset());
  
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

  create_jack_plugins();

  PR_add_menu_entry(PluginMenuEntry::separator());

  create_bus_plugins();
  create_timeskew_plugin();
  create_patchbay_plugin();
  create_midimessages_plugin();
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
}

