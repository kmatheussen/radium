
extern LANGSPEC struct Patch *NewPatchCurrPos(int patchtype, void *patchdata, const char *name);


// old version. will be removed when not used
static struct Patch *PATCH_create(int instrumenttype, void *patchdata, const char *name){
  struct Patch *patch = create_new_patch(name);
  patch->is_usable = true;
  
  if(instrumenttype==MIDI_INSTRUMENT_TYPE){
    MIDI_InitPatch(patch, patchdata);
  }else if(instrumenttype==AUDIO_INSTRUMENT_TYPE){
    AUDIO_InitPatch(patch, patchdata);
  }else{
    fprintf(stderr,"Unkown instrumenttype %d\n",instrumenttype);
    abort();
  }
  
  VECTOR_push_back(&patch->instrument->patches,patch);

  return patch;
}

// old version. will be removed when not used
struct Patch *NewPatchCurrPos(int instrumenttype, void *patchdata, const char *name){
  return PATCH_create(instrumenttype, patchdata, name);
}

/*
struct Patch *NewPatchCurrPos_set_track(int instrumenttype, void *patchdata, const char *name){
  struct Tracker_Windows *window=NULL;
  struct WTracks *wtrack;
  struct WBlocks *wblock;

  wtrack=getWTrackFromNumA(
                           -1,
                           &window,
                           -1,
                           &wblock,
                           -1
                           );

  if(wtrack==NULL)
    return NULL;

  {
    struct Patch *patch=PATCH_create(instrumenttype, patchdata, name);

    {
      ADD_UNDO(Track_CurrPos(window));
      handle_fx_when_theres_a_new_patch_for_track(wtrack->track,wtrack->track->patch,patch);
      wtrack->track->patch = patch;
    }

#if !USE_OPENGL
    UpdateFXNodeLines(window,wblock,wtrack);
#endif
    window->must_redraw = true;

    return patch;
  }

}
*/



#if 0
void InstrumentWidget_replace(struct Patch *old_patch){
  if(AUDIO_is_permanent_patch(old_patch)==true){
    GFX_Message(NULL,"Can not replace this one");
    return;
  }

  SoundPluginType *plugin_type = MW_popup_plugin_selector(NULL, 200, 200, false, NULL);
  if (plugin_type==NULL)
    return;

  SoundPlugin *plugin = PLUGIN_create(plugin_type, NULL);
  hash_t *state = PLUGIN_get_state(plugin);

  PLUGIN_delete(plugin); // not ideally to temporarily create a plugin, but alternative seems quite horrific, unfortunately. (Note that this is not dangerous, we only briefly create a plugin in order to harvest a state from it.)
    
  replace(old_patch, state);
}
#endif

void InstrumentWidget_load_preset(struct Patch *old_patch){
  if(AUDIO_is_permanent_patch(old_patch)==true){
    GFX_Message(NULL,"Can not load preset on this one");
    return;
  }

  hash_t *state = load_preset_state();
  if (state==NULL)
    return;

  replace(old_patch, state);
}


// Use selectPatchForTrack instead, or select-track-instrument in scheme.
#if 0
void PATCH_select_patch_for_track(struct Tracker_Windows *window,struct WTracks *wtrack, bool use_popup){
  ReqType reqtype;

  vector_t *patches=get_all_patches();

  NInt num_patches=patches->num_elements;

  if(num_patches==0)
    return;

  if(use_popup==true)
    reqtype=NULL;
  else
    reqtype=GFX_OpenReq(window,70,(int)(num_patches+50),"Select Patch");

  vector_t v={0};
  int new_midi_instrument = VECTOR_push_back(&v,"<New MIDI Instrument>");
  int new_sample_player = VECTOR_push_back(&v,"<New Sample Player>");
  int new_fluid_synth = VECTOR_push_back(&v,"<New FluidSynth>");
#ifdef WITH_PD
  int new_pd_instrument = VECTOR_push_back(&v,"<New Pd Instrument>");
#else
  int new_pd_instrument = -1000;
#endif
  int new_audio_instrument = VECTOR_push_back(&v,"<New Audio Instrument>");
  int load_preset = VECTOR_push_back(&v,"<Load New Preset>");

  VECTOR_push_back(&v,"----------");

  VECTOR_push_back(&v,"[submenu start]Copy Audio Instrument");  
  int start_copy_patch = v.num_elements;
  int copy_patch_vector[patches->num_elements];
  {
    int i=0;
    VECTOR_FOR_EACH(struct Patch *patch,patches){
      if (patch->instrument==get_audio_instrument()) {
        if(AUDIO_is_permanent_patch(patch)==false){
          VECTOR_push_back(&v,talloc_format("%d. %s",iterator666,patch->name));
          copy_patch_vector[i++] = iterator666;
        }
      }
    }END_VECTOR_FOR_EACH;
  }
  VECTOR_push_back(&v,"[submenu end]");

  VECTOR_push_back(&v,"----------");
  
  int start_select_patch = v.num_elements;
  
  VECTOR_FOR_EACH(struct Patch *patch,patches){
    VECTOR_push_back(&v,talloc_format("%d. %s",iterator666,patch->name));
  }END_VECTOR_FOR_EACH;

  {
    struct Patch *patch = NULL;

    int selection=GFX_Menu(window,reqtype,"Select Patch",&v);

    if(selection>=0){

      Undo_Open();{

        ADD_UNDO(Track(window,window->wblock,wtrack,window->wblock->curr_realline));

        if(selection>=start_select_patch){
          patch=patches->elements[selection-start_select_patch];

        }else if(selection>=start_copy_patch){
          int elementnum = copy_patch_vector[selection-start_copy_patch];
          struct Patch *old_patch = patches->elements[elementnum];
          SoundPlugin *old_plugin = (SoundPlugin*)old_patch->patchdata;
          hash_t *old_state = PLUGIN_get_state(old_plugin);

          patch = InstrumentWidget_new_from_preset(old_state, NULL, -100000,-100000,true);

        }else if(selection==new_midi_instrument){
          patch = NewPatchCurrPos(MIDI_INSTRUMENT_TYPE, NULL, "Unnamed");
          GFX_PP_Update(patch);

        }else if(selection==new_sample_player){
          SoundPlugin *plugin = add_new_audio_instrument_widget(PR_get_plugin_type_by_name(NULL, "Sample Player","Sample Player"),-100000,-100000,true,NULL,MIXER_get_buses());
          if(plugin!=NULL)
            patch = (struct Patch*)plugin->patch;
            
        }else if(selection==new_fluid_synth){
          SoundPlugin *plugin = add_new_audio_instrument_widget(PR_get_plugin_type_by_name(NULL, "FluidSynth","FluidSynth"),-100000,-100000,true,NULL,MIXER_get_buses());
          if(plugin!=NULL)
            patch = (struct Patch*)plugin->patch;
            
        }else if(selection==new_pd_instrument){
          SoundPlugin *plugin = add_new_audio_instrument_widget(PR_get_plugin_type_by_name(NULL, "Pd","Simple Midi Synth"),-100000,-100000,true,NULL,MIXER_get_buses());
          if(plugin!=NULL)
            patch = (struct Patch*)plugin->patch;

        }else if(selection==new_audio_instrument){
          SoundPlugin *plugin = add_new_audio_instrument_widget(NULL,-100000,-100000,true,NULL,MIXER_get_buses());
          if(plugin!=NULL)
            patch = (struct Patch*)plugin->patch;

          printf("   PLUGIN: %p, patch: %p\n",plugin,patch);
          
        }else if(selection==load_preset){
          patch = InstrumentWidget_new_from_preset(NULL, NULL, -100000,-100000,true);

        }else
          printf("Unknown option\n");

        if(patch!=NULL){
          struct Tracks *track=wtrack->track;

          PLAYER_lock();{

            handle_fx_when_theres_a_new_patch_for_track(track,track->patch,patch);
            
            track->patch=patch;
            
          }PLAYER_unlock();

#if !USE_OPENGL
          UpdateFXNodeLines(window,window->wblock,wtrack);
#endif
          window->must_redraw = true;
              
          (*patch->instrument->PP_Update)(patch->instrument,patch);
        }

      }Undo_Close();

      if (patch==NULL)
        Undo_CancelLastUndo();
      
    } // if(selection>=0)

  }


  if(reqtype!=NULL)
    GFX_CloseReq(window,reqtype);
}
#endif


#if 0
void PLUGIN_set_from_patch(SoundPlugin *old_plugin, struct Patch *new_patch){
  struct Patch *old_patch = (struct Patch*)old_plugin->patch;
  R_ASSERT_RETURN_IF_FALSE(old_patch!=NULL);

  CHIP_set_pos(new_patch, CHIP_get_pos_x(old_patch), CHIP_get_pos_y(old_patch)); // Hack. MW_move_chip_to_slot (called from Chip::Chip) sometimes kicks the chip one or to slots to the left.
  
  hash_t *connections_state = MW_get_connections_state();
    
  PATCH_replace_patch_in_song(old_patch, new_patch);
  PATCH_delete(old_patch);
    
  MW_create_connections_from_state_and_replace_patch(connections_state, old_patch->id, new_patch->id);
}
#endif


extern LANGSPEC void PLUGIN_set_from_patch(SoundPlugin *old_plugin, struct Patch *new_patch);
extern LANGSPEC SoundPlugin *PLUGIN_set_from_state(SoundPlugin *old_plugin, hash_t *state); // warning, 'old_plugin' might be deleted. The return value must be used instead.


#if 0
SoundPlugin *PLUGIN_set_from_state(SoundPlugin *old_plugin, hash_t *state){

  R_ASSERT(Undo_Is_Open());
    
  volatile struct Patch *patch = old_plugin->patch;
  if (patch==NULL) {
    RError("patch not found for old plugin");
    return NULL;
  }

  bool can_replace_patch = true;
  
  if (HASH_has_key(state, "___radium_plugin_state_v3")==false)  // Before 3.0.rc15, loading/saving states in the instrument widgets only loaded/saved the effect values, not the complete plugin state.
    can_replace_patch = false;
  
  else if(AUDIO_is_permanent_patch((struct Patch*)patch)) {
    state = HASH_get_hash(state, "effects");
    R_ASSERT(state!=NULL);
    can_replace_patch = false;
  }
        

  
  if (can_replace_patch==false) { 
    for(int i=0;i<old_plugin->type->num_effects+NUM_SYSTEM_EFFECTS;i++)
      ADD_UNDO(AudioEffect_CurrPos((struct Patch*)patch, i));
    
    PLUGIN_set_effects_from_state(old_plugin, state);

    return old_plugin;
  }

  struct Patch *old_patch = (struct Patch*)old_plugin->patch;
  R_ASSERT_RETURN_IF_FALSE2(old_patch!=NULL, NULL);

  struct Patch *new_patch = InstrumentWidget_new_from_preset(state, old_patch->name, CHIP_get_pos_x(old_patch), CHIP_get_pos_y(old_patch), false);

  if (new_patch!=NULL) {
    
    PLUGIN_set_from_patch(old_plugin, new_patch);
    
    SoundPlugin *new_plugin = (SoundPlugin*)new_patch->patchdata;
    
    if (!old_patch->name_is_edited)
      new_patch->name = PLUGIN_generate_new_patchname(new_plugin->type);

    new_patch->name_is_edited = old_patch->name_is_edited;
    
    return new_plugin;
    
  } else {
    
    return NULL;
    
  }
}
#endif
 

extern LANGSPEC void AUDIO_InitPatch(struct Patch *patch, void *patchdata);

// Note that patchdata is NULL when called from disk_patches.c
// old version. will be deleted later
void AUDIO_InitPatch(struct Patch *patch, void *patchdata) {
  set_audio_patch_attributes(patch, patchdata);
}


#if 0
struct Patch *add_new_audio_instrument_widget2(struct SoundPluginType *plugin_type, double x, double y, const char *name){
  SoundPlugin *plugin = MW_add_plugin(plugin_type, x, y, MIXER_get_buses()); //<- todo: 1. rename MW_add_plugin2, and do undo things in there. Or maybe simplify add undo.
  if(plugin==NULL)
    return NULL;

  struct Patch *patch = NewPatchCurrPos(AUDIO_INSTRUMENT_TYPE, plugin, name==NULL ? PLUGIN_generate_new_patchname(plugin_type) : name);
  
  ADD_UNDO(Chip_Add_CurrPos(patch)); // Added afterwards. (can not add before when we don't know what to add!)
  
  plugin->patch = patch;
  
  create_audio_instrument_widget(patch);
  
  return patch;
}
#endif

#if 0
// * This is the entry point for creating audio instruments.
// * The entry point for delete any instrument is common/patch.c/PATCH_delete
//
// Old version, will be deleted when no longer used
SoundPlugin *add_new_audio_instrument_widget(struct SoundPluginType *plugin_type, double x, double y, bool autoconnect, const char *name, Buses buses){
  
  if(plugin_type==NULL) {
      struct Patch *created_patch_instead = NULL;
      
      plugin_type = MW_popup_plugin_selector(name, x, y, autoconnect, &created_patch_instead);

      if (plugin_type==NULL) {

        if (created_patch_instead != NULL)
          return (struct SoundPlugin*)created_patch_instead->patchdata;
        else
          return NULL;

      }
    }
    

    SoundPlugin *plugin;

    {

      struct Tracker_Windows *window = root->song->tracker_windows;
      struct WBlocks *wblock = window->wblock;
      struct WTracks *wtrack = wblock->wtrack;

      ADD_UNDO(Track(window,wblock,wtrack,wblock->curr_realline)); // why?
      ADD_UNDO(Patchlist_CurrPos()); // why?
      ADD_UNDO(InstrumentsWidget_CurrPos()); // Probably not any point anylonger.

      plugin = MW_add_plugin(plugin_type, x, y, buses);
      if(plugin==NULL)
        return NULL;

      struct Patch *patch = NewPatchCurrPos(AUDIO_INSTRUMENT_TYPE, plugin, name==NULL ? PLUGIN_generate_new_patchname(plugin_type) : name);

      ADD_UNDO(Chip_Add_CurrPos(patch)); // It works fine to call ADD_UNDO(Chip_Add right after the chip has been created (it's the only way to do it).

      //wtrack->track->patch = patch; // Bang!
      plugin->patch = patch;

      create_audio_instrument_widget(patch);

      if(autoconnect==true) {
        ADD_UNDO(MixerConnections_CurrPos());
        MW_autoconnect_plugin(plugin);
      }
    }

    return plugin;
}
#endif

void close_all_instrument_widgets(void){
  QStackedWidget* tabs = instruments_widget->tabs;

  while(tabs->count()>0){
    //if(dynamic_cast<No_instrument_widget*>(tabs->page(0)))
    //  tabs->removeTab(1);
    //else
    tabs->removeWidget(tabs->widget(0));
  }

  MW_update_all_chips();

  //ResetUndo();
}



#if 0
static hash_t *load_preset_state(void){
  
  QString filename = request_load_preset_filename();
  
  if(filename=="")
    return NULL;

  return get_preset_state_from_filename(filename);
}


static struct Patch *M_InstrumentWidget_new_from_preset(hash_t *state, const char *name, double x, double y){
  struct Patch *patch = CHIP_create_from_plugin_state(state, name, x, y, MIXER_get_buses());
  
  if (patch!=NULL){
    R_ASSERT(patch->patchdata != NULL);

    create_audio_instrument_widget(patch);
  }

  return patch;
}


struct Patch *InstrumentWidget_new_from_preset2(wchar_t *filename, const char *name, double x, double y){
  if (filename==NULL)
    return NULL;
  
  hash_t *state = get_preset_state_from_filename(STRING_get_qstring(filename));
  if (state == NULL)
    return NULL;

  if (name==NULL)
    name = talloc_strdup(last_filename.toUtf8().constData());
  
  struct Patch *patch = M_InstrumentWidget_new_from_preset(state, name, x, y);

  if (patch != NULL)
    ADD_UNDO(Chip_Add_CurrPos(patch)); // It works fine to call ADD_UNDO(Chip_Add right after the chip has been created. (except that it's not very logical)
  
  return patch;
}

// Old version, will be deleted when no longer used
struct Patch *InstrumentWidget_new_from_preset(hash_t *state, const char *name, double x, double y, bool autoconnect){
  if (state==NULL) {
    state = load_preset_state();
    if (state != NULL && name==NULL)
      name = talloc_strdup(last_filename.toUtf8().constData());      
  }
  
  if (state == NULL)
    return NULL;

  //ADD_UNDO(Track(window,wblock,wtrack,wblock->curr_realline));      
  ADD_UNDO(Patchlist_CurrPos());
  ADD_UNDO(InstrumentsWidget_CurrPos());
  ADD_UNDO(MixerConnections_CurrPos());
  
  struct Patch *patch = M_InstrumentWidget_new_from_preset(state, name, x, y);

  if (patch != NULL){
    if (autoconnect) {
      struct SoundPlugin *plugin = (SoundPlugin*)patch->patchdata;
      MW_autoconnect_plugin(plugin);
    }

    ADD_UNDO(Chip_Add_CurrPos(patch)); // It works fine to call ADD_UNDO(Chip_Add right after the chip has been created. (except that it's not very logical)
  }
  
  return patch;
}
#endif

#if 0
void InstrumentWidget_replace_old(struct Patch *old_patch){
  SoundPluginType *plugin_type = MW_popup_plugin_selector(NULL, 200, 200, false);
  if (plugin_type==NULL)
    return;

  struct Patch *new_patch = NULL;
  SoundPlugin *new_plugin = NULL;
  
  Undo_Open();{

    //SoundPlugin *new_plugin = PLUGIN_create(plugin_type, NULL);
    //SoundPlugin *new_plugin = MW_add_plugin(plugin_type, 0,0);
    new_plugin = add_new_audio_instrument_widget(plugin_type, 0, 0, false, NULL, MIXER_get_buses());
    if (new_plugin != NULL) {
  
      new_patch = NewPatchCurrPos(AUDIO_INSTRUMENT_TYPE, new_plugin, PLUGIN_generate_new_patchname(plugin_type));
      if (new_patch==NULL){
        RError("new_patch!=NULL");
      } else {
        SoundPlugin *old_plugin = (SoundPlugin*)old_patch->patchdata;
        PLUGIN_set_from_patch(old_plugin, new_patch);
      }
    }
    
  }Undo_Close();
        

  if (new_plugin==NULL)
    Undo_CancelLastUndo();
  else 
    GFX_update_instrument_widget(new_patch);
}

static void replace(struct Patch *old_patch, hash_t *state){
  SoundPlugin *old_plugin = (SoundPlugin*)old_patch->patchdata;
  SoundPlugin *new_plugin = NULL;
  
  Undo_Open();{
    new_plugin = PLUGIN_set_from_state(old_plugin, state);
  }Undo_Close();

  if (new_plugin==NULL)
    Undo_CancelLastUndo();
  else {
    volatile struct Patch *new_patch = new_plugin->patch;
    R_ASSERT_RETURN_IF_FALSE(new_patch!=NULL);

    GFX_update_instrument_widget((struct Patch*)new_patch);
  }
}
#endif


#if 0
// old version, will be deleted (
void InstrumentWidget_remove_patch(struct Patch *patch){
  QStackedWidget* tabs = instruments_widget->tabs;

  ADD_UNDO(InstrumentsWidget_CurrPos());

  MIDI_instrument_widget *w1 = get_midi_instrument_widget(patch);
  if(w1!=NULL){
    tabs->removeWidget(w1); // Undo is storing the tab widget, so we can't delete it.
    return;
  }

  Audio_instrument_widget *w2 = get_audio_instrument_widget(patch);
  if(w2==NULL){
    RError("No such patch widget: %p\n",patch);
    return;
  } else {
    w2->prepare_for_deletion(); // <- We can't delete the widget (for various spaghetti reasons), so we do this instead.
    tabs->removeWidget(w2);  // Undo is storing the tab widget, so we can't delete it.
    //delete w2;
  }

}


void GFX_remove_patch_gui(struct Patch *patch){
  InstrumentWidget_remove_patch(patch);

  if (patch->instrument==get_audio_instrument()) {
    
    SoundPlugin *plugin = (SoundPlugin*) patch->patchdata;
    MW_delete_plugin(plugin);

    MW_update_all_chips();
      
  }
}
#endif

void InstrumentWidget_update(struct Patch *patch){
  InstrumentWidget_remove_patch(patch);
  InstrumentWidget_create_audio_instrument_widget(patch);
o}

#if 0
// old version. will be deleted
void PATCH_delete(struct Patch *patch){

  R_ASSERT(Undo_Is_Open());

  if(patch->instrument==get_audio_instrument())
    if(AUDIO_is_permanent_patch(patch)==true){
      GFX_Message(NULL,"Can not be deleted");
      return;
    }

  remove_patch_from_song(patch);

  if(patch->instrument==get_audio_instrument()){
    ADD_UNDO(MixerConnections_CurrPos());
    ADD_UNDO(Chip_Remove_CurrPos(patch));
  }

  GFX_remove_patch_gui(patch);

  patch->instrument->remove_patch(patch);

  ADD_UNDO(Patchlist_CurrPos());
  VECTOR_remove(&patch->instrument->patches,patch);
}

void PATCH_delete_CurrPos(struct Patch *patch){

  Undo_Open();{
    
    PATCH_delete(patch);

  }Undo_Close();

  root->song->tracker_windows->must_redraw=true;
  //DrawUpTrackerWindow(root->song->tracker_windows);
}
#endif


SoundPlugin *MW_add_plugin(SoundPluginType *plugin_type, double x, double y, Buses buses);


#if 0
// MW_add_plugin/MW_delete_plugin are one of two entry points for audio plugins.
// Creating/deleting a plugin goes through here, not through audio/.
//
// The other entry point is CHIP_create_from_state, which is called from undo/redo and load.
//
SoundPlugin *MW_add_plugin(SoundPluginType *plugin_type, double x, double y, Buses buses){
  if (PLAYER_is_running()==false)
    return NULL;

  SoundPlugin *plugin = PLUGIN_create(plugin_type, NULL);
  if(plugin==NULL)
    return NULL;

  if(x<=-100000)
    MW_set_autopos(&x, &y);
    
  SoundProducer   *sound_producer = SP_create(plugin, buses);
  Chip *chip = new Chip(&g_mixer_widget->scene,sound_producer,x,y);

  MW_move_chip_to_slot(chip, x, y);
    
  return plugin;
}

extern LANGSPEC void MW_delete_plugin(SoundPlugin *plugin); // Deletes chip, plugin soundproducer and connections.


void MW_delete_plugin(SoundPlugin *plugin){
  QList<QGraphicsItem *> das_items = g_mixer_widget->scene.items();

  for (int i = 0; i < das_items.size(); ++i) {
    Chip *chip = dynamic_cast<Chip*>(das_items.at(i));
    if(chip!=NULL){
      SoundProducer *producer = chip->_sound_producer;
      if(SP_get_plugin(producer)==plugin){
        hash_t *state = PLUGIN_get_state(plugin);
        delete chip; // audio connections are deleted via ~Chip(). (Yes, it's somewhat messy)
        SP_delete(producer);
        volatile struct Patch *patch = plugin->patch;
        PLUGIN_delete(plugin);
        patch->patchdata = NULL; // Correct thing to do. A subtle bug in GFX_update_all_instrument_widgets prompted me to do add it (QT tabs are note updated right away). Somewhat messy this too.
        patch->state = state;
        patch->is_usable = false; // Make sure we don't use this patch if pasting it.
        return;
      }
    }
  }
}
#endif



#if 0
// Old version. Will be deleted as soon as it's not used anymore
SoundPluginType *MW_popup_plugin_selector(const char *name, double x, double y, bool autoconnect, struct Patch **created_patch_instead){
  QMenu menu(0);

  menu_up(&menu, PR_get_menu_entries(), 0);

  MyQAction *action;

  if (doModalWindows()) {
    
    GL_lock();{
      action = dynamic_cast<MyQAction*>(menu.exec(QCursor::pos()));
    }GL_unlock();
    
  } else {
    
    GL_lock();{
      GL_pause_gl_thread_a_short_while();
    }GL_unlock();    
    action = dynamic_cast<MyQAction*>(menu.exec(QCursor::pos()));
    
  }
  
  if (action==NULL)
    return NULL;

  struct PluginMenuEntry entry = action->entry;

  if (entry.type==PluginMenuEntry::IS_CONTAINER) {

    vector_t names={};

    SoundPluginTypeContainer *plugin_type_container = entry.plugin_type_container;
    plugin_type_container->populate(plugin_type_container);

    if (plugin_type_container->num_types==0) {
      //GFX_Message(NULL, talloc_format("%s does not contain any plugin",plugin_type_container->name));
      return NULL; // no error message here. populate() must do that.
    }
     
    if (plugin_type_container->num_types==1)
      return plugin_type_container->plugin_types[0];

    for(int i=0 ; i < plugin_type_container->num_types ;  i++)
      VECTOR_push_back(&names,plugin_type_container->plugin_types[i]->name);

    char temp[1024];
    sprintf(temp,"Select plugin contained in %s", plugin_type_container->name);
    int selection=GFX_Menu(NULL,NULL,temp,&names);
    
    if (selection==-1)
      return NULL;
    else
      return plugin_type_container->plugin_types[selection];

   }else if(entry.type==PluginMenuEntry::IS_LOAD_PRESET){
    
    struct Patch *patch = InstrumentWidget_new_from_preset(NULL, name, x, y, autoconnect);
    if (created_patch_instead!=NULL)
      *created_patch_instead = patch;
    
    return NULL;
    
  } else {

    return entry.plugin_type;

  }
}
#endif




#if 0
struct Patch *CHIP_create_from_plugin_state(hash_t *plugin_state, const char *name, double x, double y, Buses buses){
  struct SoundPlugin *plugin = PLUGIN_create_from_state(plugin_state);
  R_ASSERT_RETURN_IF_FALSE2(plugin!=NULL, NULL);

  if (name==NULL)
    name = PLUGIN_generate_new_patchname(plugin->type);

  struct Patch *patch = NewPatchCurrPos(AUDIO_INSTRUMENT_TYPE, plugin, name);
  patch->patchdata = plugin;
  plugin->patch = patch;
  
  SoundProducer   *sound_producer = SP_create(plugin, buses);
    
  if(x<=-100000)
    MW_set_autopos(&x, &y);    

  Chip *chip = new Chip(&g_mixer_widget->scene,sound_producer, x, y);
  printf("Made chip %p\n",chip);
  
  MW_move_chip_to_slot(chip, x, y); // unfortunately, this function very often moves the chip to the right unnecessarily.

  return patch;
}
#endif


//extern LANGSPEC void CHIP_delete_from_patch(struct Patch *patch);
extern LANGSPEC hash_t *CHIP_get_chip_state_from_patch(struct Patch *patch);


#if 0
// Will be removed. Currently used when loading.
void CHIP_create_from_state(hash_t *state, Buses buses){
  if (PLAYER_is_running()==false)
    return;

  struct Patch *patch = PATCH_get_from_id(HASH_get_int(state, "patch"));
  double x = HASH_get_float(state, "x");
  double y = HASH_get_float(state, "y");

  struct SoundPlugin *plugin = PLUGIN_create_from_state(HASH_get_hash(state, "plugin"));
  R_ASSERT(plugin != NULL);
  patch->patchdata = plugin;
  
  if(plugin!=NULL){
    patch->is_usable = true;
    plugin->patch = patch;
    Chip *chip = new Chip(&g_mixer_widget->scene,
                          SP_create(plugin,buses),
                          x,y);    
    printf("Made chip %p\n",chip);
  }else{
    patch->is_usable = false;
    printf("Unable to create chip\n"); // proper error message given elsewhere. (ladspa or vst)
  }
}
#endif


hash_t *CHIP_get_state(Chip *chip);


#if 0
// will be unnecessary
hash_t *CHIP_get_state(Chip *chip){
  hash_t *state=HASH_create(4);

  SoundPlugin *plugin = SP_get_plugin(chip->_sound_producer);
  struct Patch *patch = CHIP_get_patch(chip);

  HASH_put_int(state, "patch", patch->id);
  HASH_put_float(state, "x", chip->x());
  HASH_put_float(state, "y", chip->y());

  HASH_put_hash(state, "plugin", PLUGIN_get_state(plugin));

  return state;
}

hash_t *CHIP_get_chip_state_from_patch(struct Patch *patch){
  Chip *chip = get_chip_from_patch_id(&g_mixer_widget->scene, patch->id);
  return CHIP_get_state(chip);
}
#endif
