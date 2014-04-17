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



// A lot of the code in this file has been written by looking at (and copying from) qtractor (http://qtractor.sourceforge.net/),
// vsti (https://github.com/kmatheussen/vsti), and the vst tilde source (made by Mark Williamson).

// There's also some nice code to look at here:
// http://code.breakfastquay.com/projects/dssi-vst/repository/entry/dssi-vst-server.cpp


#if defined(FOR_MACOSX)
#  import <Carbon/Carbon.h>
#  undef EVENT_H
// 
#endif


#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <math.h>

#include <stdint.h>

#include <vector>


#if USE_VESTIGE

#warning
#warning
#warning "Using vestige headers instead of vst headers from Steinberg."
#warning
#warning

#  include "vestige/aeffectx.h"

typedef intptr_t VstIntPtr;

const int effIdle = 53;
const int effKeysRequired = 57;
const int effGetPlugCategory = 35;
const int kPlugCategSynth = 2;

const int effGetParamLabel = 6;
const int effGetParamDisplay = 7;
const int effGetChunk = 23;
const int effSetChunk = 24;
const int effFlagsProgramChunks = 32;

const int kVstMaxParamStrLen = 8;

#else //  USE_VESTIGE

// If this fails, and you don't bother downloading and installing vstsdk from steinberg, set USE_VESTIGE to 1 in the Makefile!

#  define VST_FORCE_DEPRECATED 0
#  include <vstsdk2.4/pluginterfaces/vst2.x/aeffectx.h>

#endif //  USE_VESTIGE

#include <QApplication>
#include <QTime>
#include <QMessageBox>
#include <QBoxLayout>

#include <QWidget>
#include <QLibrary>
#include <QShowEvent>
#include <QDirIterator>
#include <QString>

#include <QSlider>

#include "../common/nsmtracker.h"
#include "../common/visual_proc.h"
#include "../common/OS_visual_input.h"
#include "../common/settings_proc.h"
#include "../common/playerclass.h"


#include "SoundPlugin.h"
#include "SoundPlugin_proc.h"

#include "SoundPluginRegistry_proc.h"

#include "../audio/VST_plugins_proc.h"


#if defined(Q_WS_X11)
#include <X11/Xlib.h>
#include <QX11Info>
#include <X11/Xatom.h>
typedef void (*XEventProc)(XEvent *);
#endif


extern PlayerClass *pc;


extern "C"{
  typedef AEffect* (*VST_GetPluginInstance) (audioMasterCallback);
}

#define MAX_EVENTS 512

namespace{ // Use namespace since we already have a widget called EditorWidget. (stupid c++, doesn't even give a warning)

  class EditorWidget;
  struct Data{
    AEffect *aeffect;
    EditorWidget *editor_widget;
    float sample_rate;
    VstEvents *events;
    VstMidiEvent midi_events[MAX_EVENTS];
  };
  
  struct TypeDataParam{
    char label[kVstMaxParamStrLen + 100]; // Add some extra space. At least some mda plugins (ePiano for instance) uses more.
    char name[kVstMaxParamStrLen + 100]; // Same here.
    float default_value;
  };
  struct TypeData{
    VST_GetPluginInstance get_plugin_instance;
    TypeDataParam *params;
  };


#ifdef __linux__

// The rest of the code inside this namespace is first copied from the file src/qtracktorVstPlugin.c in QTractor (http://qtractor.sourceforge.net/)
// (written by Rui Nuno Capela), and then modified.


static bool g_bXError = false;

static int tempXErrorHandler ( Display *, XErrorEvent * )
{
	g_bXError = true;
	return 0;
}

static XEventProc getXEventProc ( Display *pDisplay, Window w )
{
	int iSize;
	unsigned long iBytes, iCount;
	unsigned char *pData;
	XEventProc eventProc = NULL;
	Atom aType, aName = XInternAtom(pDisplay, "_XEventProc", false);

	g_bXError = false;
	XErrorHandler oldErrorHandler = XSetErrorHandler(tempXErrorHandler);
	XGetWindowProperty(pDisplay, w, aName, 0, 1, false,
		AnyPropertyType, &aType,  &iSize, &iCount, &iBytes, &pData);
	if (g_bXError == false && iCount == 1)
		eventProc = (XEventProc) (pData);
	XSetErrorHandler(oldErrorHandler);

	return eventProc;
}

static Window getXChildWindow ( Display *pDisplay, Window w )
{
	Window wRoot, wParent, *pwChildren;
	unsigned int iChildren = 0;

	XQueryTree(pDisplay, w, &wRoot, &wParent, &pwChildren, &iChildren);

	return (iChildren > 0 ? pwChildren[0] : 0);
}

#endif // __linux__


class EditorWidget;

// Dynamic singleton list of VST editors.
static QList<EditorWidget *> g_vstEditors;

class EditorWidget : public QWidget
{
public:

  bool _is_open;

	// Constructor.
	EditorWidget(QWidget *pParent, Qt::WindowFlags wflags = 0)
	  : QWidget(pParent, wflags),
                  _is_open(false),
	#if defined(Q_WS_X11)
		m_pDisplay(QX11Info::display()),
		m_wVstEditor(0),
		m_pVstEventProc(NULL),
		m_bButtonPress(false),
	#endif
		_effect(NULL) {}

	// Destructor.
	~EditorWidget() { close(); }

	// Specialized editor methods.
          void open(AEffect *effect)
	{
                _effect = effect;
		
		// Make it the right size
		struct ERect {
			short top;
			short left;
			short bottom;
			short right;
		} *pRect;

                if (effect->dispatcher(effect, effEditGetRect, 0, 0, &pRect, 0.0f)) {
#if !defined(FOR_MACOSX)
			int w = pRect->right - pRect->left;
			int h = pRect->bottom - pRect->top;
			if (w > 0 && h > 0)
				QWidget::setFixedSize(w, h);
#endif
		}

#if defined(FOR_WINDOWS)
		void *ptr = (void *) winId();
                _effect->dispatcher(_effect, effEditOpen, 0, 0, ptr, 0.0f);
#endif

#if defined(FOR_MACOSX)

		Rect contentRect = {pRect->top+100, pRect->left+100, pRect->bottom+100, pRect->right+100};
		//SetRect(&contentRect, 200, 200, 400, 400);
		HIWindowRef windowRef;
		CreateNewWindow(kDocumentWindowClass, kWindowStandardFloatingAttributes | kWindowCompositingAttribute | kWindowStandardHandlerAttribute, &contentRect, &windowRef);
		HIViewRef contentView = 0;
		GetRootControl(windowRef, &contentView);
		
		_effect->dispatcher(_effect,effEditOpen, 0, 0, (void*)windowRef, 0.0f);
		
		ShowWindow(windowRef);
#endif // defined(FOR_MACOSX)


#if defined(Q_WS_X11)
		void *ptr = (void *) winId();
                long value = (long) m_pDisplay;
                _effect->dispatcher(_effect, effEditOpen, 0, value, ptr, 0.0f);
		m_wVstEditor = getXChildWindow(m_pDisplay, (Window) winId());
		if (m_wVstEditor)
			m_pVstEventProc = getXEventProc(m_pDisplay, m_wVstEditor);
#endif

		g_vstEditors.append(this);

#if 0
		// Final stabilization...
		m_pVstPlugin->updateEditorTitle();
		m_pVstPlugin->setEditorVisible(true);
		m_pVstPlugin->idleEditor();
#endif

               // this is the same as m_pVstPlugin->idleEditor()
               _effect->dispatcher(_effect, effEditIdle, 0, 0, NULL, 0.0f);
#if !defined(FOR_MACOSX)
                show();
                update();
#endif

                _is_open=true;

	}

	// Close the editor widget.
	void close()
	{
                printf("Close()\n");

#if !defined(FOR_MACOSX)
		QWidget::close();
#endif
		if (_effect && _is_open) {
                        _effect->dispatcher(_effect,effEditClose, 0, 0, NULL, 0.0f);
		//	m_pVstPlugin->setEditorVisible(false);
			_effect = NULL;
		}

		int iIndex = g_vstEditors.indexOf(this);
		if (iIndex >= 0)
			g_vstEditors.removeAt(iIndex);

                _is_open=false;
	}

#if 0
  bool macEvent ( EventHandlerCallRef caller, EventRef event ){
    printf("got mac event\n");
    return false;
  }
#endif

#if defined(Q_WS_X11)

	// Local X11 event filter.
	bool x11EventFilter(XEvent *pEvent)
	{
		if (m_pVstEventProc && pEvent->xany.window == m_wVstEditor) {
			// Avoid mouse tracking events...
			switch (pEvent->xany.type) {
			case ButtonPress:
				m_bButtonPress = true;
				break;
			case ButtonRelease:
				m_bButtonPress = false;
				break;
			case MotionNotify:
				if (!m_bButtonPress)
					return false;
				// Fall thru...
			default:
				break;
			}
			// Process as intended...
			(*m_pVstEventProc)(pEvent);
			return true;
		} else {
			return false;
		}
	}

#endif

  /*
	qtractorVstPlugin *plugin() const
		{ return m_pVstPlugin; }
  */

protected:
#if !defined(FOR_MACOSX)

	// Visibility event handlers.
	void showEvent(QShowEvent *pShowEvent)
	{
		QWidget::showEvent(pShowEvent);
#if 0
		if (m_pVstPlugin && m_pVstPlugin->isFormVisible())
			(m_pVstPlugin->form())->toggleEditor(true);
#endif
	}

	void closeEvent(QCloseEvent *pCloseEvent)
	{
          printf("CloseEvent()\n");
#if 0
		if (m_pVstPlugin && m_pVstPlugin->isFormVisible())
			(m_pVstPlugin->form())->toggleEditor(false);
#endif
		QWidget::closeEvent(pCloseEvent);
  _is_open=false;

	}

	void moveEvent(QMoveEvent *pMoveEvent)
	{
		QWidget::moveEvent(pMoveEvent);
	#if defined(Q_WS_X11)
		if (m_wVstEditor) {
			XMoveWindow(m_pDisplay, m_wVstEditor, 0, 0);
		//	QWidget::update();
		}
	#endif
	}
#endif // !defined(FOR_MACOSX)

private:

	// Instance variables...
#if defined(Q_WS_X11)
	Display   *m_pDisplay;
	Window     m_wVstEditor;
	XEventProc m_pVstEventProc;
	bool       m_bButtonPress;
#endif

        AEffect *_effect;
};

} // namespace vst



extern "C" {


typedef AEffect* (*VST_GetPluginInstance) (audioMasterCallback);



/*
   (Copied from vstserver)

   The following audioMaster opcode handlings are copied (with a light modification)
   from the vst tilde source made by Mark Williamson.
*/

VstIntPtr VSTS_audioMaster(AEffect* effect,
                      int32_t opcode,
                      int32_t index,
                      VstIntPtr value,
                      void* ptr,
                      float opt)
{
#if 1 // If vst_tilde audioMaster (else plugin_tilde audioMaster)
	// Support opcodes
  switch(opcode){
  case audioMasterAutomate:			
    return 0;		// index, value, returns 0
    
  case audioMasterVersion:			
    return 9;		// vst version, currently 7 (0 for older)
    
  case audioMasterCurrentId:			
    return 0x951432;	// returns the unique id of a plug that's currently loading
    
  case audioMasterIdle:
    effect->dispatcher(effect, effEditIdle, 0, 0, NULL, 0.0f);
    return 0;		// call application idle routine (this will call effEditIdle for all open editors too) 

#if 0    
  case audioMasterPinConnected:	
    return false;	// inquire if an input or output is beeing connected;
#endif

#if 0
  case audioMasterWantMidi:			
    return 0;
#endif

  case audioMasterProcessEvents:		
    return 0; 	// Support of vst events to host is not available
    
  case audioMasterGetTime:
#if 0
    fprintf(stderr,"VST master dispatcher: audioMasterGetTime\n");
    break;
#else
    {
      SoundPlugin *plugin = (SoundPlugin*)effect->user;
      Data *data = (Data*)plugin->data;

      static VstTimeInfo _timeInfo;

      memset(&_timeInfo, 0, sizeof(_timeInfo));
      //printf("starttime: %d\n",(int)pc->start_time);
      //_timeInfo.ppqPos = pc->start_time / 24000;
      _timeInfo.samplePos = pc->start_time;
      _timeInfo.sampleRate = data->sample_rate;


      _timeInfo.timeSigNumerator = 4;
      _timeInfo.timeSigDenominator = 4;

      _timeInfo.tempo = 160;

      //_timeInfo.smpteOffset = pc->start_time / 1000;
      

      _timeInfo.flags = kVstCyclePosValid;
      //_timeInfo.flags = kVstCyclePosValid | kVstBarsValid | kVstPpqPosValid | kVstTimeSigValid | kVstSmpteValid;
      
      return (long)&_timeInfo;
    }
#endif		
    
#if 0
  case audioMasterTempoAt:			
    return 0;
#endif

  case audioMasterNeedIdle:	
    effect->dispatcher(effect, effIdle, 0, 0, NULL, 0.0f);
    return 1;
    
  case audioMasterGetSampleRate:		
    if(effect!=NULL){
      SoundPlugin *plugin = (SoundPlugin*)effect->user;
      Data *data = (Data*)plugin->data;
      return data->sample_rate;	
    }else
      return 44100.0f;
    
  case audioMasterGetVendorString:	// Just fooling version string
    strcpy((char*)ptr,"Radium");
    return 0;
    
  case audioMasterGetVendorVersion:	
    return 5000;	// HOST version 5000
    
  case audioMasterGetProductString:	// Just fooling product string
    strcpy((char*)ptr,"A mighty crack from the past.");
    return 0;
    
  case audioMasterVendorSpecific:		
    {
      return 0;
    }
    
    
  case audioMasterGetLanguage:		
    return kVstLangEnglish;
    
  case audioMasterUpdateDisplay:
    fprintf(stderr,"audioMasterUpdateDisplay\n");
    effect->dispatcher(effect, effEditIdle, 0, 0, NULL, 0.0f);
    return 0;
    
  case 	audioMasterSetTime:
    fprintf(stderr,"VST master dispatcher: Set Time\n");
    break;
  case 	audioMasterGetNumAutomatableParameters:
    fprintf(stderr,"VST master dispatcher: GetNumAutPar\n");
    break;
  case 	audioMasterGetParameterQuantization:
    fprintf(stderr,"VST master dispatcher: ParamQuant\n");
    break;
  case 	audioMasterIOChanged:		
    fprintf(stderr,"VST master dispatcher: IOchanged\n");
    break;
  case 	audioMasterSizeWindow:		
    fprintf(stderr,"VST master dispatcher: Size Window\n");
    break;
  case 	audioMasterGetBlockSize:	
    fprintf(stderr,"VST master dispatcher: GetBlockSize\n");
    break;
  case 	audioMasterGetInputLatency:	
    fprintf(stderr,"VST master dispatcher: GetInLatency\n");
    break;
  case 	audioMasterGetOutputLatency:	
    fprintf(stderr,"VST master dispatcher: GetOutLatency\n");
    break;
  case 	audioMasterGetPreviousPlug:	
    fprintf(stderr,"VST master dispatcher: PrevPlug\n");
    break;
  case 	audioMasterGetNextPlug:		
    fprintf(stderr,"VST master dispatcher: NextPlug\n");
    break;
  case 	audioMasterWillReplaceOrAccumulate:	
    fprintf(stderr,"VST master dispatcher: WillReplace"); 
    break;
  case 	audioMasterGetCurrentProcessLevel:	
    return 0;
    break;
  case 	audioMasterGetAutomationState:		
    fprintf(stderr,"VST master dispatcher: GetAutState\n");
    break;
  case 	audioMasterOfflineStart:	
    fprintf(stderr,"VST master dispatcher: Offlinestart\n");
    break;
  case 	audioMasterOfflineRead:		
    fprintf(stderr,"VST master dispatcher: Offlineread\n");
    break;
  case 	audioMasterOfflineWrite:	
    fprintf(stderr,"VST master dispatcher: Offlinewrite\n");
    break;
  case 	audioMasterOfflineGetCurrentPass:	
    fprintf(stderr,"VST master dispatcher: OfflineGetcurrentpass\n");
    break;
  case 	audioMasterOfflineGetCurrentMetaPass:	
    fprintf(stderr,"VST master dispatcher: GetGetCurrentMetapass\n");
    break;
  case 	audioMasterSetOutputSampleRate:		
    fprintf(stderr,"VST master dispatcher: Setsamplerate\n");
    break;
#if 0
  case 	audioMasterGetSpeakerArrangement:	
    fprintf(stderr,"VST master dispatcher: Getspeaker\n");
    break;
#endif
  case 	audioMasterSetIcon:			
    fprintf(stderr,"VST master dispatcher: seticon\n");
    break;
  case 	audioMasterCanDo:	
    fprintf(stderr,"VST master dispatcher: Can Do\n");
    break;
  case 	audioMasterOpenWindow:		
    fprintf(stderr,"VST master dispatcher: OpenWindow\n");
    break;
  case	audioMasterCloseWindow:		
    fprintf(stderr,"VST master dispatcher: CloseWindow\n");
    break;
  case	audioMasterGetDirectory:	
    fprintf(stderr,"VST master dispatcher: GetDirectory\n");
    break;
    
  case audioMasterEndEdit:
    fprintf(stderr,"VST master dispatcher: Finished editing\n");
    break;

  default:
    fprintf(stderr,"VST master dispatcher: undefed: %d , %d\n",(int)opcode , effKeysRequired );
    break;
  }	
  
  
  return 0;


#else
    char param_name[9];
#if 0 /*PLUGIN_TILDE_DEBUG*/
    /* The following procedure is copied (with a light modification) from the plugin tilde source
       made by Jarno Seppänen. I dont know what it does, but it needs
       to be here. (Got to read that VST documentation soon. :)
    */



    fprintf(stderr,"DEBUG plugin~_vst: audioMaster(0x%p, %ld, %ld, %ld, 0x%p, %f)",
	   effect, opcode, index, value, ptr, opt);
#endif

    switch (opcode) {
	case audioMasterAutomate:
#if 0
	    if(effect==NULL){
	      fprintf(stderr,"effect==NULL\n");
	    }
	    if(effect->user==NULL){
	      fprintf(stderr,"effect->user==NULL\n");
	    }
#endif
	    effect->setParameter (effect, index, opt);
	    /* Send "control" messages from here */
	    memset (param_name, 0, 9);
	    effect->dispatcher (effect, effGetParamName, index, 0, param_name, 0);
	    //	  plugin_tilde_emit_control_output (effect->user, param_name, opt);
	    return 0;
	    break;
	case audioMasterVersion:
	    return 1;
	    break;
	case audioMasterCurrentId:
	    return 0;
	    break;
	case audioMasterIdle:
	  // This is a different thread, isnt it? The winwin thread should provide enough idle cycles.
	  //	    effect->dispatcher (effect, effEditIdle, 0, 0, NULL, 0);
	    return 0;
	    break;
	case audioMasterPinConnected:
	    /* return 0="true" for all inquiries for now */
	    return 0;
	    break;
    }
#if 0 /*PLUGIN_TILDE_DEBUG*/
    fprintf(stderr,"DEBUG plugin~_vst: warning: unsupported audioMaster opcode");
#endif
    return 0;
#endif
}


  void buffer_size_is_changed(struct SoundPlugin *plugin, int new_buffer_size){
    Data *data = (Data*)plugin->data;
    AEffect *aeffect = data->aeffect;

    //fprintf(stderr,"Setting new buffer size for vst plugin: %d\n",new_buffer_size);

    aeffect->dispatcher(aeffect, effMainsChanged, 0, 0, NULL, 0.0f);
    aeffect->dispatcher(aeffect,
                        effSetBlockSize,
                        0, new_buffer_size, NULL, 0);
    aeffect->dispatcher(aeffect, effMainsChanged, 0, 1, NULL, 0.0f);
  }

static void RT_process(SoundPlugin *plugin, int64_t time, int num_frames, float **inputs, float **outputs){
  const SoundPluginType *type = plugin->type;
  Data *data = (Data*)plugin->data;
  AEffect *aeffect = data->aeffect;

  // TODO: Check if it is soundly safe to provide same input and output arrays to vst plugins.
  // (didn't find it out, so using different arrays, to be safe)

  float buf[type->num_outputs][num_frames];
  float *out[type->num_outputs];
  for(int ch=0;ch<type->num_outputs;ch++){
    if(ch<type->num_inputs && inputs[ch]==outputs[ch])
      out[ch] = buf[ch];
    else
      out[ch] = outputs[ch];
  }

  aeffect->dispatcher(aeffect,effProcessEvents,0,0,data->events,0.0f);
  data->events->numEvents=0;

  if(aeffect->flags & effFlagsCanReplacing) // && aeffect->processReplacing!=aeffect->process && aeffect->processReplacing!=NULL)
    aeffect->processReplacing(aeffect, inputs, out, num_frames);
  else{
    for(int ch=0;ch<type->num_outputs;ch++)
      memset(out[ch],0,num_frames*sizeof(float));

    aeffect->process(aeffect,inputs,out,num_frames);
  }

  for(int ch=0;ch<type->num_outputs;ch++){
    if(out[ch]==buf[ch])
      memcpy(outputs[ch],out[ch],num_frames*sizeof(float));
  }
}

#if 0
// Function copied from vsti (https://github.com/kmatheussen/vsti)
// (wrong way to do it, seems like I didn't know what I was doing when I wrote that code)
static void sendmidi(AEffect *effect,int val1, int val2, int val3){
  struct VstMidiEvent das_event;
  struct VstMidiEvent *pevent=&das_event;

  struct VstEvents events;

  //  printf("note: %d\n",note);

  pevent->type = kVstMidiType;
  pevent->byteSize = 24;
  pevent->deltaFrames = 0;
  pevent->flags = 0;
  pevent->detune = 0;
  pevent->noteLength = 0;
  pevent->noteOffset = 0;
  pevent->reserved1 = 0;
  pevent->reserved2 = 0;
  pevent->noteOffVelocity = 0;
  pevent->midiData[0] = val1;
  pevent->midiData[1] = val2;
  pevent->midiData[2] = val3;
  pevent->midiData[3] = 0;

  events.numEvents = 1;
  events.reserved  = 0;
  events.events[0]=(VstEvent*)pevent;

  //printf("Sending: %x %x %x\n",val1,val2,val3);
  
  effect->dispatcher(
  		     effect,
  		     effProcessEvents, 0, 0, &events, 0.0f
  		     );
}
#endif

static void add_midi_event(struct SoundPlugin *plugin,int time,int val1, int val2, int val3){
  Data *data = (Data*)plugin->data;
  VstEvents *events=data->events;
  
  if(events->numEvents==MAX_EVENTS){
    fprintf(stderr,"Error, too many vst midi events at once. Skipping.\n");
    goto exit;
  }

  {
    VstMidiEvent *pevent=(VstMidiEvent*)events->events[events->numEvents];
    events->numEvents++;
    
    //  printf("note: %d\n",note);
    
    pevent->type = kVstMidiType;
    pevent->byteSize = 24;
    pevent->deltaFrames = time;
    pevent->flags = 0;
    pevent->detune = 0;
    pevent->noteLength = 0;
    pevent->noteOffset = 0;
    pevent->reserved1 = 0;
    pevent->reserved2 = 0;
    pevent->noteOffVelocity = 0;
    pevent->midiData[0] = val1;
    pevent->midiData[1] = val2;
    pevent->midiData[2] = val3;
    pevent->midiData[3] = 0;
  }
  
 exit:
  return;
}


  static void play_note(struct SoundPlugin *plugin, int64_t time, int note_num, float volume,float pan){
    //printf("****************** play note at %d\n",(int)time);
    add_midi_event(plugin,time,0x9a,note_num,volume*127);
  }
  
  static void set_note_volume(struct SoundPlugin *plugin, int64_t time, int note_num, float volume){
    add_midi_event(plugin,time,0xaa,note_num,volume*127);
  }

  static void stop_note(struct SoundPlugin *plugin, int64_t time, int note_num, float volume){
    add_midi_event(plugin,time,0x90,note_num,volume*127);
  }

  float get_effect_value(struct SoundPlugin *plugin, int effect_num, enum ValueFormat value_format){
    Data *data = (Data*)plugin->data;
    AEffect *aeffect = data->aeffect;
    return aeffect->getParameter(aeffect,effect_num);
  }

  static void set_effect_value(struct SoundPlugin *plugin, int64_t time, int effect_num, float value, enum ValueFormat value_format){
    Data *data = (Data*)plugin->data;
    AEffect *aeffect = data->aeffect;
    aeffect->setParameter(aeffect,effect_num,value);
  }

  static void show_gui(struct SoundPlugin *plugin){
    Data *data = (Data*)plugin->data;
    AEffect *aeffect = data->aeffect;
    data->editor_widget->open(aeffect);
  }

  static void hide_gui(struct SoundPlugin *plugin){
    Data *data = (Data*)plugin->data;
    //AEffect *aeffect = data->aeffect;
    data->editor_widget->close();
  }

  static void get_display_value_string(SoundPlugin *plugin, int effect_num, char *buffer, int buffersize){
    TypeData *type_data = (TypeData*)plugin->type->data;
    Data *data = (Data*)plugin->data;
    AEffect *aeffect = data->aeffect;

    char disp[128] = {0};
    aeffect->dispatcher(aeffect,effGetParamDisplay,
			effect_num, 0, (void *) disp, 0.0f);

    if (disp[0]==0){
      snprintf(buffer,buffersize,"%f%s",aeffect->getParameter(aeffect,effect_num),type_data->params[effect_num].label);
    }else{
      const char *label = type_data->params[effect_num].label;
      snprintf(buffer,buffersize,"%s%s",disp,label==NULL ? "" : label);
    }
  }

  static const char *get_effect_name(struct SoundPlugin *plugin, int effect_num){
    const struct SoundPluginType *plugin_type = plugin->type;
    TypeData *type_data = (TypeData*)plugin_type->data;
    printf("type_data: %p, num: %d, Effect name: \"%s\"\n",type_data, effect_num,type_data->params[effect_num].label);
    return type_data->params[effect_num].name;
  }
  static const char *get_effect_description(const struct SoundPluginType *plugin_type, int effect_num){
    TypeData *type_data = (TypeData*)plugin_type->data;
    return type_data->params[effect_num].name;
  }



static void set_plugin_type_data(AEffect *aeffect, SoundPluginType *plugin_type){
  TypeData *type_data = (struct TypeData*)plugin_type->data;

  {
    char vendor[1024] = {0};
    aeffect->dispatcher(aeffect, effGetVendorString, 0, 0, vendor, 0.0f);
    char product[1024] = {0};
    aeffect->dispatcher(aeffect, effGetProductString, 0, 0, product, 0.0f);
    
    if(strlen(vendor)>0 || strlen(product)>0)
      plugin_type->info = strdup(QString("Vendor: "+QString(vendor)+".\nProduct: "+QString(product)).ascii());
  }

  plugin_type->num_effects = aeffect->numParams;

  plugin_type->plugin_takes_care_of_savable_values = true;
    
  plugin_type->num_inputs = aeffect->numInputs;
  plugin_type->num_outputs = aeffect->numOutputs;
    
  int category = aeffect->dispatcher(aeffect, effGetPlugCategory, 0, 0, NULL, 0.0f);
  plugin_type->is_instrument = category==kPlugCategSynth;

  TypeDataParam *params = (TypeDataParam*)calloc(sizeof(TypeDataParam),plugin_type->num_effects);    
  type_data->params = params;

  for(int i=0;i<aeffect->numParams;i++){

    if(params[i].label[0]==0)
      aeffect->dispatcher(aeffect,effGetParamLabel,i, 0, (void *) params[i].label, 0.0f);

    aeffect->dispatcher(aeffect,effGetParamName,i, 0, (void *) params[i].name, 0.0f);

    if(params[i].name[0]==0)
      sprintf(params[i].name,"%s",params[i].label);

    if(params[i].name[0]==0)
      sprintf(params[i].name,"<no name>");

    params[i].default_value = aeffect->getParameter(aeffect,i);
    
    printf("type_data: %p, i: %d. Effect name: \"%s\". label: \"%s\". default value: %f\n",type_data,i,params[i].name,params[i].label,params[i].default_value);
  }
}

static int num_running_plugins = 0;

static void *create_plugin_data(const SoundPluginType *plugin_type, SoundPlugin *plugin, hash_t *state, float sample_rate, int block_size){ // Returns plugin->data.
  TypeData *type_data = (TypeData*)plugin_type->data;

#if FULL_VERSION==0
  if(num_running_plugins >= 2){
    GFX_Message(NULL,
                "Using more than 2 VST plugins is only available to subscribers.<p>"
                "Subscribe <a href=\"http://users.notam02.no/~kjetism/radium/download.php\">here</a>.");
    return NULL;
  }
#endif // FULL_VERSION==0

  AEffect *aeffect = type_data->get_plugin_instance(VSTS_audioMaster);
  if (aeffect == NULL){
    GFX_Message(NULL,"Loading VST plugin %s failed",plugin_type->name);
    fprintf(stderr,"nope2\n");
    //abort();
    return NULL;
  }
  if (aeffect->magic != kEffectMagic){
    GFX_Message(NULL,"Loading VST plugin %s failed. It doesn't seem to be a VST plugin...",plugin_type->name);
    fprintf(stderr,"nope3\n");
    //abort();
    return NULL;
  }

  aeffect->user = plugin;

  aeffect->dispatcher(aeffect, effOpen, 0, 0, NULL, 0.0f);

  if(type_data->params==NULL)
    set_plugin_type_data(aeffect,(SoundPluginType*)plugin_type); // 'plugin_type' was created here (by using calloc), so it can safely be casted into a non-const.

  aeffect->dispatcher(
		     aeffect,
		     effSetSampleRate,
		     0,0,NULL,sample_rate);
  aeffect->dispatcher(aeffect,
		     effSetBlockSize,
		     0, block_size, NULL, 0);

  EditorWidget *editor_widget = new EditorWidget(NULL);
  //editor_widget->open(aeffect);

  Data *data=(Data*)calloc(1,sizeof(Data));
 
  data->aeffect = aeffect;
  data->editor_widget = editor_widget;
  data->sample_rate = sample_rate;

  data->events = (VstEvents*)calloc(1,sizeof(VstEvents) + (MAX_EVENTS*sizeof(VstMidiEvent*)));
  for(int i=0;i<MAX_EVENTS;i++)
    data->events->events[i] = (VstEvent*)&data->midi_events[i];

  aeffect->dispatcher(aeffect, effMainsChanged, 0, 1, NULL, 0.0f);

  num_running_plugins++;

  return data;
}

static void cleanup_plugin_data(SoundPlugin *plugin){
  Data *data = (Data*)plugin->data;
  AEffect *aeffect = data->aeffect;

  num_running_plugins--;

  delete data->editor_widget;
  aeffect->dispatcher(aeffect, effClose, 0, 0, NULL, 0.0f);

  free(data->events);
  
  free(data);
}

} // extern "C"

void add_vst_plugin_type(QFileInfo file_info){
  QString filename = file_info.absoluteFilePath();

  fprintf(stderr,"Trying to open \"%s\"\n",filename.ascii());

  QLibrary myLib(filename);


#if 0
  // qtractor does this. Check later what this is. And what about WIN64?
#if defined(FOR_WINDOWS) //defined(__WIN32__) || defined(_WIN32) || defined(WIN32)
  qtractorPluginFile_Function pfnInit
    = (qtractorPluginFile_Function) QLibrary::resolve("_init");
  if (pfnInit)
    (*pfnInit)();
#endif
#endif


  //fprintf(stderr,"Trying to resolve \"%s\"\n",filename.ascii());

  VST_GetPluginInstance get_plugin_instance = (VST_GetPluginInstance) myLib.resolve("VSTPluginMain");
  if (get_plugin_instance == NULL)
    get_plugin_instance = (VST_GetPluginInstance) myLib.resolve("main");
  if (get_plugin_instance == NULL){
    fprintf(stderr,"nope: \"%s\"\n",filename.ascii());
    return;
  }

  //fprintf(stderr,"Resolved \"%s\"\n",myLib.fileName().ascii());

  {
    SoundPluginType *plugin_type = (SoundPluginType*)calloc(1,sizeof(SoundPluginType));

    TypeData *type_data = (TypeData*)calloc(1,sizeof(TypeData));
    plugin_type->data = type_data;
    type_data->get_plugin_instance = get_plugin_instance;

    QString basename = file_info.fileName();
#if !defined(FOR_MACOSX)
    basename.resize(basename.size()-strlen(VST_SUFFIX)-1);
#endif

    plugin_type->type_name = "VST";
    plugin_type->name      = strdup(basename.ascii());

    plugin_type->is_instrument = true; // we don't know yet, so we set it to true.

    plugin_type->buffer_size_is_changed = buffer_size_is_changed;

    plugin_type->RT_process = RT_process;
    plugin_type->create_plugin_data = create_plugin_data;
    plugin_type->cleanup_plugin_data = cleanup_plugin_data;

    plugin_type->show_gui = show_gui;
    plugin_type->hide_gui = hide_gui;

    plugin_type->play_note       = play_note;
    plugin_type->set_note_volume = set_note_volume;
    plugin_type->stop_note       = stop_note;

    plugin_type->get_effect_value = get_effect_value;
    plugin_type->set_effect_value = set_effect_value;

    plugin_type->get_display_value_string=get_display_value_string;
    plugin_type->get_effect_name=get_effect_name;
    plugin_type->get_effect_description=get_effect_description;

    PR_add_plugin_type(plugin_type);
  }
}

static bool create_vst_plugins_recursively(const QString& sDir, QTime *time)
{
  QDir dir(sDir);
  dir.setSorting(QDir::Name);

  QFileInfoList list = dir.entryInfoList(QDir::AllEntries|QDir::NoDotAndDotDot);

  for (int i=0;i<list.count();i++){
    QFileInfo file_info = list[i];
    
    QString file_path = file_info.filePath();
    printf("hepp: %s\n",file_path.ascii());

    if (time->elapsed() > 1000*4){ //1000*30) {
      QMessageBox msgBox;
      msgBox.setText("Have used more than 30 seconds searching for VST plugins. Continue for another 30 seconds?");
      msgBox.addButton(QMessageBox::Yes);
      msgBox.addButton(QMessageBox::No);
      int ret = msgBox.exec();
      if(ret==QMessageBox::Yes){
        time->restart();
      } else
        return false;
    }

    if (file_info.isDir())
      if (create_vst_plugins_recursively(file_path, time)==false)
        return false;
    else if(file_info.suffix()==VST_SUFFIX){
      add_vst_plugin_type(file_info);
    }
  }

  return true;
}


void create_vst_plugins(void){

#if defined(FOR_MACOSX)
  QDir dir("/Library/Audio/Plug-Ins/VST/");

  //Digits.vst/Contents/MacOS/Digits 

  dir.setFilter(QDir::Dirs | QDir::NoDotAndDotDot);
  dir.setSorting(QDir::Name);
  QFileInfoList list = dir.entryInfoList();
  for (int i = 0; i < list.size(); ++i) {
    QFileInfo fileInfo = list.at(i);
    QDir dir(fileInfo.absoluteFilePath() + "/Contents/MacOS/");
    dir.setFilter(QDir::Files | QDir::NoDotAndDotDot);
    dir.setSorting(QDir::Name);
    
    QFileInfoList list = dir.entryInfoList();
    for (int i = 0; i < list.size(); ++i) {
      QFileInfo fileInfo = list.at(i);
      add_vst_plugin_type(fileInfo);
    }
  }

#else // !defined(FOR_MACOSX)
  QTime time;
  time.start();

  int num_paths = SETTINGS_read_int("num_vst_paths", 0);

  for(int i=0;i<num_paths; i++){
    const char *vst_path = SETTINGS_read_string(QString("vst_path")+QString::number(i), NULL);
    if(vst_path==NULL)
      continue;
    printf("vst_path: %s\n",vst_path);
    create_vst_plugins_recursively(vst_path, &time);
    PR_add_menu_entry(PluginMenuEntry::separator());
  }    

#endif // !defined(FOR_MACOSX)
}


std::vector<QString> VST_get_vst_paths(void){
  std::vector<QString> paths;

  int num_paths = SETTINGS_read_int("num_vst_paths", 0);

  for(int i=0;i<num_paths; i++){
    const char *vst_path = SETTINGS_read_string(QString("vst_path")+QString::number(i), NULL);
    if(vst_path==NULL)
      continue;

    QString s(vst_path);
    s = s.trimmed();
    if(s.length()==0)
      continue;

    paths.push_back(s);
  }

  return paths;
}

void VST_write_vst_paths(const std::vector<QString> &paths){
  SETTINGS_write_int("num_vst_paths",paths.size());
  for(unsigned int i=0;i<paths.size();i++){
    QString key_name = "vst_path" + QString::number(i);
    QString value_name = paths.at(i);
    SETTINGS_write_string(key_name.ascii(), value_name.ascii());
  }
}

void VST_add_path(QString path){
  std::vector<QString> paths = VST_get_vst_paths();
  paths.push_back(path);
  VST_write_vst_paths(paths);
}
