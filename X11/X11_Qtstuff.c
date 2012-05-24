
#include "Python.h"

#include "../common/nsmtracker.h"
#include "../common/visual_proc.h"

#include "X11_ReqType_proc.h"

#include "X11_Qtstuff_proc.h"


void X11_StartQtStuff(void){
  PyRun_SimpleString("import X11_Qtstuff");
  PyRun_SimpleString("X11_Qtstuff.GFX_StartQtstuff()");
}


char *GFX_GetLoadFileName(
	struct Tracker_Windows *tvisual,
	ReqType reqtype,
	char *seltext,
	char *dir
){
  return GFX_ReadStringFromPythonCommand("X11_Qtstuff.GFX_OpenFileRequester(\"%s\")");
}


char *GFX_GetSaveFileName(
	struct Tracker_Windows *tvisual,
	ReqType reqtype,
	char *seltext,
	char *dir
){
  return GFX_ReadStringFromPythonCommand("X11_Qtstuff.GFX_SaveFileRequester(\"%s\")");
}

void GFX_PlayListWindowToFront(void){
  PyRun_SimpleString("X11_BlockSelector.BS_ToFront()");
}
void GFX_InstrumentWindowToFront(void){
  PyRun_SimpleString("X11_MidiProperties.MIDI_ToFront()");
}

void GFX_HelpWindowToFront(void){}
void GFX_MaximizeEditorWindow(struct Tracker_Windows *tvisual){}
void GFX_MinimizeEditorWindow(struct Tracker_Windows *tvisual){}

int GFX_Menu(
	struct Tracker_Windows *tvisual,
	ReqType reqtype,
	char *seltext,
	int num_sel,
	char **menutext
){
  char *temp=talloc(num_sel*500);
  char *selectedtext;
  int lokke;

  if(reqtype==NULL || num_sel>20){

    sprintf(temp,"X11_Qtstuff.GFX_MenuDialog(\"%%s\",\"%s\"",seltext);
    
    for(lokke=0;lokke<num_sel;lokke++){
      sprintf(temp,"%s,\"%s\"",temp,menutext[lokke]);
    }
    sprintf(temp,"%s,\"cancel\")",temp);
    
    selectedtext=GFX_ReadStringFromPythonCommand(temp);
    
    for(lokke=0;;){
      if(!strcmp(selectedtext,menutext[lokke])) break;
      lokke++;
      if(lokke==num_sel){
	return -1;
      }
    }

    return lokke;
  }else{
    return GFX_ReqTypeMenu(tvisual,reqtype,seltext,num_sel,menutext);
  }
}
