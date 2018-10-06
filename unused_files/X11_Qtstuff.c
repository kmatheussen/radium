
#include "../common/includepython.h"

#include "../common/nsmtracker.h"
#include "../common/visual_proc.h"

#include "X11_ReqType_proc.h"

#include "X11_Qtstuff_proc.h"

#ifdef FOR_WINDOWS
// TODO: Fix
int mkstemp(char *tmpl){
  return 0;
}
#endif // FOR_WINDOWS



void X11_StartQtStuff(void){
#if 0
//#if !USE_GTK_VISUAL
  PyRun_SimpleString("import X11_Qtstuff");
  PyRun_SimpleString("X11_Qtstuff.GFX_StartQtstuff()");
#endif
}

#if 0
//#if !USE_GTK_VISUAL

char *GFX_ReadStringFromPythonCommand(char *pythoncommand){
  FILE *file;
  char filename[50];
  char temp[strlen(pythoncommand)+500];
  char *ret=talloc_atomic(strlen(pythoncommand)+500);
  ret[0] = '\0';

  sprintf(filename,"/tmp/radiumipctempfileXXXXXX");
  int x1 = mkstemp(filename);
  (void)x1;

  sprintf(temp,pythoncommand,filename);


  PyRun_SimpleString(temp);


  file=fopen(filename,"r");
  if(file==NULL) {
    RError("GFX_ReadStringFromPythonCommand: File is null -%s-.\n",filename);
    return ret;
  }

  void* x2 = fgets(ret,strlen(pythoncommand)+499,file);
  fclose(file);
  (void)x2;

  printf("Tried to read -%s-\n",ret);
  sprintf(temp,"rm %s",filename);
  if(system(temp)==-1)
    RWarning("Unable to delete \"%s\"",filename);
  
  return ret;
}
#endif



#ifndef GUIISQT
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

#endif


#ifndef GUIISQT
void GFX_PlayListWindowToFront(void){
  PyRun_SimpleString("X11_BlockSelector.BS_ToFront()");
}

void GFX_InstrumentWindowToFront(void){
  PyRun_SimpleString("X11_MidiProperties.MIDI_ToFront()");
}
#endif

void GFX_HelpWindowToFront(void){}
void GFX_MaximizeEditorWindow(struct Tracker_Windows *tvisual){}
void GFX_MinimizeEditorWindow(struct Tracker_Windows *tvisual){}

#if 0
//#if !USE_GTK_VISUAL

int GFX_Menu(
	struct Tracker_Windows *tvisual,
	ReqType reqtype,
	char *seltext,
	int num_sel,
	char **menutext
){
  char *temp=talloc_atomic(num_sel*500);
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

#endif
