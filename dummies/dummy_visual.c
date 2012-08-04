
#define GFX_DONTSHRINK
#include "../common/nsmtracker.h"
#include "../common/visual_proc.h"



int GFX_CreateVisual(struct Tracker_Windows *tvisual){
  tvisual->os_visual=(struct OS_visual *)talloc_atomic(sizeof(struct OS_visual));
  tvisual->fontheight=17;
  tvisual->fontwidth=13;
  tvisual->org_fontheight=tvisual->fontheight;

  tvisual->x=0;
  tvisual->y=0;
  tvisual->width=300;
  tvisual->height=300;

  return 0;
}

int GFX_ShutDownVisual(struct Tracker_Windows *tvisual){return 0;}

void OS_GFX_C2V_bitBlt(
		    struct Tracker_Windows *window,
		    int from_x1,int from_x2,
		    int to_y
		    ){
}


/* window,x1,x2,x3,x4,height, y pixmap */
void OS_GFX_C_DrawCursor(
				      struct Tracker_Windows *window,
				      int x1,int x2,int x3,int x4,int height,
				      int y_pixmap
				      ){
}


void OS_GFX_P2V_bitBlt(
		    struct Tracker_Windows *window,
		    int from_x,int from_y,
		    int to_x,int to_y,
		    int width,int height
		    ){
}


void OS_GFX_BitBlt(
	struct Tracker_Windows *tvisual,
	int dx,int dy,
	int x,int y,
	int x2,int y2
){
}


void OS_GFX_FilledBox(struct Tracker_Windows *tvisual,int color,int x,int y,int x2,int y2,int where){
}

void OS_GFX_Box(struct Tracker_Windows *tvisual,int color,int x,int y,int x2,int y2,int where){
}


void OS_GFX_Line(struct Tracker_Windows *tvisual,int color,int x,int y,int x2,int y2,int where){
}

void OS_GFX_Point(
	struct Tracker_Windows *tvisual,
	int color,
        int brightness,
	int x,int y,
        int where
	)
{
}


void OS_GFX_Points(
                   struct Tracker_Windows *tvisual,
                   int color,
                   int brightness,
                   int num_points,
                   uint16_t *x,uint16_t *y,
                   int where
                   )
{
}


void OS_GFX_SetClipRect(
                        struct Tracker_Windows *tvisual,
                        int x,int y,
                        int x2,int y2,
                        int where
                        )
{
}


void OS_GFX_CancelClipRect(struct Tracker_Windows *tvisual,int where){
}


void OS_GFX_Text(
                 struct Tracker_Windows *tvisual,
                 int color,
                 char *text,
                 int x,
                 int y,
                 int width,
                 int flags,
                 int where
){
  if(flags & TEXT_BOLD){
    // turn on bold text
  }
  {  
    if(flags & TEXT_CENTER){
    }else{
    }
  }
  if(flags & TEXT_BOLD){
    // turn off bold text
  }
}                      


void GFX_EditorWindowToFront(struct Tracker_Windows *tvisual){}
void GFX_PlayListWindowToFront(void){}
void GFX_InstrumentWindowToFront(void){}
void GFX_HelpWindowToFront(void){}
void GFX_MaximizeEditorWindow(struct Tracker_Windows *tvisual){}
void GFX_MinimizeEditorWindow(struct Tracker_Windows *tvisual){}


bool GFX_SelectEditFont(struct Tracker_Windows *tvisual){return true;}

void GFX_SetWindowTitle(struct Tracker_Windows *tvisual,char *title){return ;}

int GFX_ResizeWindow(struct Tracker_Windows *tvisual,int x,int y){return 0;}

ReqType GFX_OpenReq(struct Tracker_Windows *tvisual,int width,int height,char *title){return NULL;}
void GFX_CloseReq(struct Tracker_Windows *tvisual,ReqType reqtype){return ;}

int GFX_GetInteger(struct Tracker_Windows *tvisual,ReqType reqtype,char *text,int min,int max){return 0;}

float GFX_GetFloat(struct Tracker_Windows *tvisual,ReqType reqtype,char *text,float min,float max){return 0.0f;}

char *GFX_GetString(struct Tracker_Windows *tvisual,ReqType reqtype,char *text){return "null";}

int GFX_Menu(
	struct Tracker_Windows *tvisual,
	ReqType reqtype,
	char *seltext,
	int num_sel,
	char **menutext
){return 0;}

char *GFX_GetLoadFileName(
	struct Tracker_Windows *tvisual,
	ReqType reqtype,
	char *seltext,
	char *dir
){return NULL;}

char *GFX_GetSaveFileName(
	struct Tracker_Windows *tvisual,
	ReqType reqtype,
	char *seltext,
	char *dir
){return NULL;}

