
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


void GFX_P_ClearWindow(struct Tracker_Windows *tvisual){}

void GFX_P_FilledBox(struct Tracker_Windows *tvisual,int color,int x,int y,int x2,int y2){}

void GFX_P_Box(struct Tracker_Windows *tvisual,int color,int x,int y,int x2,int y2){}

void GFX_P_Line(struct Tracker_Windows *tvisual,int color,int x,int y,int x2,int y2){}
void GFX_P_Point(struct Tracker_Windows *tvisual,int color,int x,int y){}

void GFX_P_Text(
	struct Tracker_Windows *tvisual,
	int color,
	char *text,
	int x,
	int y,
	bool clear
	)
{}

void GFX_P_DrawTrackBorderDouble(
	struct Tracker_Windows *tvisual,
	int x, int y, int y2
){
}

void GFX_P_DrawTrackBorderSingle(
	struct Tracker_Windows *tvisual,
	int x, int y, int y2
){
}

void GFX_V_DrawTrackBorderDouble(
	struct Tracker_Windows *tvisual,
	int x, int y, int y2
){
}
void GFX_V_DrawTrackBorderSingle(
	struct Tracker_Windows *tvisual,
	int x, int y, int y2
){
}

void GFX_P_InvertText(
	struct Tracker_Windows *tvisual,
	int color,
	char *text,
	int x,
	int y,
	bool clear
){
}


void GFX_P_InvertTextNoText(
	struct Tracker_Windows *tvisual,
	int color,
	int len,
	int x,
	int y,
	bool clear
){
}

void GFX_P_Scroll(
	struct Tracker_Windows *tvisual,
	int dx,int dy,
	int x,int y,
	int x2,int y2
){
}

void GFX_C_DrawCursor(
				      struct Tracker_Windows *window,
				      int x1,int x2,int x3,int x4,int height,
				      int y_pixmap
				      ){
}

void GFX_P2V_bitBlt(
		    struct Tracker_Windows *window,
		    int from_x,int from_y,
		    int to_x,int to_y,
		    int width,int height
		    ){
}

void GFX_C2V_bitBlt(
		    struct Tracker_Windows *window,
		    int from_x1,int from_x2,
		    int to_y
		    ){
}

void GFX_EditorWindowToFront(struct Tracker_Windows *tvisual){}
void GFX_PlayListWindowToFront(void){}
void GFX_InstrumentWindowToFront(void){}
void GFX_HelpWindowToFront(void){}
void GFX_MaximizeEditorWindow(struct Tracker_Windows *tvisual){}
void GFX_MinimizeEditorWindow(struct Tracker_Windows *tvisual){}


bool GFX_SelectEditFont(struct Tracker_Windows *tvisual){return true;}

void GFX_Line(struct Tracker_Windows *tvisual,int color,int x,int y,int x2,int y2){return ;}
void GFX_All_Line(struct Tracker_Windows *tvisual,int color,int x,int y,int x2,int y2){return ;}
void GFX_Box(struct Tracker_Windows *tvisual,int color,int x,int y,int x2,int y2){return ;}
void GFX_FilledBox(struct Tracker_Windows *tvisual,int color,int x,int y,int x2,int y2){return ;}
void GFX_Slider_FilledBox(struct Tracker_Windows *tvisual,int color,int x,int y,int x2,int y2){return ;}
void GFX_All_FilledBox(struct Tracker_Windows *tvisual,int color,int x,int y,int x2,int y2){return ;}

void GFX_Text(
	struct Tracker_Windows *tvisual,
	int color,
	char *text,
	int x,
	int y,
	bool clear
){return ;}

void GFX_Text_noborder(
	struct Tracker_Windows *tvisual,
	int color,
	char *text,
	int x,
	int y
){return ;}

void GFX_InvertText(
	struct Tracker_Windows *tvisual,
	int color,
	char *text,
	int x,
	int y
){return ;}


void GFX_InvertTextNoText(
	struct Tracker_Windows *tvisual,
	int color,
	int x,
	int y
){return ;}

void GFX_InitDrawCurrentLine(
	struct Tracker_Windows *tvisual,
	int x, int y, int x2, int y2
){return ;}
void GFX_InitDrawCurrentLine2(
	struct Tracker_Windows *tvisual,
	int x, int y, int x2, int y2
){return ;}

void GFX_DrawCursorPos(
	struct Tracker_Windows *tvisual,
	int fx, int fy, int fx2, int fy2,
	int x, int y, int x2, int y2
){return ;}

void GFX_DrawTrackBorder(
	struct Tracker_Windows *tvisual,
	int x, int y, int y2
){return ;}

void GFX_SetWindowTitle(struct Tracker_Windows *tvisual,char *title){return ;}

void GFX_Scroll(
	struct Tracker_Windows *tvisual,
	int dx,int dy,
	int x,int y,
	int x2,int y2
){return ;}

void GFX_ScrollDown(
	struct Tracker_Windows *tvisual,
	int dx,int dy,
	int x,int y,
	int x2,int y2
){return ;}

void GFX_ClearWindow(struct Tracker_Windows *tvisual){return ;}

int GFX_ResizeWindow(struct Tracker_Windows *tvisual,int x,int y){return 0;}

void SetNormalPointer(struct Tracker_Windows *tvisual){return ;}
void SetResizePointer(struct Tracker_Windows *tvisual){return ;}

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

