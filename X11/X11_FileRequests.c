
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
  char *ret=GFX_ReadStringFromPythonCommand("X11_Qtstuff.GFX_OpenFileRequester(\"%s\")");
  if(ret==NULL || strlen(ret)==0)
    return NULL;
  else
    return ret;
}


char *GFX_GetSaveFileName(
	struct Tracker_Windows *tvisual,
	ReqType reqtype,
	char *seltext,
	char *dir
){
  char *ret = GFX_ReadStringFromPythonCommand("X11_Qtstuff.GFX_SaveFileRequester(\"%s\")");
  if(ret==NULL || strlen(ret)==0)
    return NULL;
  else
    return ret;
}
