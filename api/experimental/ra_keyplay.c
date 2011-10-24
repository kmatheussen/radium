


/*******************************************
  Computer Keyboard Note Playing/Editing 
*******************************************/

void keyDownPlay(int windownum,int notenum){
	struct Tracker_Windows *window=getWindowFromNum(windownum);

	notenum+=root->keyoct;

	if(notenum<=0 || notenum>127) return;
	if(window==NULL || window->curr_track<0) return;

	PATCH_playNoteCurrPos(window,notenum);
	InsertNoteCurrPos(window,notenum,0);
}

void polyKeyDownPlay(int windownum,int notenum){
	struct Tracker_Windows *window=getWindowFromNum(windownum);

	notenum+=root->keyoct;

	if(notenum<=0 || notenum>127) return;
	if(window==NULL || window->curr_track<0) return;

	PATCH_playNoteCurrPos(window,notenum);
	InsertNoteCurrPos(window,notenum,1);
}

void keyUpPlay(int windownum,int notenum){
	struct Tracker_Windows *window=getWindowFromNum(windownum);

	notenum+=root->keyoct;

	if(notenum<=0 || notenum>127) return;
	if(window==NULL || window->curr_track<0) return;

	PATCH_stopNoteCurrPos(window,notenum);
}

void setKeyAdd(int addnum){
	root->keyoct=addnum;
	GFX_UpdateKeyOctave(root->song->tracker_windows,root->song->tracker_windows->wblock);
}

void incKeyAdd(int incaddnum){
	root->keyoct+=incaddnum;
	if(root->keyoct>127 || root->keyoct<0){
		root->keyoct-=incaddnum;
	}else{
		GFX_UpdateKeyOctave(root->song->tracker_windows,root->song->tracker_windows->wblock);
	}
}

void decKeyAdd(int decaddnum){
	incKeyAdd(-decaddnum);
}

void switchEditOnOff(void){
	root->editonoff=root->editonoff?false:true;
}

void switchScrollPlayOnOff(void){
	root->scrollplayonoff=root->scrollplayonoff?false:true;
}

void switchSoundScrollOnOff(int windownum){
	struct Tracker_Windows *window=getWindowFromNum(windownum);
	if(window==NULL) return;

	window->playalong=window->playalong?false:true;
}


/*******************************************
  Navigating 
*******************************************/

void cursorDown(int windownum,int numlines){
	struct Tracker_Windows *window=getWindowFromNum(windownum);
	if(window==NULL) return;

	ScrollEditorDown(window,numlines);
}

void cursorNextNote(int windownum,int numlines){
	struct Tracker_Windows *window=getWindowFromNum(windownum);
	if(window==NULL) return;

	ScrollEditorNextNote(window);
}

void cursorPrevNote(int windownum,int numlines){
	struct Tracker_Windows *window=getWindowFromNum(windownum);
	if(window==NULL) return;

	ScrollEditorPrevNote(window);
}

void cursorPercentLine(int windownum,int percent){
	struct Tracker_Windows *window=getWindowFromNum(windownum);
	if(window==NULL) return;

	if(percent<0 || percent>99) return;

	ScrollEditorToPercentLine_CurrPos(window,percent);
}

void selectNextBlock(int windownum){
	struct Tracker_Windows *window=getWindowFromNum(windownum);
	if(window==NULL) return;

	SelectNextWBlock(window);
}

void selectPrevBlock(int windownum){
	struct Tracker_Windows *window=getWindowFromNum(windownum);
	if(window==NULL) return;

	SelectPrevWBlock(window);
}

void selectNextPlaylistBlock(void){
	SelectNextPlaylistWBlock(root->song->tracker_windows);
}

void selectPrevPlaylistBlock(void){
	SelectPrevPlaylistWBlock(root->song->tracker_windows);
}

void selectTrack(int windownum,int tracknum){
	struct Tracker_Windows *window=getWindowFromNum(windownum);
	if(window==NULL) return;

	SetCursorPosConcrete_CurrPos(window,(NInt)tracknum);
}


