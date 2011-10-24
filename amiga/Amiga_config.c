/* Copyright 2000 Kjetil S. Matheussen

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











#include <dos.h>
#include <proto/dos.h>
#include <exec/types.h>

#include "../common/nsmtracker.h"

#include <string.h>
#include <time.h>

#include <proto/intuition.h>
#include <proto/diskfont.h>
#include <proto/graphics.h>
#include <proto/gadtools.h>

#include "visual_proc.h"
#include <proto/asl.h>
#include "visual_proc.h"
#include "Amiga_readstr_proc.h"

struct Screen *mainscreen;
extern APTR                   VisualInfo;

bool useworkbench;

struct TextFont *sysfont=NULL;
UWORD DriPens[] = {
	(UWORD)~0 };

bool AmigaConfig(void){
	FILE *file;
	int screenmodesel=0;
	struct ScreenModeRequester *requester;
	struct FontRequester *frequester;
	BPTR confile;

	file=fopen("radium:radium.conf","w");

	confile=Open("CON:0/0/500/200/Radium Config",0x3ee);

	while(screenmodesel!=1 && screenmodesel!=2){
		writestring(confile,"Please write '1' for workbench and\n");
		screenmodesel=GFX_GetInteger(NULL,confile,"'2' for custom screen >",1,2);
	}
	Close(confile);

	if(screenmodesel==2){
		printf("Now select the screen you want to use.\n");
		printf("Its smart to have a larger screen than the physical dimensions (select autoscroll),\n");
		printf("and you must select 8 or more colors.\n");
		requester=AllocAslRequestTags(
			ASL_ScreenModeRequest,
			ASLSM_DoWidth,TRUE,
			ASLSM_DoHeight,TRUE,
			ASLSM_DoDepth,TRUE,
			ASLSM_DoOverscanType,TRUE,
			ASLSM_DoAutoScroll,TRUE,
			TAG_END
		);

		if(requester==NULL){
			printf("Failed. Using Worbench screen instead.\n");
			screenmodesel=1;
		}else{
			if(AslRequestTags(requester,0L)==NULL){
				printf("Failed. Using Worbench screen instead.\n");
				screenmodesel=1;
				FreeAslRequest(requester);
			}else{
				fprintf(
					file,
					"%d\n%d\n%d\n%d\n%d\n%d\n",
					requester->sm_DisplayID,
					requester->sm_DisplayWidth,
					requester->sm_DisplayHeight,
					requester->sm_DisplayDepth,
					requester->sm_OverscanType,
					requester->sm_AutoScroll
				);
				if(requester->sm_DisplayDepth<3){
					fprintf(stderr,"Warning, at least 8 colors is required for normal operation!\n");
				}
				FreeAslRequest(requester);
				frequester=AllocAslRequestTags(
					ASL_FontRequest,
					ASLFO_TitleText,"Screen default font",
					TAG_END
				);
				if(frequester!=NULL){
					if(AslRequestTags(frequester,0L)==NULL){
						fprintf(stderr,"Could not open font. Using topaz8.\n");
						fprintf(file,"topaz.font\n8\n0\n0");
					}else{
						fprintf(
							file,
							"%s\n%d\n%d\n%d\n",
							frequester->fo_Attr.ta_Name,
							frequester->fo_Attr.ta_YSize,
							frequester->fo_Attr.ta_Style,
							frequester->fo_Attr.ta_Flags
						);
					}
					FreeAslRequest(frequester);
				}
			}
		}
	}

	if(screenmodesel==1){
		fprintf(file,"workbench\n");
	}
	fclose(file);

	return true;
}


char fontname[200];
struct TextAttr SysFontAttr;
char *screenname;

extern char *filedrawer;

bool ReadAmigaConfig(bool conf){

//	char fontname[200];
//	struct TextAttr SysFontAttr;
//	( STRPTR )"Trinomic.font",  6, 0x00, 0x00 };
	ULONG DisplayID;
	ULONG DisplayWidth;
	ULONG DisplayHeight;
	ULONG DisplayDepth;
	ULONG OverscanType;
	ULONG AutoScroll;

	FILE *file;
	char temp[500];

	long tid;
	struct tm *Tm;


	time(&tid);
	Tm=gmtime(&tid);

	screenname=malloc(strlen("RADIUM. Started: ")+strlen(asctime(Tm))+10);

	sprintf(screenname,"RADIUM. Started: %s",asctime(Tm));
	screenname[strlen(screenname)-1]=0;

	if(getasn("radium")==NULL){
		fprintf(stderr,"Device 'Radium:' not found. Please assign Radium first.\n");
		return false;
	}

	if(conf){
		if( ! AmigaConfig() ){
			return false;
		}
	}

	file=fopen("radium:radium.conf","r");

	if(file==NULL){
		printf(
			"This is probably the first or second time you run Radium.\n"
			"You will now be asked for screen-type. To config\n"
			"screen-type later, use the '-config' option.\n"
		);
		if( ! AmigaConfig() ){
			return false;
		}else{
			return ReadAmigaConfig(false);
		}
	}


	fgets(temp,400,file);
	if(!strcmp(temp,"workbench\n")){
		useworkbench=true;
		fclose(file);
		mainscreen=LockPubScreen(NULL);
		if ( ! ( VisualInfo = GetVisualInfo( mainscreen, TAG_DONE ))){
			fprintf(stderr,"Could not get visual info.\n");
			return(false);
		}
	}else{

		DisplayID=atoi(temp);
		fgets(temp,400,file);
		DisplayWidth=atoi(temp);
		fgets(temp,400,file);
		DisplayHeight=atoi(temp);
		fgets(temp,400,file);
		DisplayDepth=atoi(temp);
		fgets(temp,400,file);
		OverscanType=atoi(temp);
		fgets(temp,400,file);
		AutoScroll=atoi(temp);

		fgets(fontname,190,file);
		fontname[strlen(fontname)-1]=0;
		SysFontAttr.ta_Name=fontname;
		fgets(temp,400,file);
		SysFontAttr.ta_YSize=atoi(temp);
		fgets(temp,400,file);
		SysFontAttr.ta_Style=atoi(temp);
		fgets(temp,400,file);
		SysFontAttr.ta_Flags=atoi(temp);


//		SysFontAttr.ta_Style=0;
//		SysFontAttr.ta_Flags=0;

		fclose(file);
		useworkbench=false;
/*
		printf("%s,%d,%d,%d\n",
			SysFontAttr.ta_Name,
			SysFontAttr.ta_YSize,
			SysFontAttr.ta_Style,
			SysFontAttr.ta_Flags
		);
*/
		sysfont=OpenDiskFont(&SysFontAttr);
		if(sysfont==NULL){
			fprintf(stderr,"Could not open font. Using standard.\n");
		}

		if ( ! ( mainscreen = OpenScreenTags( NULL, SA_Left,	0,
				SA_Top,		0,
				SA_Width,	DisplayWidth,
				SA_Height,	DisplayHeight,
				SA_Depth,	DisplayDepth,
				SA_Font,	&SysFontAttr,
				SA_Type,	PUBLICSCREEN,
				SA_PubName,screenname,
				SA_DisplayID,DisplayID,
				SA_AutoScroll,	AutoScroll,
				SA_Overscan,	OverscanType,
				SA_Pens,	&DriPens[0],
				SA_Title,screenname,
				TAG_DONE ))){
			fprintf(stderr,"Could not open screen.\n");
			return( false );
		}

		if ( ! ( VisualInfo = GetVisualInfo( mainscreen, TAG_DONE ))){
			fprintf(stderr,"Could not get visual info.\n");
			return(false);
		}

		PubScreenStatus(mainscreen,0);

	}

	file=fopen("radium:loadpath.txt","r");
	if(file!=NULL){
		char temp[500];
		fgets(temp,480,file);
		if(temp[strlen(temp)-1]=='\n') temp[strlen(temp)-1]=0;
		filedrawer=malloc(strlen(temp)+1);
		sprintf(filedrawer,"%s",temp);
	}else{
		filedrawer="Radium:";
		file=fopen("Radium:loadpath.txt","w");
		fprintf(file,"Radium:\n");
	}
	fclose(file);

	return true;
}
















