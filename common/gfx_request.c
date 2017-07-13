/* Copyright 2003 Kjetil S. Matheussen

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


#include "nsmtracker.h"

#include <string.h>

#include "visual_proc.h"
#include "OS_visual_input.h"
#include "OS_settings_proc.h"


int GFX_GetInteger(struct Tracker_Windows *tvisual,ReqType reqtype,const char *text,int min,int max,bool program_state_is_valid){
	ReqType file;
	char rettext[50];
	int ret=min-1;

	if(reqtype==NULL){
          file=GFX_OpenReq(tvisual,(int)strlen(text)+10,4,"");
	}else{
          file=reqtype;
	}

	if(file==0){
		return ret;
	}

	while(ret<min || ret>max){
                GFX_WriteString(file,text);
		GFX_ReadString(file,rettext,40,program_state_is_valid);
		if(rettext[0]=='\0'){
			ret=min-1;
			break;
		}
		ret=atoi(rettext);
	}

	if(reqtype==NULL){
		GFX_CloseReq(tvisual,file);
	}

	return ret;
}

float GFX_GetFloat(struct Tracker_Windows *tvisual,ReqType reqtype,const char *text,float min,float max,bool program_state_is_valid){
	ReqType file;
	char rettext[50];
	float ret=min-1.0f;

	if(reqtype==NULL){
          file=GFX_OpenReq(tvisual,(int)strlen(text)+10,4,"");
	}else{
		file=reqtype;
	}

	if(file==0){
		return ret;
	}

	while(ret<min || ret>max){
		GFX_WriteString(file,text);
		GFX_ReadString(file,rettext,40,program_state_is_valid);
		if(rettext[0]=='\0'){
			ret=min-1.0f;
			break;
		}
		if(rettext[0]=='/'){
			ret=OS_get_double_from_string(rettext+1);
			if(ret!=0.0f){
				ret=(float)1.0f/ret;
			}else{
				ret=min-1.0f;
			}
		}else{
			ret=OS_get_double_from_string(rettext);
		}
	}

	if(reqtype==NULL){
		GFX_CloseReq(tvisual,file);
	}

	return ret;
}


char *GFX_GetString(struct Tracker_Windows *tvisual,ReqType reqtype,const char *text,bool program_state_is_valid){
	ReqType file;
	char temp[70];
	char *rettext=NULL;

	if(reqtype==NULL){
          file=GFX_OpenReq(tvisual,(int)strlen(text)+10,4,"");
	}else{
		file=reqtype;
	}

	if(file==0){
		return NULL;
	}

	GFX_WriteString(file,text);
	GFX_ReadString(file,temp,49,program_state_is_valid);
        
	if(temp[0] !='\0' ){
          rettext=talloc_atomic((int)strlen(temp)+2);
		sprintf(rettext,"%s",temp);
	}

	if(reqtype==NULL){
		GFX_CloseReq(tvisual,file);
	}

	return rettext;
}


int GFX_ReqTypeMenu(
                    struct Tracker_Windows *tvisual,
                    ReqType reqtype,
                    const char *seltext,
                    const vector_t v,
                    bool program_state_is_valid
){
  char temp[500];
  int lokke;
  int ret=0;
  bool created_reqtype = false;

  if(reqtype==NULL){
    reqtype=GFX_OpenReq(tvisual,(int)strlen(seltext)+10,v.num_elements+5,"");
    created_reqtype = true;
  }

  GFX_WriteString(reqtype,seltext);
  GFX_WriteString(reqtype,"\n");
  
  if(v.num_elements<1){
    RWarning("num_sel=%d",v.num_elements);
    goto exit;
  }

  for(lokke=0;lokke<v.num_elements;lokke++){
    sprintf(temp,"%d. %s\n",lokke+1,(char*)v.elements[lokke]);
    GFX_WriteString(reqtype,temp);
  }
  //  GFX_WriteString(reqtype,"\n");

  if(v.num_elements==1){
    // TODO: What?
    GFX_WriteString(reqtype,"> 1\n");
    goto exit;
  }
  
  while(ret<=0 || ret>v.num_elements){
    GFX_WriteString(reqtype,"> ");
    GFX_ReadString(reqtype,temp,5,program_state_is_valid);
    if(temp[0]=='\0'){
      ret=0;
      break;
    }
    ret=atoi(temp);
  }
  ret--;

 exit:
  if(created_reqtype==true)
    GFX_CloseReq(tvisual,reqtype);

  return ret;
}

