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





#include <stdarg.h>
#include <errno.h>


#include "nsmtracker.h"
#include "visual_proc.h"
#include "OS_settings_proc.h"

#define TRACKER_DISK_IS_CALLING_NOW
#include "disk.h"
#undef TRACKER_DISK_IS_CALLING_NOW




DiskClass dc;


/*************************************************************
              SAVE FUNCTIONS
*************************************************************/
static const char *emptystringstring = "_________empty__________";

void DC_SaveCleanString(const char *string){
  if(strlen(string)==0){
    if(DISK_printf(dc.file,"%s",emptystringstring)<0) dc.success=false;
  }else{
    if(DISK_printf(dc.file,"%s",string)<0) dc.success=false;
  }
}

void DC_SaveST(const char *string){
	if(DISK_printf(dc.file,"\n\\\n%s\n",string)<0) dc.success=false;
}

void DC_SaveS(const char *string){
	if(DISK_printf(dc.file,"\n?%s\n",string)<0) dc.success=false;
}

void DC_SaveI(int integer){
	if(DISK_printf(dc.file,"%d\n",integer)<0) dc.success=false;
}

void DC_SaveUI(unsigned int integer){
	if(DISK_printf(dc.file,"%u\n",integer)<0) dc.success=false;
}

void DC_SaveL(long integer){
	if(DISK_printf(dc.file,"%ld\n",integer)<0) dc.success=false;
}

void DC_SaveN(long integer){
	if(DISK_printf(dc.file,"%ld\n",integer)<0) dc.success=false;
}

void DC_SaveUL(unsigned long integer){
	if(DISK_printf(dc.file,"%lu\n",integer)<0) dc.success=false;
}


/******************** OS depended function. Must be removed later *************/
void DC_SaveP(unsigned long integer){
	if(DISK_printf(dc.file,"%lu\n",integer)<0) dc.success=false;
}

void DC_SaveF(float integer){
  if(DISK_printf(dc.file,"%s\n",OS_get_string_from_double(integer))<0) dc.success=false;
}

void DC_SaveB(bool integer){
	DC_SaveI(integer?1:0);
}

void DC_SSI(const char *string,int integer){
	DC_SaveS(string);
	DC_SaveI(integer);
}

void DC_SSB(const char *string,bool integer){
	DC_SaveS(string);
	DC_SaveB(integer);
}

void DC_SSUI(const char *string,unsigned int integer){
	DC_SaveS(string);
	DC_SaveUI(integer);
}

void DC_SSL(const char *string,long integer){
	DC_SaveS(string);
	DC_SaveL(integer);
}

void DC_SSF(const char *string,float integer){
	DC_SaveS(string);
	DC_SaveF(integer);
}


/************ os spesific function, must be removed out later. ***********/
void DC_SSN(const char *string,long integer){
	DC_SaveS(string);
	DC_SaveL(integer);
}

void DC_SSUL(const char *string,unsigned long integer){
	DC_SaveS(string);
	DC_SaveUL(integer);
}

void DC_SSS(const char *string,const char *string2){
	if(string2==NULL) return;
	DC_SaveS(string);
        DC_SaveCleanString(string2);DC_SaveCleanString("\n");
}





/*************************************************************
              LOAD FUNCTIONS
*************************************************************/
int curr_disk_line;

static void DC_fgetsNoMatterWhat(void){
        curr_disk_line++;

	char *ret = dc.ls = DISK_read_trimmed_line(dc.file);

	if(ret==NULL){
          GFX_Message(NULL, "Unable to load string. Line %d",curr_disk_line);
          dc.success=false;
          return;
	}

	//printf("loading -%s-\n",ret);

	dc.ret=ret;
}

void DC_fgets(void){

  DC_fgetsNoMatterWhat();

  if (!strcmp(dc.ret,"")){
    DC_fgets();
    return;
  }
}



void *DC_doalloc(size_t size){
	void *ret=tralloc(size);
	if(ret==NULL){
          GFX_Message(NULL, "Not enough memory.\n");
          dc.success=false;
	}
	return ret;
}

void *DC_doalloc_atomic(size_t size){
	void *ret=tralloc_atomic(size);
	if(ret==NULL){
          GFX_Message(NULL, "Not enough memory (atomic).\n");
          dc.success=false;
	}
	return ret;
}

int DC_LoadI(void){
	DC_fgets();
	return atoi(dc.ret);
}

unsigned int DC_LoadUI(void){
	DC_fgets();
	return (unsigned int)strtoul(dc.ret,NULL,10);
}

long DC_LoadL(void){
	DC_fgets();
	return atol(dc.ret);
}

unsigned long DC_LoadUL(void){
	DC_fgets();
	return (unsigned int)strtoul(dc.ret,NULL,10);
}

float DC_LoadF(void){
	DC_fgets();
	return OS_get_double_from_string(dc.ret);
}

/************** OS depended function. ****************/
uint_32 DC_LoadU32(void){
	return DC_LoadUL();
}

/************** OS depended function. ****************/
NInt DC_LoadN(void){
	return DC_LoadL();
}

char *DC_LoadS(void){
	char *ret;
	DC_fgets();
        if(!strcmp(dc.ret, emptystringstring)){
          ret=DC_alloc_atomic(1);
          ret[0]='\0';
        }else{
          ret=DC_alloc_atomic(strlen(dc.ret)+1);
          strcpy(ret,dc.ret);
        }
error:
	return ret;
}

char *DC_LoadSNoMatterWhat(void){
	char *ret;
	DC_fgetsNoMatterWhat();
        if(!strcmp(dc.ret, emptystringstring)){
          ret=DC_alloc_atomic(1);
          ret[0]='\0';
        }else{
          ret=DC_alloc_atomic(strlen(dc.ret)+1);
          strcpy(ret,dc.ret);
        }
error:
	return ret;
}

bool DC_LoadB(void){
	DC_fgets();
        R_ASSERT(!strcmp(dc.ret,"1") || !strcmp(dc.ret,"0"));
	return (bool)(atoi(dc.ret)==1?true:false);
}

/*
bool DC_isObject(void){
	return dc.type==LS_OBJECT ? true : false;
}

bool DC_isEndObject(void){
	return dc.type==LS_ENDOBJECT ? true : false;
}

bool DC_isVariable(void){
	return dc.type==LS_VARIABLE ? true : false;
}
*/

int DC_getType(void){
	return dc.type;
}

int DC_Next(void){

	if( ! dc.success) return LS_ERROR;

	DC_fgets();

	switch(dc.ret[0]){
		case '\\':
			dc.type=LS_OBJECT;
			DC_fgets();
			break;
		case '?':
			dc.type=LS_VARIABLE;
			break;
		case '/':
			dc.type=LS_ENDOBJECT;
			break;
		default:
                  RError("DC_Next: Unknown type: \"%s\". Line: %d ",dc.ret,curr_disk_line);
                  dc.success=false;
                  return LS_ERROR;
	}

	return dc.type;
}


int DC_whatString(char **variables,int num){
	char *string;
	int ret=0;

	if( ! dc.success ) return LS_ERROR;

	if(dc.type==LS_VARIABLE){
		string=dc.ret+1;
	}else{
		string=dc.ret;
	}

	while(strcmp(string,variables[ret])){
	  //printf("num: %d, name: -%s-\n",ret,variables[ret]);
		ret++;
		if(ret>=num){
			RError(
			       "Error. Unknown identifier '%s', dc.type=%s., line: %d\n",
			       string,
			       dc.type==LS_VARIABLE?"LS_VARIABLE":dc.type==LS_OBJECT?"LS_OBJECT":"UNKNOWN",
                               curr_disk_line
                               );
			dc.success=false;
			return LS_ERROR;
		}
	}

	return ret;
}


void DC_SkipBlock(void){
  
  int blockindent=1;
  
  while(blockindent>0){
    DC_fgets();
    if(dc.success==false) return;

    switch(dc.ret[0]){
    case '\\':
      dc.type=LS_OBJECT;
      blockindent++;
      DC_fgets();
      break;
    case '?':
      dc.type=LS_VARIABLE;
      break;
    case '/':
      dc.type=LS_ENDOBJECT;
      blockindent--;
    default:
      break;
    }
  }
  DC_fgets();
}




