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


#ifndef RADIUM_COMMON_DISK_H
#define RADIUM_COMMON_DISK_H

#include <string.h>

#include "OS_disk_proc.h"



//extern int errno;

struct PlayListHolder{
	struct ListHeader1 l;
	NInt listnum;
};
#define NextPlayListHolder(a) ((struct PlayListHolder *)((a)->l.next))

struct PatchNumHolder{
	struct ListHeader1 l;
	NInt blocknum;
	NInt tracknum;
	NInt patchnum;
};

typedef struct{
	disk_t *file;
	const wchar_t *filename;
	bool success;
	int type;
	char *ls;
	char *ret;

	struct PlayListHolder *playlist;
	NInt num_playlists;

	bool colorize;
	int startcolor;
}DiskClass;

//Types for ls:
#define LS_OBJECT 0			// Lines that start with a '/'. ls contains OBJECT type (next string)
#define LS_ENDOBJECT 1	 	// A '\'. ls doesn't contain anything useful.
#define LS_VARIABLE 2		// Lines that start with a '?'.
#define LS_ERROR -1

#ifndef TRACKER_DISK_IS_CALLING_NOW
extern DiskClass dc;
#endif

extern void DC_SaveCleanString(const char *string);
extern void DC_SaveST(const char *string);
extern void DC_SaveS(const char *string);
extern void DC_SaveI(int integer);
extern void DC_SaveUI(unsigned int integer);
extern void DC_SaveL(long integer);
extern void DC_SaveN(long integer);
extern void DC_SaveUL(unsigned long integer);
extern void DC_SaveP(unsigned long integer);
extern void DC_SaveF(float integer);
extern void DC_SaveB(bool integer);
extern void DC_SSI(const char *string,int integer);
extern void DC_SSB(const char *string,bool integer);
extern void DC_SSUI(const char *string,unsigned int integer);
extern void DC_SSL(const char *string,long integer);
extern void DC_SSF(const char *string,float integer);
extern void DC_SSN(const char *string,long integer);
extern void DC_SSUL(const char *string,unsigned long integer);
extern void DC_SSS(const char *string,const char *string2);

extern void DC_fgets(void);
extern int DC_LoadI(void);
extern long DC_LoadL(void);
extern float DC_LoadF(void);
extern uint_32 DC_LoadU32(void);
extern NInt DC_LoadN(void);
extern char *DC_LoadS(void);
extern char *DC_LoadSNoMatterWhat(void);
extern bool DC_LoadB(void);

extern void *DC_doalloc(size_t size);
extern void *DC_doalloc_atomic(size_t size);

extern int DC_Next(void);
extern void DC_SkipBlock(void);

extern int DC_whatString(char **variables,int num);

extern float disk_load_version;
extern int curr_disk_line;

#include "list_proc.h"


#define DC_success() if( ! dc.success ) return

#define DC_start(structname) DC_success();DC_SaveST(structname)
#define DC_end() DC_SaveCleanString("/\n")

#define DC_ListAdd1(a,b) ListAddElement1(a,(struct ListHeader1 *)b)
#define DC_ListAddP(a,b) ListAddElementP(a,b)
#define DC_ListAdd3(a,b) ListAddElement3(a,(struct ListHeader3 *)b)
#define DC_ListAdd3_a(a,b) ListAddElement3_a(a,(struct ListHeader3 *)b)

#define DC_alloc(a) DC_doalloc(a); if( !dc.success) goto error
#define DC_alloc_atomic(a) DC_doalloc_atomic(a); if( !dc.success) goto error



#define GENERAL_LOAD(num_objs,num_vars)				\
start:															\
	if( !dc.success) goto error;							\
		switch(DC_Next()){									\
			case LS_VARIABLE:									\
				switch(DC_whatString(vars,num_vars)){	\
					case 0:										\
						goto var0;								\
					case 1:										\
						goto var1;								\
					case 2:										\
						goto var2;								\
					case 3:										\
						goto var3;								\
					case 4:										\
						goto var4;								\
					case 5:										\
						goto var5;								\
					case 6:										\
						goto var6;								\
					case 7:										\
						goto var7;								\
					case 8:										\
						goto var8;								\
					case 9:										\
						goto var9;								\
					case 10:										\
						goto var10;								\
					case 11:										\
						goto var11;								\
					case 12:										\
						goto var12;								\
					case 13:										\
						goto var13;								\
					case 14:										\
						goto var14;								\
					case 15:										\
						goto var15;								\
					case 16:										\
						goto var16;								\
					case 17:										\
						goto var17;								\
					case 18:										\
						goto var18;								\
					case 19:										\
						goto var19;								\
					case 20:										\
						goto var20;								\
					default:										\
						goto error;								\
				}													\
				break;											\
			case LS_OBJECT:									\
				switch(DC_whatString(objs,num_objs)){	\
					case 0:										\
						goto obj0;								\
					case 1:										\
						goto obj1;								\
					case 2:										\
						goto obj2;								\
					case 3:										\
						goto obj3;								\
					case 4:										\
						goto obj4;								\
					case 5:										\
						goto obj5;								\
					case 6:										\
						goto obj6;								\
					default:										\
						goto error;								\
				}													\
				break;											\
			case LS_ENDOBJECT:								\
				goto end;										\
			case LS_ERROR:										\
				goto error;										\
	}


/*

var0:
var1:
var2:
var3:
var4:
var5:
var6:
var7:
var8:
var9:
var10:
var11:
var12:
var13:
var14:
var15:
var16:
var17:
var18:

obj0:
obj1:
obj2:
obj3:
obj4:
obj5:
obj6:

error:
end:

*/

#endif // RADIUM_COMMON_DISK_H
