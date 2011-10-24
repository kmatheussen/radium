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










#include <proto/reqtools.h>
#include <libraries/reqtools.h>

#include "nsmtracker.h"

#include <proto/intuition.h>
#include <proto/graphics.h>

#include "Amiga_colors_proc.h"

extern struct Screen *mainscreen;

LONG rtPaletteRequest(char *title,struct rtReqInfo *reqinfo,Tag tag, ...){
	return rtPaletteRequestA(title,reqinfo,(struct TagItem *)&tag);
}


void Amiga_ConfigColors(void){
	FILE *file;
	UWORD depth;
	long num_colors=1;
	long lokke;
	int lokke2;
	ULONG *table;

	rtPaletteRequest(
		"Color Config. Saved as radium:radiumcolors.conf",NULL,
		RT_Screen,mainscreen,
		TAG_END
	);

	file=fopen("radium:radiumcolors.conf","w");
	if(file==NULL){
		RError("Could not save radium:radiumcolors.conf\n");
		return;
	}
	depth=GetBitMapAttr(mainscreen->RastPort.BitMap,BMA_DEPTH);
	for(lokke=0;lokke<depth;lokke++){
		num_colors*=2;
	}

	fprintf(file,"%d\n",num_colors);

	table=talloc_atomic((size_t)(sizeof(ULONG)*num_colors*3));

	GetRGB32(mainscreen->ViewPort.ColorMap,0,num_colors,table);

	for(lokke=0;lokke<num_colors;lokke++){
		if(lokke==256) break;
		for(lokke2=0;lokke2<3;lokke2++){
			fprintf(file,"%lu\n",table[lokke*3+lokke2]);
		}
	}

	fclose(file);
	
}

void Amiga_SetScreenColors(void){
	FILE *file;
	UWORD depth;
	int num_colors_on_file;
	long lokke;
	int lokke2;
	long num_colors=1;
	char temp[100];
	ULONG rgb[3];

	file=fopen("radium:radiumcolors.conf","r");
	if(file==NULL) return;

	depth=GetBitMapAttr(mainscreen->RastPort.BitMap,BMA_DEPTH);
	for(lokke=0;lokke<depth;lokke++){
		num_colors*=2;
	}

	fgets(temp,90,file);
	num_colors_on_file=atoi(temp);

	for(lokke=0;lokke<num_colors_on_file;lokke++){
		if(lokke>=num_colors || lokke==256) break;
		for(lokke2=0;lokke2<3;lokke2++){
			fgets(temp,90,file);
			rgb[lokke2]=strtoul(temp,NULL,10);
		}
		SetRGB32(&mainscreen->ViewPort,lokke,rgb[0],rgb[1],rgb[2]);
	}

	fclose(file);
}


