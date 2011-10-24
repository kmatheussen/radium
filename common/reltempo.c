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


#include "nsmtracker.h"

#include "reltempo_proc.h"




/******************************************************
  FUNTION
    The reltempo-format is on a form that makes it
    easy to find average tempo and make gfx info.
    This function converts reltempo to real realtempo.
  EXAMPLES
    1.0 -> 2.0
    0.5 -> 1.5
    0.0 -> 1.0
   -0.5 -> 1/1.5
   -1.0 -> 1/2.0
******************************************************/

float RelTempo2RealRelTempo(float reltempo){

	if(reltempo>=0.0f) return (float)(reltempo+1.0f);

	return (float)(1.0f/((-reltempo)+1.0f));

}

/******************************************************
  FUNCTION
    Does the opposite thing of RelTempo2RealRelTempo.
******************************************************/

float RealRelTempo2RelTempo(float realreltempo){
	if(realreltempo>=1.0f) return (float)(realreltempo-1.0f);

	return (float)(-((1.0f/realreltempo)-1.0f));

}


/******************************************************
  FUNCTION
    Returns the Average RealRelTempo between two
    reltempos;
******************************************************/

float FindAverageRealRelTempo(float r1,float r2){
	return RelTempo2RealRelTempo((float)((r1+r2)/2.0f));
}


/******************************************************
  FUNCTION
    Returns the reltempo for the relative x value 'x',
    in the temponodearea in wblock 'wblock'.
******************************************************/
float Gfx2RelTempo(struct WBlocks *wblock,int x){

	int tw=wblock->temponodearea.width-1;
	float max=wblock->reltempomax-1.0f;

	return (float)((x*max*2/tw)-max);

}






