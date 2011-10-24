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
#include "placement_proc.h"
#include "list_proc.h"

#include "temponodes_legalize_proc.h"


/*****************************************************
  FUNCTION
    Make shure that:

    -That there are at least two temponodes

    -The first and last element is really placed
     at first and last position.

    -No nodes has the same position or is placed
     in a not sequental order.

    -block->lasttemponode really points to the last
     temponode.

  NOTE
    This might seem like (bad) hacking, but the alternative
    is much worse: Less readable code. Because a lot
    of checking in the lines functions has to be
    done then. So this is in my opinion a very good
    solution, allthough the result is not allways
    perfect. (could be frustrating.. and "bug"-reports
    might come. :) Anyway, the most important thing
    is that the program is stable.

*****************************************************/

void LegalizeTempoNodes(struct Blocks *block){
	Place *p1,*p;
	struct TempoNodes *temponode=block->temponodes;
	struct TempoNodes *temp;

	if(temponode==NULL){
		temponode=talloc(sizeof(struct TempoNodes));
		temponode->reltempo=0.0f;
		block->temponodes=temponode;
	}

	p1=&temponode->l.p;
	PlaceSetFirstPos(p1);
	temponode=NextTempoNode(temponode);

	if(temponode==NULL){
		temponode=talloc(sizeof(struct TempoNodes));
		temponode->reltempo=0.0f;
		PlaceSetLastPos(block,&temponode->l.p);
		block->lasttemponode=temponode;
		block->temponodes->l.next=&temponode->l;
		return;
	}

	for(;;){
		temp=NextTempoNode(temponode);

		p=&temponode->l.p;
		if(PlaceLessOrEqual(p,p1)){
			ListRemoveElement3(&block->temponodes,&temponode->l);
			temponode=NULL;
		}

		if(temp==NULL) break;
		temponode=temp;
		p1=p;
	}

	if(temponode==NULL){
		temponode=(struct TempoNodes *)ListLast3(&block->temponodes->l);
	}

	if(temponode==block->temponodes){
		temponode=talloc(sizeof(struct TempoNodes));
		temponode->reltempo=0.0f;
		PlaceSetLastPos(block,&temponode->l.p);
		block->lasttemponode=temponode;
		ListAddElement3(&block->temponodes,&temponode->l);
		return;
	}

	PlaceSetLastPos(block,&temponode->l.p);
	block->lasttemponode=temponode;
}


