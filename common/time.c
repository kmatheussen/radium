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






/********** general ***********************
 STime format:
 1 second=PFREQ
 1 minute=PFREQ*60
******************************************/

#include <math.h>
#include "nsmtracker.h"
#include "playerclass.h"
#include "placement_proc.h"
#include "list_proc.h"
#include "reltempo_proc.h"

#include "time_proc.h"




extern struct Root *root;
extern PlayerClass *pc;


/********************************************************/
/*************** OUTPUT ********************************/
/********************************************************/


/********************************************************
  FUNCTION
    Returns the STime for Place 'p' for the block 'block'.
  NOTES
    The routine is very much used by the player-routine,
    and should be optimized as much as possible. It can
    be made faster (general optimization, binary-search
    and get rid of floats). Hmm, well, it probably doesn't use
    much time now, so its allso probably no use at all optimizing.
********************************************************/

STime Place2STime_from_times(
                             const struct STimes *times,
                             const Place *p
                             )
{

	int line1 = p->line;
        int line2 = line1+1;
	const struct STimes *stime= &times[line1];

//	printf("P2ST, block: %x, stime: %x, line: %d\n",block,stime,line);

	STime time1=stime->time;

	if(0==p->counter) return time1;

	STime time2=times[line2].time;

	float fp = GetfloatFromPlacement(p);
	float fp1 = line1;
        float fp2 = fp1+1.0;

	const struct STimeChanges *stc = stime->timechanges;
	if(stc!=NULL){

		float orgfp2 = fp2;

		struct STimeChanges *next=NextSTimeChange(stc);
		while(next!=NULL){
			if(PlaceGreaterOrEqual(&next->l.p,p)){
				fp2=GetfloatFromPlacement(&next->l.p);
				time2=next->time;
				break;
			}
			stc=next;
			next=NextSTimeChange(next);
		}

		fp1=GetfloatFromPlacement(&stc->l.p);
		time1=stc->time;

		if(stc->tempo1!=0.0f){
			float tempo=stc->tempo1 * (
                                                   RelTempo2RealRelTempo( (float) (
                                                                                   stc->rel + (stc->deltarel*(fp-fp1)/(2*(fp2-fp1)))
                                                                                   ))
                                                   );

			return (STime) (
				time1 + (
					pc->pfreq*60*(fp-fp1)/tempo
				)
			);

		}
		if(next!=NULL){
			time2=next->time;
		}else{
			fp2=orgfp2;
		}
	}


        return scale(fp, fp1, fp2, time1, time2);
}

STime Place2STime(
	const struct Blocks *block,
	const Place *p
){
  return Place2STime_from_times(block->times, p);
}

bool isSTimeInBlock(const struct Blocks *block,STime time){
	if(time > block->times[block->num_lines].time) return false;
	return true;
}

STime getBlockSTimeLength(const struct Blocks *block){
	return block->times[block->num_lines].time;
}




/********************************************************/
/****************  INPUT ********************************/
/********************************************************/


/**************** StimePlace "class" *********************/

typedef struct{

// private

	struct TempoNodes *temponode;
	SDB
	struct Tempos *stempo;
	struct LPBs *slpb;

	struct STimes *times;
	struct STimeChanges *stc;

	Place *p1;
	Place *p2;

	bool btempo;
	bool blpb;
	bool breltempo;

	int tempo;
	int lpb;

	bool first;

// even more private

	float relp1;		// Place represented as float
	float reltempo1;

	float relp2;		// Place represented as float
	float reltempo2;

// perhaps even a bit more private than even more private

	STime nexttime;

// and even more private than that again

	Place firstplace;
	Place lastplace;


}STimePlace;



/**************** StimePlace methods *********************/

	/******** private methods *******/


static void STP_cNextLPB(STimePlace *stp){
	stp->blpb=true;
}

static void STP_cNextTempo(STimePlace *stp){
	stp->btempo=true;
}

static void STP_cNextTempoNode(STimePlace *stp){
	stp->breltempo=true;
}

static void STP_NextLPB(STimePlace *stp){
	STP_cNextLPB(stp);
	if(stp->slpb!=NULL){
		stp->p2= &stp->slpb->l.p;
	}else{
		stp->p2= &stp->lastplace;
	}
}

static void STP_NextTempo(STimePlace *stp){
	STP_cNextTempo(stp);
	if(stp->stempo!=NULL){
		stp->p2= &stp->stempo->l.p;
	}else{
		stp->p2= &stp->lastplace;
	}
}

static void STP_NextTempoNode(STimePlace *stp){
	STP_cNextTempoNode(stp);

	if(stp->temponode!=NULL){
		stp->p2= &stp->temponode->l.p;
	}else{
		stp->p2= &stp->lastplace;
	}
}



static void STP_fillinSTimes2(
	STimePlace *stp,
	Place *p1,
	Place *p2
){
	bool tchange=false;
	bool rchange=false;

	float tfp1,tfp2;
	float reltempo1=stp->reltempo1;
	float reltempo2=stp->reltempo2;

	struct STimeChanges *timechange=NULL;

	if(PlaceEqual(p1,p2)){
		return;
	}
	if(reltempo1!=reltempo2) rchange=true;
	if(rchange || p1->counter>0) tchange=true;

	if(tchange){
		timechange=talloc(sizeof(struct STimeChanges));
		timechange->time=stp->nexttime;
		timechange->tempo1=0.0f;
	}

	if(0==p1->counter){
          stp->times[p1->line].time = stp->nexttime;
	}

	tfp1=GetfloatFromPlacement(p1);
	tfp2=GetfloatFromPlacement(p2);

	if(rchange){

		timechange->rel=
			reltempo1 + (
				(reltempo2-reltempo1)*(tfp1-stp->relp1)/(stp->relp2-stp->relp1)
			)
		;

		timechange->deltarel = (
			reltempo1 + (
				(reltempo2-reltempo1)*(tfp2-stp->relp1)/(stp->relp2-stp->relp1)
			)
		) - timechange->rel;


		timechange->tempo1 = stp->lpb * stp->tempo;


		stp->nexttime = (
			stp->nexttime + (
				pc->pfreq*60*(tfp2-tfp1)/(
					(timechange->tempo1*FindAverageRealRelTempo(timechange->rel,(float)(timechange->rel+timechange->deltarel)))
				)
			)
		);

	}else{
		stp->nexttime = (
			stp->nexttime + (
				pc->pfreq*60*(tfp2-tfp1)/(stp->tempo*stp->lpb*RelTempo2RealRelTempo(stp->reltempo1))
			)
		);
	}

	if(tchange){
		PlaceCopy(&timechange->l.p,p1);
		ListAddElement3(&stp->times[p1->line].timechanges,&timechange->l);
	}
}

static void STP_fillinLastSTimeTempos(STimePlace *stp){
		stp->times[stp->lastplace.line+1].time=stp->nexttime;	
}


	/******** public methods ********/


static bool STP_getNextTimePlace(STimePlace *stp){

	stp->p1=stp->p2;

	if(stp->btempo){
		stp->tempo=stp->stempo->tempo;
		stp->stempo=NextTempo(stp->stempo);

		stp->btempo=false;
	}

	if(stp->blpb){
		stp->lpb=stp->slpb->lpb;
		stp->slpb=NextLPB(stp->slpb);

		stp->blpb=false;
	}

	if(stp->breltempo){
		stp->relp1=stp->relp2;
		stp->reltempo1=stp->reltempo2;

		stp->temponode=NextTempoNode(stp->temponode);

		if(stp->temponode!=NULL){
			stp->relp2=GetfloatFromPlacement(&stp->temponode->l.p);
			stp->reltempo2=stp->temponode->reltempo;
		}

		stp->breltempo=false;
	}


	if(stp->temponode==NULL){
		if(stp->stempo==NULL){
			if(stp->slpb==NULL){
				if(PlaceEqual(&stp->lastplace,stp->p2)){
					return false;
				}else{
					stp->p2= &stp->lastplace;
				}
			}else{
				STP_NextLPB(stp);
			}
		}else{
			switch(PlaceCmp(&stp->stempo->l.p,&stp->slpb->l.p)){
				case -1:
					STP_NextTempo(stp);
					break;
				case 0:
					STP_cNextTempo(stp);
					STP_NextLPB(stp);
					break;
				case 1:
					STP_NextLPB(stp);
					break;
			}
		}
	}else{
		if(stp->stempo==NULL){
			if(stp->slpb==NULL){
				STP_NextTempoNode(stp);
			}else{
				switch(PlaceCmp(&stp->slpb->l.p,&stp->temponode->l.p)){
					case -1:
						STP_NextLPB(stp);
						break;
					case 0:
						STP_cNextLPB(stp);
						STP_NextTempoNode(stp);
						break;
					case 1:
						STP_NextTempoNode(stp);
						break;
				}
			}
		}else{
			if(stp->slpb==NULL){
				switch(PlaceCmp(&stp->temponode->l.p,&stp->stempo->l.p)){
					case -1:
						STP_NextTempoNode(stp);
						break;
					case 0:
						STP_cNextTempoNode(stp);
						STP_NextTempo(stp);
						break;
					case 1:
						STP_NextTempo(stp);
						break;
				}
			}else{
				switch(PlaceCmp(&stp->temponode->l.p,&stp->stempo->l.p)){
					case -1:
						switch(PlaceCmp(&stp->temponode->l.p,&stp->slpb->l.p)){
							case -1:
								STP_NextTempoNode(stp);
								break;
							case 0:
								STP_cNextTempoNode(stp);
								STP_NextLPB(stp);
								break;
							case 1:
								STP_NextLPB(stp);
								break;
						}
						break;
					case 0:
						switch(PlaceCmp(&stp->temponode->l.p,&stp->slpb->l.p)){
							case -1:
								STP_cNextTempoNode(stp);
								STP_NextTempo(stp);
								break;
							case 0:
								STP_cNextTempoNode(stp);
								STP_cNextTempo(stp);
								STP_NextLPB(stp);
								break;
							case 1:
								STP_NextLPB(stp);
								break;
						}
						break;
					case 1:
						switch(PlaceCmp(&stp->stempo->l.p,&stp->slpb->l.p)){
							case -1:
								STP_NextTempo(stp);
								break;
							case 0:
								STP_cNextTempo(stp);
								STP_NextLPB(stp);
								break;
							case 1:
								STP_NextLPB(stp);
								break;
						}
						break;
				}
			}
		}
	}

//	STP_setSTPtempos(stp);

	return true;
}



static void STP_Constructor(STimePlace *stp,struct Blocks *block){

	/* Misc */
	PlaceSetFirstPos(&stp->firstplace);
	PlaceSetLastPos(block,&stp->lastplace);
	stp->btempo=false;
	stp->blpb=false;
	stp->breltempo=false;

	/* Times */
	stp->times = talloc(sizeof(struct STimes)*(block->num_lines+1));


	/* Tempos */
	stp->tempo=root->tempo;
	stp->stempo= block->tempos;
	if(stp->stempo!=NULL && PlaceEqual(&stp->firstplace,&stp->stempo->l.p)){
		stp->tempo=stp->stempo->tempo;
		stp->stempo=NextTempo(stp->stempo);
	}

	/* LBPs */
	stp->lpb=root->lpb;
	stp->slpb=block->lpbs;
	if(stp->slpb!=NULL && PlaceEqual(&stp->firstplace,&stp->slpb->l.p)){
		stp->lpb=stp->slpb->lpb;
		stp->slpb=NextLPB(stp->slpb);
	}

	/* TempoNodes */
	stp->temponode=block->temponodes;
	stp->relp1=GetfloatFromPlacement(&stp->temponode->l.p);	// first temponode is allways at firstplace (just try dragging down the highest temponode).
	stp->reltempo1=stp->temponode->reltempo;

	stp->temponode=NextTempoNode(stp->temponode);
	stp->relp2=GetfloatFromPlacement(&stp->temponode->l.p);	// There is allways at least two temponode objects for each block.
	stp->reltempo2=stp->temponode->reltempo;


	/* Placements */
	stp->p1= &stp->firstplace;

	stp->p2=&stp->temponode->l.p;
	if(stp->stempo!=NULL){
		stp->p2=PlaceMin(stp->p2,&stp->stempo->l.p);
	}
	if(stp->slpb!=NULL){
		stp->p2=PlaceMin(stp->p2,&stp->slpb->l.p);
	}

	/* bools */
	if(stp->stempo!=NULL && PlaceEqual(stp->p2,&stp->stempo->l.p)) stp->btempo=true;
	if(stp->slpb!=NULL && PlaceEqual(stp->p2,&stp->slpb->l.p)) stp->blpb=true;
	if(PlaceEqual(stp->p2,&stp->temponode->l.p)) stp->breltempo=true;

	/* time */
	stp->nexttime=0;


}



static void STP_fillinSTimeTempos(STimePlace *stp){
	int line;
	Place p1,p2;

	if(stp->p2->line == stp->p1->line){

		STP_fillinSTimes2(stp,stp->p1,stp->p2);

	}else{

		p1.counter=p2.counter=0;
		p1.dividor=p2.dividor=1;

		p2.line=stp->p1->line+1;
		STP_fillinSTimes2(stp,stp->p1,&p2);

		for(line=p2.line;line<stp->p2->line;line++){
			p1.line=line;
			p2.line=line+1;
			STP_fillinSTimes2(stp,&p1,&p2);
		}

		STP_fillinSTimes2(stp,&p2,stp->p2);

	}

}


/**************** StimePlace end *************************/

#ifdef TRACKER_DEBUG
/* A debugging function. */
void PrintSTimes(struct Blocks *block){
	struct WBlocks *wblock;
	struct LocalZooms **reallines;
	struct STimes *stime;
	struct STimeChanges *timechanges;
	int lasttime=0;
	int nowtime;
	int line,realline;

	for(line=0;line<=block->num_lines;line++){
		stime= &block->times[line];
		timechanges=stime->timechanges;
		nowtime=stime->time;

		printf("%d. %d. Delta: %d, Ch: %p\n",line,nowtime,nowtime-lasttime,timechanges);
		while(timechanges!=NULL){
                  printf("   place: %f, time: %lld, tempo1: %f, rel: %f, deltarel: %f\n",
				GetfloatFromPlacement(&timechanges->l.p),
				timechanges->time,timechanges->tempo1,timechanges->rel,timechanges->deltarel
			);
			timechanges=NextSTimeChange(timechanges);
		}
		lasttime=nowtime;
	}
	printf("--------reallines:----------\n");

	if(root->song->tracker_windows!=NULL){
		wblock=root->song->tracker_windows->wblock;
		reallines=wblock->reallines;
		if(wblock!=NULL){
			lasttime=0;
			for(realline=0;realline<wblock->num_reallines;realline++){
				nowtime=Place2STime(block,&reallines[realline]->l.p);
				printf("realline: %d, Place: %f, time: %d, delta: %d,\n",realline,GetfloatFromPlacement(&reallines[realline]->l.p),nowtime,nowtime-lasttime);
				lasttime=nowtime;
			}
		}
	}

}
#endif

/********************************************************
  FUNCTION
    Update the STimes for the block 'block'. The STimes struct is
    used by other routines to easier and faster convert
    Placements to STimes for a block.
********************************************************/

void UpdateSTimes(struct Blocks *block){
	STimePlace stp;

	STP_Constructor(&stp,block);

	do{
		STP_fillinSTimeTempos(&stp);
	}while(STP_getNextTimePlace(&stp));

        block->times = (const struct STimes*)stp.times;

	STP_fillinLastSTimeTempos(&stp);

#ifdef TRACKER_DEBUG
	//PrintSTimes(block);
#endif

}


void UpdateAllSTimes(void){
	struct Blocks *block=root->song->blocks;

	while(block!=NULL){
		UpdateSTimes(block);
		block=NextBlock(block);
	}
}










