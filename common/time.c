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



// It may be that these old timing structures may cover everything perfectly.
// Unfortunatly, I don't understand it anymore. I was a lot more trained
// in math when I wrote this code (year 2000), so it's likely to work.



/********** general ***********************
 STime format:
 1 second=PFREQ
 1 minute=PFREQ*60
******************************************/

#include <math.h>
#include "nsmtracker.h"
#include "playerclass.h"
#include "player_pause_proc.h"
#include "placement_proc.h"
#include "list_proc.h"
#include "reltempo_proc.h"
#include "seqtrack_proc.h"
#include "OS_Bs_edit_proc.h"

#include "time_proc.h"




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

static double Place2STime_from_times2(
                                     const struct STimes *times,
                                     double fp
                                     )
{

        int line1 = (int)fp;
        int line2 = line1+1;
	const struct STimes *stime= &times[line1];

//	printf("P2ST, block: %x, stime: %x, line: %d\n",block,stime,line);

	double time1=stime->time;

	double time2=times[line2].time;

	double fp1 = line1;
        double fp2 = fp1+1.0;

        if (fabs(fp1 - fp) < 0.0000001)
          return time1;

	const struct STimeChanges *stc = stime->timechanges;
        
	if(stc!=NULL && stc->l.p.counter==0){ // The stc->l.p.counter==0 test is to work around a bug in the STimeChanges generation. Timing will probably not be quite correct, but it's better than crashing.

		double orgfp2 = fp2;

		struct STimeChanges *next=NextSTimeChange(stc);

		while(next!=NULL){
                        double maybe_new_fp2 = GetDoubleFromPlace(&next->l.p);
                        if (maybe_new_fp2 >= fp) {
                                fp2=maybe_new_fp2;
				time2=next->time;
				break;
			}
			stc=next;
			next=NextSTimeChange(next);
		}

		fp1=GetDoubleFromPlace(&stc->l.p);
		time1=stc->time;

		if(stc->tempo1!=0.0f){
			double tempo=stc->tempo1 * (
                                                   RelTempo2RealRelTempo( (double) (                                                                                   
                                                                                   stc->rel + (stc->deltarel*(fp-fp1)/(2*(fp2-fp1))) // i.e. stc->rel + scale(fp, fp1, fp2, 0, stc->deltarel / 2.0f),
                                                                                   ))
                                                   );

			return (double) (
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


        return scale_double(fp, fp1, fp2, time1, time2);
}

STime Place2STime_from_times(
                             const struct STimes *times,
                             const Place *p
                             )
{

	if(0==p->counter){
          int line1 = p->line;
          const struct STimes *stime= &times[line1];
          STime time1=stime->time;
          return time1;
        }

        return Place2STime_from_times2(times, GetDoubleFromPlace(p));
}

STime Place2STime(
	const struct Blocks *block,
	const Place *p
){
  return Place2STime_from_times(block->times, p);
}

#define NUM_STIME2PLACE2_TRIES 40

static double STime2Place2(
                          const struct STimes *times,
                          double correct_time,
                          int num_tries_left,
                          
                          double low_result,
                          double high_result,
                          
                          STime low_time,
                          STime high_time
                          )
{
  double maybe_result;

  if (low_time==high_time)  // Happens at extreme high tempo.
    return low_time;

  if (num_tries_left==NUM_STIME2PLACE2_TRIES)
    maybe_result = scale_double(correct_time, low_time, high_time, low_result, high_result); // usually correct
  else
    maybe_result = (high_result+low_result) / 2.0f;

  /*
  printf("%d: %d %d (delta: %d) (%f, %f, %f)\n",
         num_tries_left, 
         (int)low_time,
         (int)high_time,
         (int)(high_time-low_time),
         low_result,
         maybe_result,
         high_result
         );
  */     
         

  if (num_tries_left==0)
    return maybe_result;

  double maybe_time = Place2STime_from_times2(times, maybe_result);

  if ( fabs(maybe_time-correct_time) < 1.0)
    return maybe_result;

  else if (maybe_time > correct_time)
    return STime2Place2(times,
                        correct_time,
                        num_tries_left-1,
                        
                        low_result,
                        maybe_result,
                        
                        low_time,
                        maybe_time
                        );

  else
    return STime2Place2(times,
                        correct_time,
                        num_tries_left-1,
                        
                        maybe_result,
                        high_result,
                        
                        maybe_time,
                        high_time
                        );
}


double STime2Place_f(
                  const struct Blocks *block,
                  double time
                  )
{

  if (time < 0)
    return 0.0;

  if (time >= getBlockSTimeLength(block))
    return block->num_lines;

  
#if 0
  int line1=0;
  int line2=1;
  for(;;){
    if (time >= block->times[line1].time && time < block->times[line2].time)
      break;

    line1++;
    line2++;    
  }
#else
  int line1,line2;
  int line=1;
  while(block->times[line].time < time) // could be optimized by binary search, but binary search is a horror to get right, and it's not that important here.
    line++;

  line2 = line;
  line1 = line-1;
#endif

  
  return STime2Place2(block->times,
                      time,
                      NUM_STIME2PLACE2_TRIES,
                      line1,
                      line2,
                      block->times[line1].time,
                      block->times[line2].time
                      );
}

Place STime2Place(
                  const struct Blocks *block,
                  STime time
                  )
{
  Place place;
  Place *firstplace = PlaceGetFirstPos();
  Place lastplace;
  PlaceSetLastPos(block, &lastplace);

  if (time < 0) {
    return *firstplace;
  }

  if (time >= getBlockSTimeLength(block)){
    PlaceTilLimit(&place,&lastplace);
    return place;
  }

  double place_f = STime2Place_f(block,time);

  Double2Placement(place_f, &place);
    
  if (PlaceGreaterOrEqual(&place, &lastplace))
    PlaceTilLimit(&place,&lastplace);

  else if (PlaceLessThan(&place,firstplace))
    place = *firstplace;
  
  //if (place.line==64)
  //  abort();

  return place;
}

STime getBlockSTimeLength(const struct Blocks *block){
  if (block==NULL){
#if !defined(RELEASE)
    abort();
#else
    return 48000; // This should never happened, and it has never happened.
#endif
  }
  if (block->num_lines != block->num_time_lines)
    RWarning("block->num_lines != block->num_time_lines: %d != %d",block->num_lines, block->num_time_lines);
    
  return block->times[block->num_time_lines].time;
}

bool isSTimeInBlock(const struct Blocks *block,STime time){
  STime block_length = getBlockSTimeLength(block);
  if(time > block_length)
    return false;
  else
    return true;
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

	double relp1;		// Place represented as double
	double reltempo1;

	double relp2;		// Place represented as double
	double reltempo2;

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
	bool create_timechange_node=false;
	bool reltempo_changed=false;

	double tfp1,tfp2;
	double reltempo1=stp->reltempo1;
	double reltempo2=stp->reltempo2;

	struct STimeChanges *timechange=NULL;

	if(PlaceEqual(p1,p2)){
		return;
	}
	if(reltempo1!=reltempo2) reltempo_changed=true;
	if(reltempo_changed || p1->counter>0) create_timechange_node=true;

	if(create_timechange_node){
		timechange=talloc(sizeof(struct STimeChanges));
		timechange->time=stp->nexttime;
		timechange->tempo1=0.0f;
	}

	if(0==p1->counter){
          stp->times[p1->line].time = stp->nexttime;
	}

	tfp1=GetDoubleFromPlace(p1);
	tfp2=GetDoubleFromPlace(p2);

	if(reltempo_changed){

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
					(timechange->tempo1*FindAverageRealRelTempo(timechange->rel,(double)(timechange->rel+timechange->deltarel)))
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

	if(create_timechange_node){
		PlaceCopy(&timechange->l.p,p1);
		ListAddElement3(&stp->times[p1->line].timechanges,&timechange->l); // Should this be ListAddElement3_a ?
	}
}

static void STP_fillinLastSTimeTempos(STimePlace *stp){
  stp->times[stp->lastplace.line + 1].time = stp->nexttime + 1; // Add 1 since it's not legal to have a place placed outside the block, so therefore the "lastplace" variable is only set to just almost the next line position after the last line.
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
			stp->relp2=GetDoubleFromPlace(&stp->temponode->l.p);
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



static void STP_Constructor(STimePlace *stp,struct Blocks *block, int default_bpm, int default_lpb){

	/* Misc */
	PlaceSetFirstPos(&stp->firstplace);
	PlaceSetLastPos(block,&stp->lastplace);
	stp->btempo=false;
	stp->blpb=false;
	stp->breltempo=false;

	/* Times */
	stp->times = talloc(sizeof(struct STimes)*(block->num_lines+1));


	/* Tempos */
	stp->tempo=default_bpm;
	stp->stempo= block->tempos;
	if(stp->stempo!=NULL && PlaceEqual(&stp->firstplace,&stp->stempo->l.p)){
		stp->tempo=stp->stempo->tempo;
		stp->stempo=NextTempo(stp->stempo);
	}

	/* LBPs */
	stp->lpb=default_lpb;
	stp->slpb=block->lpbs;
	if(stp->slpb!=NULL && PlaceEqual(&stp->firstplace,&stp->slpb->l.p)){
		stp->lpb=stp->slpb->lpb;
		stp->slpb=NextLPB(stp->slpb);
	}

	/* TempoNodes */
	stp->temponode=block->temponodes;
	stp->relp1=GetDoubleFromPlace(&stp->temponode->l.p);	// first temponode is allways at firstplace (just try dragging down the highest temponode).
	stp->reltempo1=stp->temponode->reltempo;

	stp->temponode=NextTempoNode(stp->temponode);
	stp->relp2=GetDoubleFromPlace(&stp->temponode->l.p);	// There is allways at least two temponode objects for each block.
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

	if(stp->p2->line == stp->p1->line){

		STP_fillinSTimes2(stp,stp->p1,stp->p2);

	}else{
                Place p1,p2;

		p1.counter=p2.counter=0;
		p1.dividor=p2.dividor=1;

		p2.line=stp->p1->line+1;
		STP_fillinSTimes2(stp,stp->p1,&p2);

		for(int line=p2.line;line<stp->p2->line;line++){
			p1.line=line;
			p2.line=line+1;
			STP_fillinSTimes2(stp,&p1,&p2);
		}

		STP_fillinSTimes2(stp,&p2,stp->p2);

	}

}


/**************** StimePlace end *************************/

#if 0
#include <inttypes.h>
/* A debugging function. */
void PrintSTimes(struct Blocks *block){
	struct WBlocks *wblock;
	struct LocalZooms **reallines;
	const struct STimes *stime;
	const struct STimeChanges *timechanges;
	int lasttime=0;
	int nowtime;
	int line,realline;

	for(line=0;line<=block->num_lines;line++){
		stime= &block->times[line];
		timechanges=stime->timechanges;
		nowtime=stime->time;

		printf("%d. %d. Delta: %d, Ch: %p\n",line,nowtime,nowtime-lasttime,timechanges);
		while(timechanges!=NULL){
                  printf("   place: %f, time: %" PRId64 ", tempo1: %f, rel: %f, deltarel: %f\n",
                         GetDoubleFromPlace(&timechanges->l.p),
                         timechanges->time,
                         timechanges->tempo1,
                         timechanges->rel,
                         timechanges->deltarel
			);
			timechanges=NextSTimeChange(timechanges);
		}
		lasttime=nowtime;
	}
	printf("--------reallines:----------\n");

	if(root->song->tracker_windows!=NULL){
		wblock=root->song->tracker_windows->wblock;
		if(wblock!=NULL){
                  reallines=wblock->reallines;
                  if (reallines!=NULL){
			lasttime=0;
			for(realline=0;realline<wblock->num_reallines;realline++){
				nowtime=Place2STime(block,&reallines[realline]->l.p);
				printf("realline: %d, Place: %f, time: %d, delta: %d,\n",realline,GetDoubleFromPlace(&reallines[realline]->l.p),nowtime,nowtime-lasttime);
				lasttime=nowtime;
			}
                  }
		}
	}

}
#endif

/*
static void update_is_beat(struct Blocks *block, struct STimes *times){
  struct LPBs *lpb = block->lpbs;

  int curr_lpb = root->lpb;
  int counter = 0;

  int line;

  for (line=0 ; line<block->num_lines ; line++){
    if(counter==curr_lpb){
      counter = 0;
    }

    while (lpb != NULL && lpb->l.p.line==line) {
      curr_lpb = lpb->lpb;
      counter = 0;
      
      lpb = NextLPB(lpb);
    }

    if(counter==0) {
      times[line].is_beat = true;
      //printf("%d\n",line);
    }

    counter++;
  }
}
*/

/********************************************************
  FUNCTION
    Update the STimes for the block 'block'. The STimes struct is
    used by other routines to easier and faster convert
    Placements to STimes for a block.
********************************************************/

void UpdateSTimes2(struct Blocks *block, int default_bpm, int default_lpb){
	STimePlace stp;

	STP_Constructor(&stp, block, default_bpm, default_lpb);

	do{
		STP_fillinSTimeTempos(&stp);
	}while(STP_getNextTimePlace(&stp));

        PC_Pause();{
          block->times = (const struct STimes*)stp.times;
          block->num_time_lines = block->num_lines;

          STP_fillinLastSTimeTempos(&stp);

          PLAYER_lock();{
            ALL_SEQTRACKS_FOR_EACH(){
              RT_legalize_seqtrack_timing(seqtrack);
            }END_ALL_SEQTRACKS_FOR_EACH;
          }PLAYER_unlock();
          
          SEQUENCER_update();
          BS_UpdatePlayList();

        }PC_StopPause(NULL);
        
        //update_is_beat(block, stp.times);

#if 0
	PrintSTimes(block);
#endif

}

void UpdateSTimes(struct Blocks *block){
  UpdateSTimes2(block, root->tempo, root->lpb);
}


void UpdateAllSTimes(void){
	struct Blocks *block=root->song->blocks;

	while(block!=NULL){
		UpdateSTimes(block);
		block=NextBlock(block);
	}
}










