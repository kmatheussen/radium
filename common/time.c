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
#include "hashmap_proc.h"
#include "reltempo_proc.h"
#include "seqtrack_proc.h"
#include "OS_Bs_edit_proc.h"
#include "Beats_proc.h"
#include "visual_proc.h"

#include "../embedded_scheme/s7extra_proc.h"
#include "../embedded_scheme/scheme_proc.h"

#include "../api/api_timing_proc.h"

#include "time_proc.h"



#if !USE_NEW_TIMING

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

#if 0
static double Place2STime_from_times2(
                                     const struct STimes *times,
                                     double fp
                                     )
{
  int line1 = (int)fp;
  int line2 = line1+1;

  double fp1 = line1;

  if (fabs(fp1 - fp) < 0.0000001)
    return time1;

  double fp2 = fp1+1.0;

  const struct STimes *stime = &times[line1];

  const struct STimeChanges *stc = stime->timechanges;

  while(stc!=NULL){

    double maybe_fp = GetDoubleFromPlace(stc->l.p);
    
  }
}
#endif

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



#else // USE_NEW_TIMING


#if 1 //defined(RELEASE)
#define D(n)
#else
#define D(n) n
#endif


struct STimeChange{
  double y1,x1,t1; // y=line (place as double), x = tempo at y, t = time at y
  double y2,x2,t2; //

  double logt1;   // Precalculated log(x1)     [ !!!!! NOT log(t1)    !!!!! ]
  double logt2t1; // Precalculated log(x2/x1)  [ !!!!! NOT log(t2/t2) !!!!! ]

  double glidingswing_scale_value; // hack to fix gliding swing values. Not used if it has value 0;
};


/*
  ;; For x2 >= x1:
  ;; integrate 1/(t1*((t2/t1)^(x/b))), x from 0 to c
  ;; https://www.wolframalpha.com/input/?i=integrate+1%2F(t1*((t2%2Ft1)%5E(x%2Fb))),+x+from+0+to+c
*/
static double get_stime_from_stimechange_equal_ratio_accelerando(const struct STimeChange *tc, double y){
  double t1 = tc->x1;
  double t2 = tc->x2;
  double b = tc->y2 - tc->y1;
  double c = y - tc->y1;

  //(b - b (t2/t1)^(-c/b))/(t1 log(t2/t1))
  double numerator = b - b * pow(t2/t1,-c/b);
  double denominator = t1 * tc->logt2t1;

  return pc->pfreq * 60 * numerator/denominator;
}



/*
  For x1 <= x2:
  integrate 1/(t1 + (t2 - (t1 * (t2/t1)^((b-x)/b)))), t1>0, t2>0, x from 0 to c
  (-b Log[t1]+c Log[t2/t1]+b Log[t1+t2-t2 ((t2/t1))^(-c/b)])/((t1+t2) Log[t2/t1])
*/
static double get_stime_from_stimechange_equal_ratio_deaccelerando(const struct STimeChange *tc, double y){
  double t1 = tc->x1;
  double t2 = tc->x2;
  double b = tc->y2 -tc->y1;
  double c = y - tc->y1;

  /*
      (/ (+ (- (* b (log t1)))
            (* c (log (/ t2 t1)))
            (* b (log (+ t1 t2 (- (* t2 (expt (/ t2 t1) (- (/ c b)))))))))
         (* (+ t1 t2)
            (log (/ t2 t1))))))
  */

  double n1 = -b * tc->logt1;
  double n2 = c * tc->logt2t1;
  double n3 = b * log(t1 + t2 - t2 * pow(t2/t1, -c/b));
  double numerator   = n1+n2+n3;

  double denominator = (t1+t2) * tc->logt2t1;

  return (double)pc->pfreq * 60.0 * numerator/denominator;
}


static double get_stime_from_stimechange_linear(const struct STimeChange *c, double y){
  const double k = (c->x2-c->x1) / (c->y2-c->y1);
  const double Tbp = scale_double(y, c->y1, c->y2, c->x1, c->x2);

  return (double)pc->pfreq * 60.0 * (1.0/k) * log(Tbp/c->x1);
}

static inline bool tchange_has_gliding_tempo(const struct STimeChange *c){
  return fabs(c->x2-c->x1) >= 0.003; // approx.
}

static double get_stime_from_stimechange(const struct STimeChange *c, double y, const bool has_t){
  //printf("  Diff: %f\n", fabs(c->x2-c->x1));

  double stime;
  
  if (!tchange_has_gliding_tempo(c)){

    // no tempo change (almost no tempo change)
    ///////////////////////////////////////////
    
    //    if (has_t)
    //   return scale_double(y, c->y1, c->y2, c->t1, c->t2) - c->t1;

    /*
      speed = distance / time
      distance = time*speed
      time = distance/speed
    */

    double distance = y - c->y1; 
    double speed = (c->x1 + c->x2) / 2.0;
    stime = (double)pc->pfreq * 60.0 * distance / speed;

  } else if (c->x2>c->x1) {

    // accelerando
    //////////////

    if (root->song->linear_accelerando)
      stime = get_stime_from_stimechange_linear(c, y);
    else
      stime = get_stime_from_stimechange_equal_ratio_accelerando(c, y);
    
  } else {

    // ritardando
    /////////////

    if (root->song->linear_ritardando)
      stime = get_stime_from_stimechange_linear(c, y);
    else
      stime = get_stime_from_stimechange_equal_ratio_deaccelerando(c, y);
    
  }

  if (c->glidingswing_scale_value != 0)
    return stime * c->glidingswing_scale_value;
  else
    return stime;
}

static const struct STimeChange *get_stimechange(const struct STimes *stimes, double y){
  const struct STimeChange *time_change=stimes[(int)y].tchanges;
  R_ASSERT_NON_RELEASE(time_change!=NULL);
  
  while(time_change->y2 < y){
    time_change = time_change + 1; // All changes in a block are allocated sequentially.
    //R_ASSERT_RETURN_IF_FALSE2(time_change->t1 > 0, (time_change-1)->t2); // Can happen, for instance if the second tempo node is crammed to the top of the block.
    R_ASSERT_NON_RELEASE(time_change->y2 != 0);
  }

  return time_change;
}

double Place2STime_from_times2(
                               const struct STimes *stimes,
                               double place_as_float
                               )
{
  R_ASSERT_NON_RELEASE(stimes!=NULL);
  R_ASSERT_NON_RELEASE(&stimes[(int)place_as_float] != NULL);

  // Find the right time_change to use.
  const struct STimeChange *time_change=get_stimechange(stimes, place_as_float);

  //printf("  %f: Found %f -> %f. (%f -> %f)", place_as_float, time_change->y1, time_change->y2, time_change->t1/pc->pfreq, time_change->t2/pc->pfreq);
  double ret = time_change->t1 + get_stime_from_stimechange(time_change, place_as_float, true);

  R_ASSERT_NON_RELEASE(ret < time_change->t2 + 10);

  if (ret > time_change->t2)
    return time_change->t2; // Could happen due to rounding errors.
  else
    return ret;
}

STime Place2STime_from_times(int num_lines, const struct STimes *times, const Place *p){
  if(0==p->counter)
    return times[p->line].time; // Most common situation. Should probably inline this function.

  if (p->line >= num_lines){
    R_ASSERT_NON_RELEASE(false);
    return times[num_lines].time;
  }

  double y = GetDoubleFromPlace(p);
  return Place2STime_from_times2(times, y);
}

STime Place2STime(
	const struct Blocks *block,
	const Place *p
){
  return Place2STime_from_times(block->num_lines, block->times, p);
}




// Precalculate timing for all line starts. (optimization)
struct STimes *create_stimes_from_tchanges(int num_lines, const struct STimeChange *time_changes, int num_elements){//const struct STimeChange **tchanges){

  struct STimes *stimes = talloc(sizeof(struct STimes)*(1+num_lines));

  int i=0;

  for(int line=0;line<=num_lines;line++){
    while(time_changes[i].y2 <= line && i < num_elements-1)
      i++;

#if !defined(RELEASE)
    if (line > time_changes[i].y2){
      RError("i>=num_elements: %d >= %d. line: %d, num_lines: %d",i,num_elements,line,num_lines);
      break;
    }
#endif
    
    const struct STimeChange *tchange = &time_changes[i];

    // more work to avoid slightly wrong values due to rounding errors.
    if (line==tchange->y1)
      stimes[line].time = tchange->t1;
    else if (line==tchange->y2)
      stimes[line].time = tchange->t2;
    else {    
      double dur = get_stime_from_stimechange(tchange, line, true);
      stimes[line].time = tchange->t1 + dur;
    }
    
    stimes[line].tchanges = tchange;
    
    D(printf("   STIME %d: %d. t1: %d, t2: %d. Dur: %f\n",
             line,
             (int)stimes[line].time,// / pc->pfreq,
             (int)tchange->t1,
             (int)tchange->t2,
             -1.0
             //dur/pc->pfreq);
             ));
  }
  /*
  stimes[num_lines].time = tchanges[num_lines-1]->t2;
  stimes[num_lines].tchanges = tchanges[num_lines-1];
  printf("   STIME %d: %f\n", num_lines, (double)stimes[num_lines].time / pc->pfreq);
  //  getchar();
  */

  return stimes;
}

static func_t *g_create_block_timings = NULL;

static dyn_t get_timings_from_scheme(const struct Blocks *block, const dyn_t dynbeats, const dyn_t filledout_swings, int bpm, int lpb){
#if defined(RELEASE)
  if (g_create_block_timings==NULL)
    g_create_block_timings=s7extra_get_func_from_funcname_for_storing("create-block-timings");
#else
  g_create_block_timings=s7extra_get_func_from_funcname("create-block-timings"); // Reload symbol every time so that we can redefine the function during runtime.
#endif
  
  return S7CALL(dyn_int_int_int_dyn_dyn_dyn_dyn_dyn,g_create_block_timings,
                block->num_lines,
                bpm,
                lpb,
                API_getAllBPM(block),
                API_getAllLPB(block),
                API_getAllTemponodes(block),
                dynbeats,
                filledout_swings
                );
}

// Fallback in case create-block-timings failed.
static dyn_t get_fallback_timings(const struct Blocks *block, const dyn_t dynbeats, int bpm, int lpb){
#if defined(RELEASE)
  if (g_create_block_timings==NULL)
    g_create_block_timings=s7extra_get_func_from_funcname_for_storing("create-block-timings");
#else
  g_create_block_timings=s7extra_get_func_from_funcname("create-block-timings");
#endif

  // First try scheme again, but now without swing.
  {
    dyn_t ret = S7CALL(dyn_int_int_int_dyn_dyn_dyn_dyn_dyn,g_create_block_timings,
                       block->num_lines,
                       bpm,
                       lpb,
                       API_getAllBPM(block),
                       API_getAllLPB(block),
                       API_getAllTemponodes(block),
                       dynbeats,
                       g_empty_dynvec
                       );
    
    if (ret.type==ARRAY_TYPE)
      return ret;
  }


  // Didn't work. Just return plain timing.
  {
    dynvec_t ret = {0};
    
    hash_t *hash = HASH_create(3);
    HASH_put_float(hash, ":x1", bpm*lpb);
    HASH_put_int(hash, ":y1", 0);
    HASH_put_float(hash, ":x2", bpm*lpb);
    HASH_put_int(hash, ":y2", block->num_lines);
    
    DYNVEC_push_back(&ret, DYN_create_hash(hash));
    return DYN_create_array(ret);
  }
}


// Returns an array of STimeChange elements
static struct STimeChange *create_time_changes_from_scheme_data(const dynvec_t *timings){
  struct STimeChange *time_changes = talloc(sizeof(struct STimeChange)*(timings->num_elements+1)); // Allocate one more element so that the last element is nulled out (used for assertion).
  
  for(int i=0;i<timings->num_elements;i++){
    hash_t *h = timings->elements[i].hash;
    time_changes[i].y1 = DYN_get_double_from_number(HASH_get_dyn(h, ":y1"));
    time_changes[i].y2 = DYN_get_double_from_number(HASH_get_dyn(h, ":y2"));

    double x1 = DYN_get_double_from_number(HASH_get_dyn(h, ":x1"));
    double x2 = DYN_get_double_from_number(HASH_get_dyn(h, ":x2"));

    time_changes[i].x1 = x1;
    time_changes[i].x2 = x2;

    time_changes[i].logt1 = log(x1);
    time_changes[i].logt2t1 = log(x2/x1);

    //printf("   TIMING. %d: %f,%f - %f,%f. t1: %f, t2: %f\n", i, time_changes[i].y1, time_changes[i].x1, time_changes[i].y2, time_changes[i].x2, time_changes[i].t1 / pc->pfreq, time_changes[i].t2 / pc->pfreq);
  }

  return time_changes;
}


// 1. Sets the .t1/.t2 attributes when .y1/.y2 is on a bar line. We do this to ensure all bars are equally long. (They are sometimes slightly wrong, probably due to rounding errors).
// The rounding errors also accumulate further down the block if we don't do this.
// This also makes sure that all STimeChange arrays of a block have the same duration. (Did experience a crash somewhere when that was not the case.)
//
// 2. Sets the .glidingswing_scale_value attribute.
// This is is necessary if using gliding swings since 'create-tempo-multipliers-from-swing' in timing.scm doesn't calculate correct tempo multipliers for gliding swings.
// (It's quite complicated to fix 'create-tempo-multipliers-from-swing'. Code is probably cleaner this way.)
//
static void postprocess_swing_changes(const struct Beats *beats, struct STimeChange *swing_changes, int num_swing_changes, const struct STimes *nonswing_stimes, int num_lines){

  int changepos1 = 0;
  
  double nonswing_t1 = 0;
  int last_barnum = beats->bar_num;

  // Iterate over all beats. When we hit a bar ("if (new_bar)"), we do our things.
  do{
    beats = NextBeat(beats);

    R_ASSERT_RETURN_IF_FALSE(changepos1 < num_swing_changes);
    
    bool new_bar = false;
    Place p2;

    if (beats==NULL){
      p2 = p_Create(num_lines,0,1);
      new_bar = true;
    } else if (beats->bar_num != last_barnum){
      p2 = beats->l.p;
      last_barnum = beats->bar_num;
      new_bar = true;
    }

    if (new_bar){

      double nonswing_t2 = Place2STime_from_times(num_lines, nonswing_stimes, &p2);

      double float_p2 = GetDoubleFromPlace(&p2);

      //printf("p2: %s. nonswing: %f. float_p2: %f. changepos1: %d\n", p_ToString(p2), nonswing_t2, float_p2, changepos1);
    
      int changepos2 = changepos1;

      bool contains_gliding_swing = false;
      
      // Find swing duration and the range of swing_changes that belongs to this bar.
      do{
        struct STimeChange *swing_change = &swing_changes[changepos2];
        if (swing_change->y1+0.00001 >= float_p2){
          R_ASSERT_NON_RELEASE(swing_change->y1 == float_p2); // I guess this assertion could fail in some situations without anything being wrong. (probably not in debug mode on an intel cpu though)
          break;
        }
        
        contains_gliding_swing = contains_gliding_swing || tchange_has_gliding_tempo(swing_change);
        
        changepos2++;
      }while(changepos2 < num_swing_changes);
      
      R_ASSERT_RETURN_IF_FALSE(changepos2 > changepos1);
      
      if (contains_gliding_swing){

        double nonswing_dur = nonswing_t2 - nonswing_t1;

        double swing_dur = 0.0;

        // find bar-duration of the swings.
        for(int pos = changepos1 ; pos < changepos2 ; pos++)
          swing_dur += get_stime_from_stimechange(&swing_changes[pos], swing_changes[pos].y2, false);
        
        // fill in glidingswing_scale_value for the changes belonging to this bar. (Workaround. The scheme code doesn't calculate correct tempo values for gliding swings.)
        if (fabs(nonswing_dur-swing_dur) > 0.001){
          R_ASSERT_RETURN_IF_FALSE(swing_dur > 0);
          double scale_value = nonswing_dur / swing_dur;
          for(int pos = changepos1 ; pos < changepos2 ; pos++)
            swing_changes[pos].glidingswing_scale_value = scale_value;
        }
      }

      // set t1 and t2 for bar start and bar end values.
      D(printf("   . Setting swing[%d].t1 = %f\n", changepos1, nonswing_t1));
      D(printf("   . Setting swing[%d].t2 = %f\n", changepos2-1, nonswing_t2));
      swing_changes[changepos1].t1 = nonswing_t1;
      swing_changes[changepos2-1].t2 = nonswing_t2;
      
      changepos1 = changepos2;
      nonswing_t1 = nonswing_t2;
    }    

  }while(beats!=NULL); //  && changepos1 < num_swing_changes
  
}


static void set_t_values_in_time_changes(struct STimeChange *time_changes, int num_time_changes){
  double t1 = 0;
  
  for(int i=0;i<num_time_changes;i++){
    struct STimeChange *time_change = &time_changes[i];

    if (time_changes->t1 == 0)
      time_change->t1 = t1;

    if (time_change->t2 == 0)
      time_change->t2 = time_change->t1 + get_stime_from_stimechange(time_change, time_change->y2, false);

    if (time_change->t2 < time_change->t1){
      R_ASSERT(false);
      time_change->t2 = time_change->t1;
    }
  
    t1 = time_change->t2;
  }

}
  

static const struct STimeChange *create_tchanges(const dynvec_t *timings, const struct Beats *beats, const struct STimes *nonswing_stimes, int num_lines){
  int num_time_changes = timings->num_elements;

  struct STimeChange *time_changes = create_time_changes_from_scheme_data(timings);

  if (nonswing_stimes != NULL)
    postprocess_swing_changes(beats, time_changes, num_time_changes, nonswing_stimes, num_lines);

  set_t_values_in_time_changes(time_changes, num_time_changes);

  return time_changes;
}



static struct STimes *create_stimes(const struct Blocks *block,
                                    const dyn_t dynbeats,
                                    const struct Beats *beats,
                                    const dyn_t filledout_swings,
                                    const struct STimes *nonswing_stimes,
                                    int default_bpm, int default_lpb)
{  
  //printf("LENGTH dynbeats: %d\n", dynbeats.array->num_elements);
  dyn_t timings = get_timings_from_scheme(block, dynbeats, filledout_swings, default_bpm, default_lpb);
  //printf("LENGTH timings: %d\n", timings.array->num_elements);
  
  if (timings.type!=ARRAY_TYPE){
    GFX_Message(NULL, "Error. timings function returned faulty data. Expected an array, got %s\n", DYN_type_name(timings.type));
    timings = get_fallback_timings(block, dynbeats, default_bpm, default_lpb);
    R_ASSERT(timings.type==ARRAY_TYPE);      
  }

  const struct STimeChange *time_changes = create_tchanges(timings.array, beats, nonswing_stimes, block->num_lines);
    
  struct STimes *times = create_stimes_from_tchanges(block->num_lines, time_changes, timings.array->num_elements);

  return times;
}

static dyn_t create_filledout_swings(const dyn_t global_swings, const dyn_t track_swings, int num_lines, dyn_t beats){
  static func_t *create_filledout_swings2 = NULL;

#if defined(RELEASE)
  if (create_filledout_swings2==NULL)
    create_filledout_swings2=s7extra_get_func_from_funcname_for_storing("create-filledout-swings2"); 
#else
  create_filledout_swings2=s7extra_get_func_from_funcname("create-filledout-swings2"); 
#endif
 
  return S7CALL(dyn_dyn_dyn_dyn_int,create_filledout_swings2,
                beats,
                global_swings,
                track_swings,
                num_lines
                );
}


static void update_stuff2(struct Blocks *blocks[], int num_blocks,
                          int default_bpm, int default_lpb, Ratio default_signature, bool plugins_should_receive_swing_tempo,
                          bool only_signature_has_changed, bool update_beats, bool update_swings)
{
  struct STimes *stimes_without_global_swings[num_blocks];
  struct STimes *stimes_with_global_swings[num_blocks];
  const struct Beats *beats[num_blocks];
  dyn_t dynbeats[num_blocks];
  dyn_t filledout_swingss[num_blocks];
  dynvec_t filledout_trackswingss[num_blocks];
  vector_t trackstimess[num_blocks];

  R_ASSERT_RETURN_IF_FALSE(g_scheme_has_inited1);
  
  memset(filledout_trackswingss, 0, sizeof(dynvec_t)*num_blocks);
  memset(trackstimess, 0, sizeof(vector_t)*num_blocks);
  
  bool only_update_beats[num_blocks];
  bool only_update_beats_for_all_blocks = true;

  for(int i=0;i<num_blocks;i++){
    bool has_swings = blocks[i]->swing_enabled==true && blocks[i]->swings!=NULL;
    only_update_beats[i] = has_swings==false && only_signature_has_changed;
    if (only_update_beats[i]==false)
      only_update_beats_for_all_blocks = false;
  }
  
  // beats
  {
    for(int i=0;i<num_blocks;i++){
      beats[i] = update_beats ? Beats_get(blocks[i], default_signature, default_lpb) : blocks[i]->beats;
      if (!only_update_beats[i]){
        D(printf("   Getting DynBeats for %d\n", i));
        dynbeats[i] = API_getAllBeats(beats[i]);
      }
    }
  }

  // swings
  {
    for(int i = 0 ; i < num_blocks ; i++){
      if (!only_update_beats[i]){
        
        if (update_swings){
          bool has_block_swings = blocks[i]->swing_enabled==true && blocks[i]->swings!=NULL;
            
          const dyn_t block_swings = API_getAllBlockSwings(blocks[i]);
          filledout_swingss[i] = create_filledout_swings(g_empty_dynvec, block_swings, blocks[i]->num_lines, dynbeats[i]);
          
          dyn_t empty_track_swing = {0};
          
          struct Tracks *track = blocks[i]->tracks;
          
          while(track!=NULL){
            if (track->swings==NULL){
              if (empty_track_swing.type==UNINITIALIZED_TYPE){
                if (has_block_swings==false)
                  empty_track_swing = filledout_swingss[i];
                else
                  empty_track_swing = create_filledout_swings(block_swings, g_empty_dynvec, blocks[i]->num_lines, dynbeats[i]); // Same as block_swings, except that everything is auto-filled out.
              }
              DYNVEC_push_back(&filledout_trackswingss[i], empty_track_swing);
            }else{
              const dyn_t filledout_trackswing = create_filledout_swings(block_swings, API_getAllTrackSwings(track), blocks[i]->num_lines, dynbeats[i]);
              DYNVEC_push_back(&filledout_trackswingss[i], filledout_trackswing);
            }
            track = NextTrack(track);
          }
        } else {
          filledout_swingss[i] = blocks[i]->filledout_swings;
        }
      }
    }
  }

  // times
  {
    for(int i = 0 ; i < num_blocks;i++){
      if (!only_update_beats[i]){

        bool has_block_swings = blocks[i]->swing_enabled==true && blocks[i]->swings!=NULL;
        
        int num_lines = blocks[i]->num_lines;

        stimes_without_global_swings[i] = create_stimes(blocks[i], dynbeats[i], beats[i], g_empty_dynvec, NULL, default_bpm, default_lpb);
        int64_t blocklen = stimes_without_global_swings[i][num_lines].time;
        
        if (!has_block_swings)
          stimes_with_global_swings[i] = stimes_without_global_swings[i];
        else{
          D(printf("  Calling create_stimes for %d\n", i));
          stimes_with_global_swings[i] = create_stimes(blocks[i], dynbeats[i], beats[i], filledout_swingss[i], stimes_without_global_swings[i], default_bpm, default_lpb);
          R_ASSERT(stimes_with_global_swings[i][num_lines].time == blocklen);
        }
        
        if (update_swings) {
          
          const struct Tracks *track = blocks[i]->tracks;
          while(track!=NULL){
            if (track->swings==NULL)
              VECTOR_push_back(&trackstimess[i], stimes_with_global_swings[i]);
            else{
              const dyn_t filledout_trackswing = filledout_trackswingss[i].elements[track->l.num];
              struct STimes *trackstimes = create_stimes(blocks[i], dynbeats[i], beats[i], filledout_trackswing, stimes_without_global_swings[i], default_bpm, default_lpb);
              VECTOR_push_back(&trackstimess[i], trackstimes);
              R_ASSERT(trackstimes[num_lines].time = blocklen);
            }
            track = NextTrack(track);
          }
          
        }

      }
    }
  }

  // apply
  PC_Pause();{
    for(int i=0;i<num_blocks;i++){
      struct Blocks *block = blocks[i];
      
      block->beats = beats[i];

      if (only_update_beats[i]==false) {
        
        if (filledout_swingss[i].type==ARRAY_TYPE)
          block->filledout_swings = filledout_swingss[i];
        
        if (stimes_without_global_swings[i] != NULL){ // Shouldn't happen, but if it does, we keep the old timing.
          block->times_without_global_swings = stimes_without_global_swings[i];
          block->num_time_lines = block->num_lines;
        }else{
          R_ASSERT_NON_RELEASE(false);
        }

        if (stimes_with_global_swings[i] != NULL) // Shouldn't happen, but if it does, we keep the old timing.
          block->times_with_global_swings = stimes_with_global_swings[i];
        else{
          R_ASSERT_NON_RELEASE(false);
        }
        
        if (plugins_should_receive_swing_tempo)
          block->times = block->times_with_global_swings;
        else
          block->times = block->times_without_global_swings;
        
        if (update_swings){
          struct Tracks *track = block->tracks;
          while(track!=NULL){
            track->filledout_swings = filledout_trackswingss[i].elements[track->l.num];
            track->times = trackstimess[i].elements[track->l.num];
            track = NextTrack(track);
          }
        }

      }
    }

    if (only_update_beats_for_all_blocks==false){
      PLAYER_lock();{
        int i = 0;
        SEQUENCER_timing_has_changed();
        ALL_SEQTRACKS_FOR_EACH(){
          PLAYER_maybe_pause_lock_a_little_bit(i++);
          RT_legalize_seqtrack_timing(seqtrack);
        }END_ALL_SEQTRACKS_FOR_EACH;
      }PLAYER_unlock();
    }
    
    SEQUENCER_update();
    BS_UpdatePlayList();
    
  }PC_StopPause(NULL);
}

static void update_all(struct Song *song,
                       int default_bpm, int default_lpb, Ratio default_signature, bool plugins_should_receive_swing_tempo,
                       bool only_signature_has_changed, bool update_beats, bool update_swings)
{
  int num_blocks = ListFindNumElements1(&song->blocks->l);
  
  struct Blocks *blocks[num_blocks];

  {
    int i = 0;
    struct Blocks *block=song->blocks;
    while(block!=NULL){
      blocks[i] = block;
      i++;
      block=NextBlock(block);
    }
  }

  update_stuff2(blocks, num_blocks,
                default_bpm, default_lpb, default_signature, plugins_should_receive_swing_tempo,
                only_signature_has_changed, update_beats, update_swings);
}

static void update_block(struct Blocks *block,
                         int default_bpm, int default_lpb, Ratio default_signature, bool plugins_should_receive_swing_tempo,
                         bool only_signature_has_changed, bool update_beats, bool update_swings)
{
  struct Blocks *blocks[1] = {block};

  update_stuff2(blocks, 1,
                default_bpm, default_lpb, default_signature, plugins_should_receive_swing_tempo,
                only_signature_has_changed, update_beats, update_swings);
}



void TIME_block_tempos_have_changed(struct Blocks *block){
  update_block(block, root->tempo, root->lpb, root->signature, root->song->plugins_should_receive_swing_tempo, false, false, true);
}

void TIME_block_LPBs_have_changed(struct Blocks *block){
  update_block(block, root->tempo, root->lpb, root->signature, root->song->plugins_should_receive_swing_tempo, false, true, true);
}

void TIME_block_signatures_have_changed(struct Blocks *block){
  update_block(block, root->tempo, root->lpb, root->signature, root->song->plugins_should_receive_swing_tempo, true, true, true);
}

void TIME_block_num_lines_have_changed(struct Blocks *block){
  update_block(block, root->tempo, root->lpb, root->signature, root->song->plugins_should_receive_swing_tempo, false, true, true);
}

void TIME_block_num_tracks_have_changed(struct Blocks *block){
  update_block(block, root->tempo, root->lpb, root->signature, root->song->plugins_should_receive_swing_tempo, false, false, true);
}

void TIME_block_swings_have_changed(struct Blocks *block){
  update_block(block, root->tempo, root->lpb, root->signature, root->song->plugins_should_receive_swing_tempo, false, false, true);
}

void TIME_everything_in_block_has_changed2(struct Blocks *block, const struct Root *root, const struct Song *song){
  update_block(block, root->tempo, root->lpb, root->signature, song->plugins_should_receive_swing_tempo, false, true, true);
}

void TIME_everything_in_block_has_changed(struct Blocks *block){
  TIME_everything_in_block_has_changed2(block, root, root->song);
}

void TIME_global_tempos_have_changed(void){
  update_all(root->song, root->tempo, root->lpb, root->signature, root->song->plugins_should_receive_swing_tempo, false, false, true);
}

void TIME_global_LPB_has_changed(void){
  update_all(root->song, root->tempo, root->lpb, root->signature, root->song->plugins_should_receive_swing_tempo, false, true, true);
}

void TIME_global_signature_has_changed(void){
  update_all(root->song, root->tempo, root->lpb, root->signature, root->song->plugins_should_receive_swing_tempo, true, true, true);
}

void TIME_everything_has_changed2(const struct Root *root, struct Song *song){
  update_all(song, root->tempo, root->lpb, root->signature, song->plugins_should_receive_swing_tempo, false, true, true);
}

void TIME_everything_has_changed(void){
  TIME_everything_has_changed2(root, root->song);
}



#endif // USE_NEW_TIMING



/******************
 *  Time -> Place *
 ******************/

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


