/* Copyright 2016 Kjetil S. Matheussen

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



#include <math.h>

#include "nsmtracker.h"
#include "data_as_text_proc.h"
#include "Signature_proc.h"

#include "signaturetext_proc.h"


int SIGNATURETEXT_subsubtrack(struct Tracker_Windows *window){
  if (window->curr_track != SIGNATURETRACK)
    return -1;

  return 0;
}

bool SIGNATURETEXT_keypress(struct Tracker_Windows *window, struct WBlocks *wblock, int realline, const Place *place, int key){
  int subsubtrack = SIGNATURETEXT_subsubtrack(window);

  if (subsubtrack==-1)
    return false;

  if (key == EVENT_DEL)
    RemoveSignaturesCurrPos(window);
  else
    SetSignatureCurrPos(window);

  return true;
}
  
