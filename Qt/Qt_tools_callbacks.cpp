/* Copyright 2013 Kjetil S. Matheussen

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

#include "mQt_tools_callbacks.h"

//#include "../common/nsmtracker.h"

#include "helpers.h"

struct Root;
extern struct Root *root;


extern int num_users_of_keyboard;


void TOOLS_open(void){
  static Tools *widget=NULL;
  if(widget==NULL){
    widget = new Tools(NULL);
    //widget->setWindowModality(Qt::ApplicationModal);
  }

  safeShowOrExec(widget);
}



