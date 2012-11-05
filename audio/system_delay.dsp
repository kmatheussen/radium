/* Copyright 2012 Kjetil S. Matheussen

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




/*
import("filter.lib");


process = @(ms2samples(dtime)) //min(50,max(0,dtime:smooth(0.999)))))
	with 
	{ 
		dtime	= hslider("delay[unit:ms][style:knob]", 0, 0, 50, 0.1);
	};
*/

// Some smaller simplifications. -Kjetil


declare name 	"SmoothDelay";
declare author 	"Yann Orlarey";
declare copyright "Grame";
declare version "1.0";
declare license "STK-4.3";


//--------------------------process----------------------------
//
// 	A stereo smooth delay with a feedback control
//  
//	This example shows how to use sdelay, a delay that doesn't
//  click and doesn't transpose when the delay time is changed
//-------------------------------------------------------------

import("music.lib");

max_delay = 50;
ms2samples(ms) = ms * SR / 1000.0;

process = sdelay(max_delay*48000*4/1000, interp, dtime)//max(0,min(ms2samples(max_delay), dtime)))
    with 
	{ 
            N 	   = ms2samples(max_delay);
            interp = 1024*32;
            dtime  = hslider("delay[unit:ms][style:knob]", 0, 0, max_delay, 0.1) : ms2samples;
	};

