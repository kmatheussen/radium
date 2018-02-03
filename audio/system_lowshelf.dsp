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


import("filter_smoothing.lib");
import("math.lib");


// Using the default "low_shelf" filter in filter.lib, which is a third order low shelf filter.
//
// I think it is implemented by Julius O. Smith III, but I'm not sure. TODO: find author and credit properly.


// A slight modification of lowshelf to avoid running db2linear in the inner DSP loop when smoothing the level:

system_lowshelf = low_shelf(level, freq) with {

  freq = min(SR/2-1,
             vslider("[0] Lowshelf Freq [unit:Hz]",
                     315, 40.0, 2000.0, 1)
             );

  level = vslider("[1] Level [unit:dB]",
                  0.0, -35.0, 35.0, 0.1);// : smooth(0.999);
  
};

process = system_lowshelf;

