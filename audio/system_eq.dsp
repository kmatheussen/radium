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


// This is the same equalizer which is used in the Faust implemention of Zita Rev.
// I think it is implemented by Julius O. Smith III, but I'm not sure. TODO: find author and credit properly.


// REFERENCE: 
//   P.A. Regalia, S.K. Mitra, and P.P. Vaidyanathan,
//   "The Digital All-Pass Filter: A Versatile Signal Processing Building Block"
//   Proceedings of the IEEE, 76(1):19-37, Jan. 1988.  (See pp. 29-30.)
//

 
// Zolzer style peaking eq (not used in zita-rev1) (filter.lib):
// pareq_stereo(eqf,eql,Q) = peak_eq(eql,eqf,eqf/Q), peak_eq(eql,eqf,eqf/Q);
// Regalia-Mitra peaking eq with "Q" hard-wired near sqrt(g)/2 (filter.lib):



system_eq = pareq_mono(freq, level, eq1q) with {

  pareq_mono(freq,level,Q) = peak_eq_rm(level,freq,tpbt)
  with {
    tpbt = wcT/sqrt(max(0,g)); // tan(PI*B/SR), B bw in Hz (Q^2 ~ g/4)
    wcT = 2*PI*freq/SR;  // peak frequency in rad/sample
    g = db2linear(level); // peak gain
  };

  eq1_group(x) = hgroup("[3] RM Peaking Equalizer 1", x);

  freq = eq1_group(vslider("[1] Eq1 Freq [unit:Hz] [style:knob]
       [tooltip: Center-frequency of second-order Regalia-Mitra peaking equalizer section 1]",
      315, 40, 20000, 1));
  
  level = eq1_group(vslider("[2] Eq1 Level [unit:dB] [style:knob]
       [tooltip: Peak level in dB of second-order Regalia-Mitra peaking equalizer section 1]",
                           0, -35, 35, 0.1));
  
  eq1q = eq1_group(vslider("[3] Eq1 Q [style:knob]
       [tooltip: Q = centerFrequency/bandwidth of second-order peaking equalizer section 1]",
       3, 0.1, 10, 0.1));
  
};

process = system_eq;

