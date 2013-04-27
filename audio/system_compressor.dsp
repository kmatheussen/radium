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


// The functions compression_gain_mono, compressor_stereo, and compressor_stereo_demo
// are copied from effect.lib in the faust distribution.
//
// All three functions are written by Julius O. Smith III.
//
// They have been slightly modified and optimized by me.


import("filter.lib");
import("fast_log_exp.dsp");

effect = library("effect.lib");

def_ratio = 2.0;
def_threshold = -20.0;

def_attack = 50.148;
def_release = 100.237;

def_ingain = 0.0;
def_outgain = 0.0;

// ratio
ratio = hslider("[0] Ratio [style:slider]  [tooltip: A compression Ratio of N means that for each N dB increase in input signal level above Threshold, the output level goes up 1 dB]",
                def_ratio, 1, 20, 0.1);

// threshold
threshold = hslider("[1] Threshold [unit:dB] [style:slider]  [tooltip: When the signal level exceeds the Threshold (in dB), its level is compressed according to the Ratio]",
                    def_threshold, -20, 20, 0.1);

// attack
attack = hslider("[2] Attack [unit:ms] [style:slider]  [tooltip: Time constant in ms (1/e smoothing time) for the compression gain to approach (exponentially) a new lower target level (the compression `kicking in')]",
                     def_attack, 0, 500, 0.1)
  : *(0.001)
  : max(1/SR);

// release
release = hslider("[3] Release [unit:ms] [style: slider]  [tooltip: Time constant in ms (1/e smoothing time) for the compression gain to approach (exponentially) a new higher target level (the compression 'releasing')]",
                  def_release, 0, 1000, 0.1)
  : *(0.001)
  : max(1/SR);

// ingain
ingain = hslider("[4] Input Gain [unit:dB]  [tooltip: The compressed-signal output level is increased by this amount (in dB) to make up for the level lost due to compression]",
                 def_ingain, -40, 40, 0.1)
  : db2linear;

// outgain
outgain = hslider("[5] Output Gain [unit:dB]  [tooltip: The compressed-signal output level is increased by this amount (in dB) to make up for the level lost due to compression]",
                  def_outgain, -40, 40, 0.1)
  : db2linear : smooth(0.999);

gainview2(i) = _ <: _,hbargraph("[7] [%i] Gakk",-50,10) : attach;



compression_gain_mono(ratio,thresh,att,rel) = _
    : effect.amp_follower_ud(att,rel)
    //: linear2db : outminusin_db(ratio,thresh) : kneesmooth(att) : db2linear
    //: linear2db : outminusin_db(ratio,thresh) : db2linear
    //: fasterlinear2db : outminusin_db(ratio,thresh) : kneesmooth(att) : fasterdb2linear
    //: ll2_linear2db : outminusin_db(ratio,thresh) : kneesmooth(att) : ll2_db2linear

    //: fasterlinear2db : gainview2(0) : outminusin_db(ratio,thresh) : gainview2(1) : kneesmooth(att) : fasterdb2linear
    : ll2_linear2db : gainview2(0) : outminusin_db(ratio,thresh) : gainview2(1) : kneesmooth(att) : ll2_db2linear

    //: quitefastlinear2db : outminusin_db(ratio,thresh) : kneesmooth(att) : quitefastdb2linear
    //: linear2db_from_table : outminusin_db(ratio,thresh) : kneesmooth(att) : db2linear_from_table
    //: outminusin_linear(ratio,thresh) : kneesmooth(att) 
with {
  // kneesmooth(att) installs a "knee" in the dynamic-range compression,
  // where knee smoothness is set equal to half that of the compression-attack.
  // A general 'knee' parameter could be used instead of tying it to att/2:
  kneesmooth(att)  = smooth(tau2pole(att/2.0));
  // compression gain in dB:
  outminusin_linear(ratio,thresh,level) = level*db2linear(thresh) * (1.0/float(ratio)-1.0);
  outminusin_db(ratio,thresh,level) = max(level-thresh,0.0) * (1.0/float(ratio)-1.0);
  // Note: "float(ratio)" REQUIRED when ratio is an integer > 1!
};

compressor_stereo(ratio,thresh,att,rel,x,y) = cgm*x, cgm*y with {
  cgm = compression_gain_mono(ratio,thresh,att,rel,abs(x)+abs(y));
};

compressor_stereo_demo = compressor with{
    compressor = (_,_)
    //: *(ingain), *(ingain) 
    //  : displaygain(0)
    : compressor_stereo(ratio,threshold,attack,release)
    //  : displaygain(1)
    : *(outgain), *(outgain)
    //  : displaygain(2)
    ;
};

process = compressor_stereo_demo; //(x,0) : (_,!);

