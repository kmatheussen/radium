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
  : db2linear;

gainview2(i) = _ <: _,hbargraph("[7] [%i] Gakk",-50,10) : attach;


/*
displaygain(i) = _,_ <: _,_,(abs,*(0):+) : _,_,gainview(i) : _,attach;

min_db          = -100.0; // db2linear_from_table(-100) = 0
min_linear_db   = -80.0; // Between min_db and min_linear_db, there is a linear conversion

max_linear_db   = 40.0; // Between max_linear_db and max_db, there is a linear conversion
max_db          = 80.0; // 10000.0 = db2linear_from_table(80) =  db2linear_from_table(81) =  db2linear_from_table(82) = ...

db2linear_from_table(x) = select2(x>min_db,
                                  0.0,
                                  select2(x>min_linear_db,
                                          scale(x,
                                                min_db, min_linear_db,
                                                0.0, db2linear(min_linear_db)),
                                          select2(x<max_db,
                                                  db2linear(max_db),
                                                  select2(x<max_linear_db,
                                                          scale(x,
                                                                max_linear_db, max_db, 
                                                                db2linear(max_linear_db), db2linear(max_db)),
                                                          table_read(x)))))
    with{
    table_read(x)        = rdtable(tablesize, db2linearform, int(scale(x, min_linear_db,max_linear_db, 0,tablesize)));
    db2linearform        = db2linear(scale(time, 0,tablesize, min_linear_db,max_linear_db));
    time                 = (+(1)~_ ) - 1; 			// 0,1,2,3,...
    scale(x,x1,x2,y1,y2) = y1 + (x-x1)*(y2-y1)/(x2-x1);
    tablesize       = 1 << 16;
  };


linear2db_from_table(x) = select2(x>min_linear,
                                  min_db,
                                  select2(x>min_linear_linear,
                                          scale(x,
                                                0.0, min_linear_db,
                                                min_linear, min_linear_linear),
                                          select2(x<max_linear,
                                                  max_db,
                                                  select2(x<max_linear_linear,
                                                          scale(x,
                                                                max_linear_db, max_db,
                                                                max_linear_linear, max_linear),
                                                          table_read(x)))))
    with{
      table_read(x)        = rdtable(tablesize, linear2dbform, int(scale(x, min_linear_linear,max_linear_linear, 0,tablesize)));
      linear2dbform        = linear2db(scale(time, 0,tablesize, min_linear_linear,max_linear_linear));
      time                 = (+(1)~_ ) - 1; 			// 0,1,2,3,...
      scale(x,x1,x2,y1,y2) = y1 + (x-x1)*(y2-y1)/(x2-x1);

      min_linear          = db2linear(min_db);
      min_linear_linear   = db2linear(min_linear_db);
      max_linear_linear   = db2linear(max_linear_db);
      max_linear          = db2linear(max_db);
      tablesize           = 1 << 16;
   };

fasterpow2 = ffunction(float fasterpow2 (float), "typepunning.h", "");
fasterlog = ffunction(float fasterlog (float), "typepunning.h", "");

fasterdb2linear(x) = fasterpow2(1.442695040*x*log(10)/20);
fasterlinear2db(x) = 20*log10(e)*fasterlog(x) with{e=2.71828182845904523536028747135266249;};

quitefastdb2linear(x) = exp(x*log(10.0)/20);
quitefastlinear2db(x) = 20*log10(e)*log(x) with{e=2.71828182845904523536028747135266249;};
*/


// The arithmetic for ll2_pow2 and ll2_log are copied from "fastonebigheader.h" written by
// Paul Mineiro:
// http://www.machinedlearnings.com/2011/06/fast-approximate-logarithm-exponential.html


pun_int_to_float = ffunction(float pun_int_to_float(int), "typepunning.h", "");
pun_float_to_int = ffunction(float pun_float_to_int(int), "typepunning.h", "");

ll2_pow2(x) = pun_int_to_float(( (1 << 23) * (clipp + 126.94269504))) with{
    clipp = max(-126.0,x);
};

ll2_exp(x) = ll2_pow2(1.442695040*x);
ll2_db2linear(x) = ll2_exp(x*log(10.0)/20);

ll2_log(x) = y - 87.989971088 with{
    y = float(pun_float_to_int(x)) * 8.2629582881927490e-8;
};

ll2_linear2db(x) = 20*log10(e)*ll2_log(x) with{e=2.71828182845904523536028747135266249;};

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

