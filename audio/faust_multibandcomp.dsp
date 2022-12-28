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


// Code is based on "compressor_demo" from the file effects.lib in the Faust distribution,
// written by Julius O. Smith III.

// benchmark:
// /home/kjetil/faudiostream/compiler/faust -a bench.cpp faust_multibandcomp.dsp >benchmark.cpp && g++ benchmark.cpp  -O3 -Wall -msse -mfpmath=sse  -o benchmark -lpthread && ./benchmark


import("filter.lib");
effect = library("effect.lib");
import("fast_log_exp.dsp");

def_ratio = 2.0;
def_threshold = -20.0;

def_attack = 100;
def_release = 200;

def_ingain = 0.0;
def_outgain = 0.0;


solo(i) = int(checkbox("Band %i: [0] Solo [tooltip: When this is checked, the compressor is enabled. If not, sound is muted.]"));
get_gain_from_solo(i) = float(solo(i) | ( (solo(1)==0) & (solo(2)==0) & (solo(3)==0) ));

compression_gain_mono(ratio,thresh,att,rel) = _
    : effect.amp_follower_ud(att,rel)
    : ll2_linear2db 
    : outminusindb(ratio,thresh) 
    : kneesmooth(att) 
    : ll2_db2linear
with {
  // kneesmooth(att) installs a "knee" in the dynamic-range compression,
  // where knee smoothness is set equal to half that of the compression-attack.
  // A general 'knee' parameter could be used instead of tying it to att/2:
  kneesmooth(att)  = smooth(tau2pole(att/2.0));
  // compression gain in dB:
   outminusindb(ratio,thresh,level) = max(level-thresh,0.0) * (1.0/float(ratio)-1.0);
  // Note: "float(ratio)" REQUIRED when ratio is an integer > 1!
};

compressor_stereo(ratio,thresh,att,rel,x,y) = cgm*x, cgm*y with {
  cgm = compression_gain_mono(ratio,thresh,att,rel,abs(x)+abs(y));
};


//---------------------------- compressor_demo -------------------------
// USAGE: _,_ : compressor_demo : _,_;
//
compressor(i) = effect.bypass2(bypass,compressor_stereo_demo) with {

    bypass = checkbox("Band %i:[0.5] Bypass [tooltip: When this is checked, the compressor is enabled. If not, sound is muted.]");

    displaygain = _,_ <: _,_,(abs,abs:+) : _,_,gainview : _,attach;

    compressor_stereo_demo = *(ingain), *(ingain) 
    : displaygain(compressor_stereo(ratio,threshold,attack,release)) 
    : *(outgain), *(outgain)
    : *(get_gain_from_solo(i)),*(get_gain_from_solo(i))
    : outgainview;

    ratio = hslider("Band %i:[2] Ratio [style:slider]  [tooltip: A compression Ratio of N means that for each N dB increase in input signal level above Threshold, the output level goes up 1 dB]",
                    def_ratio, 1, 20, 0.1);

    threshold = hslider("Band %i:[3] Threshold [unit:dB] [style:slider]  [tooltip: When the signal level exceeds the Threshold (in dB), its level is compressed according to the Ratio]",
                        def_threshold, -100, 10, 0.1);
    
    attack = hslider("Band %i:[4] Attack [unit:ms] [style:slider]  [tooltip: Time constant in ms (1/e smoothing time) for the compression gain to approach (exponentially) a new lower target level (the compression `kicking in')]",
                     def_attack, 0, 500, 0.1)
    : *(0.001)
    : max(1/SR);

    release = hslider("Band %i:[5] Release [unit:ms] [style: slider]  [tooltip: Time constant in ms (1/e smoothing time) for the compression gain to approach (exponentially) a new higher target level (the compression 'releasing')]",
                      def_release, 0, 1000, 0.1)
    : *(0.001)
    : max(1/SR);

    gainview = compression_gain_mono(ratio,threshold,attack,release)
    : *(0.5)
    : hbargraph("Band %i:[6][1] Input Gain bargraph [tooltip: dummy tooltip]",
                0.0, 1.0);

    ingain = hslider("Band %i:[6][2] Input Gain [unit:dB]  [tooltip: The compressed-signal output level is increased by this amount (in dB) to make up for the level lost due to compression]",
                     def_ingain, -40, 40, 0.1)
    : db2linear;


    outgainview = (_,_) <: _,_,(abs,abs:+) : _,_,hbargraph("Band %i: [7] [1] Outgain",0.0,1.0) : _,attach;

    outgain = hslider("Band %i:[7][2] Output Gain [unit:dB]  [tooltip: The compressed-signal output level is increased by this amount (in dB) to make up for the level lost due to compression]",
                      def_outgain, -40, 40, 0.1)
    : db2linear;
};


split1 =  vslider("[C] Split Freq 1 [unit:Hz] [style:knob]
                  [tooltip: Center-frequency of second-order Regalia-Mitra peaking equalizer section 1]",
                  166, 40, 999, 1);

split2 =  vslider("[D] Split Freq 2 [unit:Hz] [style:knob]
                  [tooltip: Center-frequency of second-order Regalia-Mitra peaking equalizer section 1]",
                  1500, 1000, 15000, 1);

stereo_split = filterbank(3,(split1,split2));
      
comb = (stereo_split,stereo_split);



limiter = effect.bypass2(limiter_bypass, process) with{

    process = input_gain : compressor_stereo(ratio,-6,attack,release) : output_gain;

    limiter_bypass = checkbox("[E] Limiter Bypass"); // Must be named "Limiter Bypass". Used in Qt_PluginWidget.cpp and Qt_plugin_widget_callbacks.h

    input_gain    = *(ingain),*(ingain);
    ingain        = ingain_slider : ll2_db2linear;
    ingain_slider = hslider("[F] Limiter Input Gain [unit:dB]  [tooltip: Adjust overall gain.]",
                            0, -40, 40, 0.1);

    ratio   = vslider("[G] Limiter Ratio [unit: :1]",4,4,20,1);
    attack  = vslider("[H] Limiter Attack [unit: us]",800,20,800,1) / 1000000.0;
    release = vslider("[I] Limiter Release [unit: ms]",500,50,1100,1) / 1000.0;

    output_gain    = *(outgain),*(outgain);
    outgain = hslider("[J] Limiter Output Gain [unit:dB]  [tooltip: Adjust overall gain.]",
                   0, -40, 40, 0.1) : ll2_db2linear;
};

main_gain =
    hslider("[K] Final Output Gain [unit:dB]  [tooltip: Adjust overall gain.]",
            def_outgain, -40, 40, 0.1)
    : db2linear;

multiband_comp(a,b) = 
    ((a,b) : comb : (!,!,_,!,!,_) : compressor(1)) ,
    ((a,b) : comb : (!,_,!,!,_,!) : compressor(2)) ,
    ((a,b) : comb : (_,!,!,_,!,!) : compressor(3))
    :> (_,_)
    : limiter
    : *(main_gain), *(main_gain)
    ;

process = multiband_comp;
