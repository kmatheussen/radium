import("filter.lib");
//import("fast_log_exp.dsp");

// ../bin/packages/faust2/compiler/faust -I ../bin/packages/faust2/architecture envelope_follower.dsp 

effect = library("effect.lib");

def_attack = 50.0; //.148;
def_release = 100.0; //.237;

// attack
attack = hslider("[2] Attack [unit:ms] [style:slider]",  def_attack, 0, 500, 0.1)
  : *(0.001)
  : max(1/SR);

// release
release = hslider("[3] Release [unit:ms] [style: slider]", def_release, 0, 1000, 0.1)
  : *(0.001)
  : max(1/SR);


//vumeter(x)   = attach(x, x : vbargraph("heo", 0, 1));
//attach_it(x) = attach(x, vumeter(x));

process = effect.amp_follower_ar(attack,release);// : vumeter;

