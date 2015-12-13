
// Based on code written by Romain Michon. Found here: https://ccrma.stanford.edu/~rmichon/faustWorkshops/course2015/#tremolo
// (just made it stereo)


import("music.lib");
import("filter_smoothing.lib");

tremolo(a,b) = a*multiply, b*multiply
with{
    multiply = 1 - depth*(osc(freq)*0.5 + 0.5);
    freq = hslider("[1] frequency",5,0.1,15,0.01) : smooth(0.999);
    depth = hslider("[2] depth",0,0,1,0.01) : smooth(0.999);
};

process = tremolo;

