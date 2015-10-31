#
# Using Python for this was a mistake. It would have been easier to write this code in s7 scheme.
# List manipulation is not a big strength of Python.


# protracker mod:
# http://www.aes.id.au/modformat.html
#
# xm:
# ftp://ftp.modland.com/pub/documents/format_documentation/FastTracker%202%20v2.04%20%28.xm%29.html
#
# s3m:
# http://lclevy.free.fr/mo3/s3m.txt
#
# it:
# http://schismtracker.org/wiki/ITTECH.TXT


# (easier to forget) todo's:
# * get max velocity for sample instrument
# * get max counter/division value
# * bound volume values between 0 and max.
# * sample finetune
# * don't add unused samples to radium (quite messy)
# * remove old instruments before generating
# * Option to remove unused tracks.
# * Remove reltempo track
# * pad samples in kk-song are not looped.

from __future__ import division # we always want floating point division

import sys, os, filecmp
import struct, copy
import wave


if __name__ == "__main__":
    sys.g_program_path = '__main__' # hack to be able to import import_midi

import import_midi # some helper methods there



if __name__ == "__main__":
    global radium
    radium = import_midi.get_radium_mock()
else:
    import radium

# table copied from the MikMod source
numchanneldict = {
  "M.K." : 4, #protracker},	/* protracker 4 channel */
  "M!K!" : 4, #protracker},	/* protracker 4 channel */
  "FLT4" : 4, #startracker},	/* startracker 4 channel */
  "2CHN" : 2, #fasttracker},	/* fasttracker 2 channel */
  "4CHN" : 4, #fasttracker},	/* fasttracker 4 channel */
  "6CHN" : 6, #fasttracker},	/* fasttracker 6 channel */
  "8CHN" : 8, #fasttracker},	/* fasttracker 8 channel */
  "10CH" : 10, #fasttracker},	/* fasttracker 10 channel */
  "12CH" : 12, #fasttracker},	/* fasttracker 12 channel */
  "14CH" : 14, #fasttracker},	/* fasttracker 14 channel */
  "15CH" : 15, #fasttracker},	/* fasttracker 15 channel */
  "16CH" : 16, #fasttracker},	/* fasttracker 16 channel */
  "18CH" : 18, #fasttracker},	/* fasttracker 18 channel */
  "20CH" : 20, #fasttracker},	/* fasttracker 20 channel */
  "22CH" : 22, #fasttracker},	/* fasttracker 22 channel */
  "24CH" : 24, #fasttracker},	/* fasttracker 24 channel */
  "26CH" : 26, #fasttracker},	/* fasttracker 26 channel */
  "28CH" : 28, #fasttracker},	/* fasttracker 28 channel */
  "30CH" : 30, #fasttracker},	/* fasttracker 30 channel */
  "32CH" : 32, #fasttracker},	/* fasttracker 32 channel */
  "CD81" : 8, #oktalyzer},	/* atari oktalyzer 8 channel */
  "OKTA" : 8, #oktalyzer},	/* atari oktalyzer 8 channel */
  "16CN" : 16, #taketracker},	/* taketracker 16 channel */
  "32CN" : 32, #taketracker},	/* taketracker 32 channel */
  "    " : 4 #ins15tracker}	/* 15-instrument 4 channel */
}

# table copied from the MikMod sound library (last two rows) and the soundtracker source code (the linux program called "soundtracker", not the ultimate soundtracker)
note_to_period_table = [1712,1616,1524,1440,1356,1280,1208,1140,1076,1016, 960, 906,
    856, 808, 762, 720, 678, 640, 604, 570, 538, 508, 480, 453,
    428, 404, 381, 360, 339, 320, 302, 285, 269, 254, 240, 226,
    214, 202, 190, 180, 170, 160, 151, 143, 135, 127, 120, 113,
    107, 101,  95,  90,  85,  80,  75,  71,  67,  63,  60,  56,

    53, 50, 47, 45, 42, 40, 37, 35, 33, 31, 30, 28,
    27, 25, 24, 22, 21, 20, 19, 18, 17, 16, 15, 14
]

table_start_note = 24
table_end_note = table_start_note + len(note_to_period_table)
first_period = note_to_period_table[0]
last_period = note_to_period_table[-1]

def scale(x,x1,x2,y1,y2):
    return y1 + ( ((x-x1)*(y2-y1))
                  /
                  (x2-x1)
                )

def boundaries(minval, val, maxval):
    if val < minval:
        return minval
    elif val > maxval:
        return maxval
    else:
        return val
    
def period_to_note(period):
    prev_period = first_period

    if period > prev_period:
        return boundaries(0.001, scale(period, 1712, 8192, table_start_note, 0), 127.999)

    for note in range(1, len(note_to_period_table)):
        next_period = note_to_period_table[note]
        if period >= next_period:
            return table_start_note + scale(period, prev_period, next_period, note-1, note)
        prev_period = next_period

    return boundaries(0.001, scale(period, last_period, 0, table_end_note, 128), 127.999)


class Velocity:
    def __init__(self, linenum, value, do_glide):
        self.linenum = linenum
        self.value = value
        self.do_glide = do_glide

class Note:
    def __init__(self, tracknum, linenum, samplenum, notenum):
        self.mod_tracknum = tracknum
        self.radium_tracknum = -1 # assigned later
        self.linenum = linenum
        self.end_linenum = -1
        self.samplenum = samplenum
        self.notenum = notenum
        self.velocities = []
        is_left = ((tracknum-1)//2 % 2) == 1
        if is_left:
            self.pan = -1
        else:
            self.pan = 1
        self.effects = []

#    def print_note(self):
        
    # todo: Convert velocity values from 0-64 range to radium range
    def generate(self, pattern):
        
        if self.linenum==self.end_linenum: # happens if note starts with volume 0.
            return

        # 1. find end place
        if self.end_linenum == pattern.num_lines:
            end_line = self.end_linenum-1
            end_counter = 65534-1
            end_dividor = 65534
        else:
            end_line = self.end_linenum
            end_counter = 0
            end_dividor = 1

        # 1. create note
        radium_notenum = radium.addNote2(
            self.notenum, self.velocities[0].value * (65536 // 64),
            self.linenum, 0, 1,
            end_line, end_counter, end_dividor,
            -1, -1, self.radium_tracknum
        )
        
        # 2. set end note velocity (if necessary)
        if len(self.velocities) > 0:
            radium.setVelocity(1, self.velocities[-1].value / 64, end_line + (end_counter/end_dividor), radium_notenum, self.radium_tracknum)

        # 3. add velocities
        for velocity in self.velocities:
            if velocity.linenum > self.linenum and velocity.linenum < self.end_linenum:
                radium.createVelocity(velocity.value / 64, velocity.linenum, radium_notenum, self.radium_tracknum)

    def legalize_velocities(self):
        pass

    def set_end_note_based_on_velocities(self):
        last_end_line = -1

        for velocity in self.velocities:
            print("velocity",velocity.linenum,velocity.value)
            if velocity.value==0:
                if last_end_line == -1:
                    last_end_line = velocity.linenum
            else:
                last_end_line = -1

        if last_end_line != -1:
            self.end_linenum = last_end_line

    # todo:
    # * End note at last velocity with value 0
    # * Convert values to 0-1
    # * Ensure velocities doesn't go below 0 or above 64
    # * Add velocity nodes for volume effects
    # * Move velocity nodes that are placed on top of another velocity node to right before that next velocity node
    # * last note can't end at line 64, but right before
    def add_velocities_from_mod_effects(self, pattern, samples):
        print "len / num",len(samples),self.samplenum
        #if self.samplenum==31:
        #    self.samplenum=30

        sample = samples[self.samplenum]
        last_volume = sample.volume

        self.velocities.append(Velocity(self.linenum, last_volume, False))

        for effect in self.effects:
            if effect.effectnum==12 or (effect.effectnum==14 and (effect.value1==10 or effect.value1==11)):
                if effect.effectnum==14 and effect.value1==10:
                    last_volume = last_volume + effect.value2
                elif effect.effectnum==14 and effect.value1==11:
                    last_volume = last_volume - effect.value2
                else:
                    last_volume = effect.value

                if last_volume < 0:
                    last_volume = 0
                elif last_volume > 64: # happens, even for 0xc0 commands. Mikmod calls them ""heavy" volumes" and sets them to 0x40.
                    last_volume = 64

                if effect.linenum==self.linenum:
                    assert(len(self.velocities)==1)
                    self.velocities[0].value = last_volume # i.e. use custom value for note
                else:
                    self.velocities.append(Velocity(effect.linenum, last_volume, False))

            elif effect.effectnum==10 or effect.effectnum==5 or effect.effectnum==6:
                if effect.value1 > 0:
                    value = effect.value1
                else:
                    value = -effect.value2
                next_volume = last_volume + (value * (pattern.get_tpd(effect.linenum) - 1))
                self.velocities.append(Velocity(effect.linenum, last_volume, False))
                self.velocities.append(Velocity(effect.linenum+1, next_volume, True))
                last_volume = next_volume

        self.velocities.append(Velocity(self.end_linenum, last_volume, False))

        self.set_end_note_based_on_velocities()

        self.legalize_velocities()

        print("velo",self.velocities)

    def printit(self):
        print str(self.samplenum) + ": " + str(self.notenum)


class Effect:
    def __init__(self, tracknum, linenum, effectnum, value):
        self.mod_tracknum = tracknum
        self.linenum = linenum
        self.effectnum = effectnum
        self.value = value
        self.value1 = (value & 0xf0) >> 4
        self.value2 = value & 0x0f

class Tempo:
    def __init__(self, linenum, bpm):
        self.linenum = linenum
        self.bpm = bpm

    def generate(self, last_lpb):
        if self.bpm > 999: # bpm can be between 32 and 1530, while radium has a bpm range of 1-999. Need to use higher lpb for bpm above 999.
            if last_lpb != 8:
                radium.addLPB(8, self.linenum, 0, 1) 
            radium.addBPM(int(round(self.bpm/2)), self.linenum, 0, 1)
            return 8
        else:
            if last_lpb != 4:
                radium.addLPB(4, self.linenum, 0, 1)
            radium.addBPM(int(round(self.bpm)), self.linenum, 0, 1)
            return 4

class ModSpeed:
    def __init__(self, linenum, value):
        self.linenum = linenum
        if value==0:
            self.value = 1
        else:
            self.value = value

    def is_tpd(self):
        return self.value <= 32

    def is_bpm(self):
        return not self.is_tpd()

'''
[15]: Set speed
     Where [15][x][y] means "set speed to x*16+y". Though it is nowhere
     near that simple. Let z = x*16+y. Depending on what values z takes,
     different units of speed are set, there being two: ticks/division
     and beats/minute (though this one is only a label and not strictly
     true). If z=0, then what should technically happen is that the
     module stops, but in practice it is treated as if z=1, because
     there is already a method for stopping the module (running out of
     patterns).

     If z<=32, then it means "set ticks/division to z"

     If z>32 it means "set beats/minute to z" (convention says that
     this should read "If z<32.." but there are some composers out there
     that defy conventions).

     Default values are:
         6 ticks/division, and
         125 beats/minute (4 divisions = 1 beat).

     ***********************************************************************
     **** The beats/minute tag is only meaningful for 6 ticks/division. ****
     ***********************************************************************

     To get a more accurate view of how things work, use the following formula:

                             24 * beats/minute
          divisions/minute = -----------------
                              ticks/division

     Hence divisions/minute range from 24.75 to 6120, eg. to get a value
     of 2000 divisions/minute use 3 ticks/division and 250 beats/minute.

       Seems to work like this:

          Radium LPB = 4

          tempo_A = beats/minute
          tempo_B = ticks/division

                          24 * tempo_A / tempo_B             6 * tempo_A
          Radium BPM  =   ----------------------    =   ------------------------
                                   4                           tempo_B

     If multiple "set speed" effects are performed in a single division,
     the ones on higher-numbered channels take precedence over the ones
     on lower-numbered channels. This effect has a large number of
     different implementations, but the one described here has the
     widest usage.
'''

mod_bpm = 125 # beats per minute when tpd==6 (LPB (lines per beat) is always 4)
mod_tpd = 6 # ticks per division ("division" means line)

def reset_bpm():
    global mod_bpm, mod_tpd

    mod_bpm = 125
    mod_tpd = 6

def get_bpm(speed):
    global mod_bpm, mod_tpd

    if value <= 32:
        mod_tpd = value
    else:
        mod_bpm = value

    return int(round(6 * mod_bpm / mod_tpd))

def get_bpm_from_mod_speed(mod_bpm, mod_tpd):
    return int(round(6 * mod_bpm / mod_tpd))

class Pattern:
    def __init__(self, patternnum, num_channels, name):
        self.num_channels = num_channels
        self.num_radium_tracks = -1 # set later
        self.patternnum = patternnum
        self.name = name
        self.notes = []
        self.effects = []
        self.mod_speeds = []
        self.tempos = [] # generated after parsing from the mod_speeds values.
        self.lastNotes = {} # Used during parsing. tracknum is key
        self.num_lines = 64
        self.bpm_from_previous_pattern = -1
        self.tpd_from_previous_pattern = -1

    def set_note_endlines(self):
        for note in self.notes:
            if note.end_linenum==-1:
                note.end_linenum = self.num_lines

    def add_note_velocities_from_mod_effects(self, samples):
        for note in self.notes:
            note.add_velocities_from_mod_effects(self, samples)

    def has_last_note(self, tracknum):
        return self.lastNotes.has_key(tracknum)

    def last_note(self, tracknum):
        return self.lastNotes[tracknum]

    def add_note(self, note):
        tracknum = note.mod_tracknum

        # first set end line for previous note
        if self.has_last_note(tracknum):
            last_note = self.last_note(tracknum)
            last_note.end_linenum = note.linenum

        self.lastNotes[tracknum] = note
        self.notes.append(note)

    def add_tempo(self, tempo):
        self.tempos.append(tempo)

    def add_mod_speed(self, mod_speed):
        self.mod_speeds.append(mod_speed)

    # this is ugly. python lacks proper list traversing pattern matching and proper recursive support
    def remove_redundant_mod_speeds(self):
        new_mod_speeds = []
        last_bpm = None
        last_tpd = None

        for mod_speed in self.mod_speeds:
            if mod_speed.is_tpd() and last_tpd and mod_speed.linenum==last_tpd.linenum:
                    last_tpd.value = mod_speed.value
            elif mod_speed.is_bpm() and last_bpm and mod_speed.linenum==last_bpm.linenum:
                    last_bpm.value = mod_speed.value
            else:
                new_mod_speeds.append(mod_speed)

        self.mod_speeds = new_mod_speeds

    def __get_ending_mod_speed(self, start_mod_bpm, start_mod_tpd):
        end_mod_bpm = start_mod_bpm
        end_mod_tpd = start_mod_tpd

        for mod_speed in self.mod_speeds:
            if mod_speed.is_tpd():
                end_mod_tpd = mod_speed.value
            else:
                end_mod_bpm = mod_speed.value

        return end_mod_bpm, end_mod_tpd
            

    def add_tempos_from_mod_speeds(self, song, start_mod_bpm, start_mod_tpd):
        if self.bpm_from_previous_pattern != -1:

            assert(len(self.tempos) > 0)
            assert(self.tpd_from_previous_pattern != -1)

            # 'add_tempos_from_mod_speeds' has been called on this pattern before

            for mod_speed in self.mod_speeds:
                if mod_speed.linenum==0:
                    if mod_speed.is_tpd():
                        start_mod_tpd = mod_speed.value
                    else:
                        start_mod_bpm = mod_speed.value
                else:
                    break

            first_tempo = Tempo(0, get_bpm_from_mod_speed(start_mod_bpm, start_mod_tpd))
            old_first_tempo = self.tempos[0]

            if first_tempo.bpm != old_first_tempo.bpm: # If a pattern is played in a different tempo than earlier in the playlist, we need to make a new pattern
                copied_pattern = copy.deepcopy(self)
                copied_pattern.patternnum = len(song.patterns)
                copied_pattern.name = copied_pattern.name + " (new tempo)"
                copied_pattern.tempos = []
                copied_pattern.tpd_from_previous_pattern = -1
                copied_pattern.bpm_from_previous_pattern = -1

                song.patterns.append(copied_pattern)
                return copied_pattern.add_tempos_from_mod_speeds(song, start_mod_bpm, start_mod_tpd)

        else:

            assert(len(self.tempos) == 0)
            assert(self.tpd_from_previous_pattern == -1)

            self.bpm_from_previous_pattern = start_mod_bpm
            self.tpd_from_previous_pattern = start_mod_tpd

            tempos = {} # key is line, value is tempo. Stored in a dictionary to avoid several tempos on the same line

            tempos[0] = Tempo(0, get_bpm_from_mod_speed(start_mod_bpm, start_mod_tpd)) # all patterns is going to have a tempo at line 0

            current_mod_bpm = start_mod_bpm
            current_mod_tpd = start_mod_tpd

            # add tempos
            for mod_speed in self.mod_speeds:
                if mod_speed.is_tpd():
                    current_mod_tpd = mod_speed.value
                else:
                    current_mod_bpm = mod_speed.value

                tempos[mod_speed.linenum] = Tempo(mod_speed.linenum, get_bpm_from_mod_speed(current_mod_bpm, current_mod_tpd))

            # convert tempos stored in a dictionary to tempos stored in a list
            for linenum in range(self.num_lines):
                if tempos.has_key(linenum):
                    self.tempos.append(tempos[linenum])

        ending_bpm, ending_tpd = self.__get_ending_mod_speed(start_mod_bpm, start_mod_tpd)
        print "ending "+str(self.patternnum)+": ",start_mod_bpm, ending_bpm, " --- ", start_mod_tpd,ending_tpd
        return self.patternnum, ending_bpm, ending_tpd
                

    def get_tpd(self, linenum):
        tpd = self.tpd_from_previous_pattern

        for mod_speed in self.mod_speeds:
            if mod_speed.linenum <= linenum:
                if mod_speed.is_tpd():
                    tpd = mod_speed.value
            else:
                break

        return tpd

    def assign_radium_tracknum_to_notes(self, song):
        curr_tracknum = 0
        
        for mod_tracknum in range(self.num_channels):
            assigned_tracks = {} # samplenum is key, radium_tracknum is value

            for note in self.notes:
                samplenum = note.samplenum
                
                if note.mod_tracknum==mod_tracknum:
                    if assigned_tracks.has_key(samplenum):
                        note.radium_tracknum = assigned_tracks[samplenum]
                    else:
                        note.radium_tracknum = curr_tracknum
                        assigned_tracks[samplenum] = curr_tracknum
                        curr_tracknum += 1

        return curr_tracknum

    def __get_radium_track_note(self, tracknum):
        for note in self.notes:
            if note.radium_tracknum==tracknum:
                return note
        return None
        
    def set_radium_track_pans(self, song):            
        for tracknum in range(self.num_radium_tracks):
            note = self.__get_radium_track_note(tracknum)
            if note:
                samplenum = note.samplenum
                sample = song.samples[samplenum]
                if sample.instrument_num != -1:
                    radium.setInstrumentForTrack(sample.instrument_num, tracknum)
                radium.setTrackPan(note.pan, tracknum)
                radium.setTrackPanOnOff(True, tracknum)
            

    def generate(self, song):
        radium.setNumLines(self.num_lines)

        self.num_radium_tracks = self.assign_radium_tracknum_to_notes(song)

        if self.num_radium_tracks==0:
            self.num_radium_tracks=1

        radium.setNumTracks(self.num_radium_tracks)

        radium.minimizeBlockTracks()                        

        self.set_radium_track_pans(song)

        #radium.setNumTracks(self.num_channels)

        #for tracknum in range(self.num_channels):
        #    is_left = ((tracknum-1)//2 % 2) == 1
        #    if is_left:
        #        pan = -1
        #    else:
        #        pan = 1
        #    radium.setTrackPan(pan, tracknum)
        #    radium.setTrackPanOnOff(True, tracknum)

        #assigned = {}
        #for note in self.notes:
        #    tracknum = note.tracknum
        #    if not assigned.has_key(tracknum):
        #        sample = song.samples[note.samplenum]
        #        radium.setInstrumentForTrack(sample.instrument_num, tracknum)
        #        assigned[tracknum] = True

        print "len tempos: ", len(self.tempos)
        last_lpb = 4
        for tempo in self.tempos:
            last_lpb = tempo.generate(last_lpb)

        for note in self.notes:
            note.generate(self)

    def add_effect(self, effect):
        if self.lastNotes.has_key(effect.mod_tracknum):
            note = self.lastNotes[effect.mod_tracknum]
            note.effects.append(effect)
        else:
            self.effects.append(effect)

def signed_char_to_unsigned_char(c):
    s = unpack(c, 'int8')
    return chr(s+128)

def find_free_wav_filename(base_filename):
    filename = base_filename+".wav"
    num = 2
    similars = []

    while os.path.exists(filename):
        similars.append(filename)
        filename = base_filename + str(num) + ".wav"
        num += 1

    return similars,filename

def clean_filename_char(c):
    if c.isalnum():
        return c
    elif c=="_" or c=="-" or c==".":
        return c
    else:
        return str(ord(c))

def clean_filename(name):
    clean = map(clean_filename_char, name)
    return "".join(clean)

class Sample:
    def __init__(self, name, num_samples, finetune, volume, loop_start, loop_length):
        if name=="":
            self.name = "noname"
        else:
            self.name = name
        self.num_samples = num_samples
        self.finetune = finetune
        self.volume = volume
        self.loop_start = loop_start
        self.loop_length = loop_length
        self.filename = ""
        self.instrument_num = -1

    def num_bytes(self):
        return self.num_samples

    def generate(self):
        if self.filename != "" and self.num_samples>0:
            self.instrument_num = radium.createAudioInstrument("Sample Player","Sample Player", self.name)
            radium.setInstrumentSample(self.instrument_num, self.filename)
            if self.loop_length>0:
                radium.setInstrumentLoopData(self.instrument_num, self.loop_start, self.loop_length)
            if self.finetune!=0:
                print "setInstrumentEffect, orgval: ",self.finetune
                #radium.showMessage("setInstrumentEffect, orgval: "+str(self.finetune)+", instrument num: "+str(self.instrument_num))
                radium.setInstrumentEffect(self.instrument_num, "Finetune", scale(self.finetune,-8,7,0.25,0.75))

    def save(self, file, pos):
        homedir = os.path.expanduser("~") # supposed to work on windows too, according to the internet
        base_dir = os.path.join(homedir, ".radium", "mod_samples")

        if not os.path.exists(base_dir):
            os.makedirs(base_dir)

        base_filename = os.path.join(base_dir, clean_filename(self.name))

        similars,filename = find_free_wav_filename(base_filename)

        file.seek(pos)
        print "pos before",file.tell()
        assert(file.tell()==pos)

        print "name:", self.name, ", num_bytes: ",self.num_bytes(), "loop start/length:",self.loop_start,self.loop_length

        data = file.read(self.num_bytes())
        print "pos after",file.tell()
        num_bytes_read = file.tell() - pos

        if num_bytes_read != self.num_bytes():
            print "wrong ("+self.name+"): ", num_bytes_read, "!=", self.num_bytes(), ", POS:", file.tell()
            assert(False)

        wavfile = wave.open(filename, "w")

        wavfile.setnchannels(1)
        wavfile.setsampwidth(1)
        wavfile.setframerate(8287)

        unsigned_bytes = map(signed_char_to_unsigned_char, data)
        wavfile.writeframes("".join(unsigned_bytes))

        wavfile.close()

        for similar in similars:
            if filecmp.cmp(similar, filename):
                os.remove(filename)
                filename = similar
                break

        self.filename = filename


class Playlist:
    def __init__(self, patternnums):
        self.patternnums = patternnums
    
    def number_of_patterns(self):
        highest = 0
        for patternnum in self.patternnums:
            if patternnum > highest:
                highest = patternnum
        return highest+1

    def generate(self):
        radium.setPlaylistLength(len(self.patternnums))
        pos = 0
        for patternnum in self.patternnums:
            radium.setPlaylistBlock(pos, patternnum)
            pos += 1

class Song:
    def __init__(self, name, patterns, samples, playlist):
        self.name = name
        self.patterns = patterns
        self.samples = samples
        self.playlist = playlist

    def add_note_velocities_from_mod_effects(self):
        for pattern in self.patterns:
            pattern.add_note_velocities_from_mod_effects(self.samples)

    def add_tempos_from_mod_speeds(self):
        mod_bpm, mod_tpd = (125, 6)

        for pattern in self.patterns:
            pattern.remove_redundant_mod_speeds()

        new_playlist_patternnums = []

        # first generate those patterns that belongs to a playlist. We know more about tempo here.
        for patternnum in self.playlist.patternnums:
            pattern = self.patterns[patternnum]
            new_patternnum, mod_bpm, mod_tpd = pattern.add_tempos_from_mod_speeds(self, mod_bpm, mod_tpd)
            new_playlist_patternnums.append(new_patternnum)

        # then generate tempos in the other patterns. We don't know previous tempo here, so we just set it to the default values 125,6
        for pattern in self.patterns:
            if not pattern.tempos:
                pattern.add_tempos_from_mod_speeds(self, 125, 6)

        self.playlist = Playlist(new_playlist_patternnums)

    def generate(self):
        import_midi.clear_radium_editor()

        for sample in self.samples:
            sample.generate()

        radium.setLPB(4)   # Deafult mod value
        radium.setBPM(125) # Default mod value

        for pattern in self.patterns:
            pattern.generate(self)
            radium.appendBlock()

        self.playlist.generate()

# copied from http://code.activestate.com/recipes/577610-decoding-binary-files/ (Yony Kochinski)
typeNames = {
    'int8'   :'b',
    'uint8'  :'B',
    'int16'  :'h',
    'uint16' :'H',
    'int32'  :'i',
    'uint32' :'I',
    'int64'  :'q',
    'uint64' :'Q',
    'float'  :'f',
    'double' :'d',
    'char'   :'s'
}

def unpack(data, typename):
    return struct.unpack(typeNames[typename], data)[0]

def read_uint8(file, start):
    file.seek(start)
    c = file.read(1)
    return unpack(c, 'uint8')

def read_bigendian16(file, start):
    b1 = read_uint8(file, start)
    b2 = read_uint8(file, start+1)
    return (b1 << 8) + b2

def read_string(file, start, length):
    file.seek(start)
    ret = ""
    pos = 0
    while pos < length:
        c = file.read(1)
        w = unpack(c, 'uint8')
        if (w==0):
            break
        ret += c
        pos += 1
        #print str(pos)+": c: " + str(c) + " - " + str(w)
    return ret

def read_sample(file, pos):
    name = read_string(file, pos, 22)
    pos += 22

    num_samples = read_bigendian16(file, pos) * 2
    pos += 2

    finetune = read_uint8(file, pos) & 0x0f
    if finetune>=8:
        finetune -= 16
    pos += 1

    volume = read_uint8(file, pos)
    pos += 1

    loop_start = read_bigendian16(file, pos) * 2
    pos += 2

    loop_length = read_bigendian16(file, pos) * 2
    if loop_length==2: # mod peculiarity
        loop_length = 0

    pos += 2

    return Sample(name, num_samples, finetune, volume, loop_start, loop_length)


def read_samples(file, pos):
    samples = []
    for i in range(15):
        samples.append(read_sample(file, pos))
        pos += 30

    fourletters = read_string(file, pos+130, 4)
    print "fourletters1: "+fourletters

    if fourletters == "" or True: # 4th Revision says: "If no letters are here, then this is the start of the pattern data, and only 15 samples were present.". However, that doesn't seem to always be true (data could for instance hit the name of a sample). I don't see any good way to detect it if there are only 15 samples present, and neither SoundTracker nor MikMod seems to care about #15 sample files, so we just assume there are always 31 instruments.
        for i in range(16):
            samples.append(read_sample(file, pos))
            pos += 30
    
    return (samples, pos)


def read_trackline(file, pattern, tracknum, linenum, pos):
    '''
    from http://www.aes.id.au/modformat.html :

    7654-3210 7654-3210 7654-3210 7654-3210
    wwww xxxxxxxxxxxxxx yyyy zzzzzzzzzzzzzz

    wwwwyyyy (8 bits) is the sample for this channel/division
    xxxxxxxxxxxx (12 bits) is the sample's period (or effect parameter)
    zzzzzzzzzzzz (12 bits) is the effect for this channel/division
    '''
    byte1 = read_uint8(file, pos)
    byte2 = read_uint8(file, pos+1)
    byte3 = read_uint8(file, pos+2)
    byte4 = read_uint8(file, pos+3)

    #print(hex(byte1))
    #print(hex(byte3))
    
    samplenum = (byte1 & 0xf0) + (byte3 >> 4)
    if samplenum > 0:
        period = ((byte1 & 0x0f) << 8) + byte2
        if period==0:
            if pattern.has_last_note(tracknum):
                notenum = pattern.last_note(tracknum).notenum
            else:
                notenum = 0
        else:
            notenum = period_to_note(period)

        if notenum > 0:
            note = Note(tracknum, linenum, samplenum-1, notenum)
            pattern.add_note(note)
        
            note.printit()
#        print "period: "+str(period)
#        print "samplenum: "+str(samplenum)

    effectnum = byte3 & 0x0f
    effectvalue = byte4

    if effectnum>0:
        print "effect: "+str(effectnum)+", "+str(effectvalue)

    if effectnum==15:
        pattern.add_mod_speed(ModSpeed(linenum, effectvalue))

    elif effectnum==11 or effectnum==13:
        pattern.num_lines = linenum+1

    elif effectnum>0:
        pattern.add_effect(Effect(tracknum, linenum, effectnum, effectvalue))


def read_pattern(file, num_channels, patternnum, pos):
    pattern = Pattern(patternnum, num_channels, "Pattern "+str(patternnum))

    for linenum in range(64):
        if linenum == pattern.num_lines:
            break

        for ch in range(num_channels):
            print
            print "******************* line "+str(linenum)+", track "+str(ch)
            read_trackline(file, pattern, ch, linenum, pos)
            pos += 4

    pattern.set_note_endlines()

    return pattern


def read_patterns(file, num_channels, num_patterns, pos):
    patterns = []

    for patternnum in range(num_patterns):
        pattern = read_pattern(file, num_channels, patternnum, pos)
        patterns.append(pattern)
        pos += 64 * 4 * num_channels

    return (patterns, pos)


def read_playlist(file, pos):
    song_length = read_uint8(file, pos)
    print "song length: "+str(song_length)

    pos += 2

    playlist = []
    for i in range(song_length):
        print str(i)+": "+str(read_uint8(file, pos+i))
        playlist.append(read_uint8(file, pos+i))

    print "playlist: "+str(playlist)
    return Playlist(playlist)

    
def read_song(file):
    reset_bpm()

    pos = 0
    name = read_string(file, pos, 20)
    pos += 20

    (samples, pos) = read_samples(file, pos)

    playlist = read_playlist(file, pos)

    print "number of patterns: "+str(playlist.number_of_patterns())
    pos += 130

    fourletters = read_string(file, pos, 4)
    print "fourletters2: "+fourletters
    pos += 4

    numchannels = numchanneldict[fourletters]

    print "POS: "+hex(pos)

    (patterns, pos) = read_patterns(file, numchannels, playlist.number_of_patterns(), pos)

    print(repr(patterns))
    print("num_patterns: ",len(patterns))
    print("playlist",playlist.patternnums)

    for sample in samples:
        sample.save(file, pos)
        pos += sample.num_bytes()

    song = Song(name, patterns, samples, playlist)

    return song


def generate_from_mod(song):
    song.add_tempos_from_mod_speeds()

    song.add_note_velocities_from_mod_effects()

    song.generate()



def import_mod(filename=""):
    #file = open("workerstecnopop3.mod", "rb")
    #file = open("/home/kjetil/Downloads/temp/NIAGRA.MOD", "rb")
    #file = open("/home/kjetil/Downloads/GODZILLA.MOD", "rb")
    file = open("/home/kjetil/Downloads/hoffman_and_daytripper_-_professional_tracker.mod", "rb")
    #file = open("/home/kjetil/Downloads/hoisaga1.mod", "rb")
    #file = open("/home/kjetil/Downloads/knulla-kuk.mod", "rb")
    #file = open("/home/kjetil/Downloads/DOPE.MOD", "rb")
    #file = open("/home/kjetil/Downloads/velcoitytest.mod", "rb")
 
    song = read_song(file)

    generate_from_mod(song)


if __name__ == "__main__":
    #file = open("workerstecnopop3.mod", "rb")
    #file = open("/home/kjetil/Downloads/1990_mix.mod", "rb")
    file = open("/home/kjetil/Downloads/velcoitytest.mod", "rb")
    file = open("/home/kjetil/Downloads/GODZILLA.MOD", "rb")
    file = open("/home/kjetil/Downloads/knulla-kuk.mod", "rb")
    #file = open("/home/kjetil/Downloads/DOPE.MOD", "rb")

    song = read_song(file)

    generate_from_mod(song)


