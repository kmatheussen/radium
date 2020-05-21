"""
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
"""


import sys,os
import platform

#sys.setrecursionlimit(1500)

class NullWriter(object):
    def write(self, value): pass


if __name__ == "__main__" or sys.g_program_path=='__main__':
    sys.path.append("python-midi")
else:
    sys.path.append(os.path.join(sys.g_program_path,"python-midi"))
    if platform.system() != "Linux" and os.isatty(sys.stdout.fileno()):
        sys.stdout = sys.stderr = NullWriter()

import src as midi

import fractions

class RadiumMock:
    def addNote3(self,
                notenum, velocity,
                line, counter, dividor,
                end_line, end_counter, end_dividor,
                tracknum, blocknum, windownum):
#        return
        print "addNote3",tracknum,line,counter,dividor,end_line,end_counter,end_dividor,notenum,velocity
    def setMainLPB(self,lpb):
        pass
    def setMainBPM(self,pbm):
        pass
    def dummy(self,*args):
        pass
    def addLPB3(self, lpb, linenum, counter, dividor):
        print "addLPB3", str(linenum) + ": "+hex(lpb)
    def addBPM3(self, bpm, linenum, counter, dividor):
        print "addBPM3", str(linenum) + ": "+hex(bpm)
    def addVelocityF(self, value, floatplace, notenum, tracknum):
        print "   addVelocityF",floatplace,value

    def setPlaylistBlock(self, pos, blocknum):
        print "Playlist ",pos,blocknum

    def setInstrumentEffect(self, instrument_num, effect_name, value):
        print "setInstrumentEffect ",instrument_num,effect_name,value

    def addMessage(self, message):
        print "MESSAGE: "+message

    def getNumVelocities(self, *args):
        return 2


def get_radium_mock():
    radium = RadiumMock()
    radium.addNote3 = radium.addNote3
    radium.setMainLPB = radium.setMainLPB
    radium.setMainBPM = radium.setMainBPM
    radium.setNumLines = radium.dummy
    radium.setNumTracks = radium.dummy
    radium.openRequester = radium.dummy
    radium.closeRequester = radium.dummy
    radium.addSignature = radium.dummy
    radium.addSignature3 = radium.dummy
    radium.createMIDIInstrument = radium.dummy
    radium.createAudioInstrument = radium.dummy
    radium.setInstrumentSample = radium.dummy
    radium.setInstrumentLoopData = radium.dummy
    radium.setInstrumentData = radium.dummy
    radium.setInstrumentForTrack = radium.dummy
    radium.getNumBlocks = radium.dummy
    radium.appendBlock=radium.dummy
    radium.selectPrevBlock=radium.dummy
    radium.deleteBlock=radium.dummy
    radium.setTrackVolume = radium.dummy
    radium.setTrackPan = radium.dummy
    radium.setTrackPanOnOff = radium.dummy
    radium.setPlaylistLength = radium.dummy
    radium.minimizeBlockTracks = radium.dummy
    radium.setVelocityF = radium.dummy
    radium.newSong = radium.dummy
    radium.resetUndo = radium.dummy
    return radium

if __name__ == "__main__" or sys.g_program_path=='__main__':
    radium = get_radium_mock()
else:
    import radium

class Note:
    def __init__(self, start_tick, channel=0, notenum=64, velocity=100, end_tick=-1):
        self.start_tick = start_tick
        self.end_tick = end_tick
        self.channel = channel
        self.notenum = notenum
        self.velocity = velocity

    def has_end_tick(self):
        return self.end_tick >= 0

    def has_legal_end_tick(self):
        return self.has_end_tick() and self.end_tick > self.start_tick

    def set_end(self, end_tick, end_velocity):
        self.end_tick = end_tick
        self.end_velocity = end_velocity

    def printit(self):
        print self.start_tick, self.end_tick

def is_overlapping(a, b):
    if b.start_tick < a.start_tick:
        return is_overlapping(b, a)
    elif not a.has_end_tick():
        return True
    else:
        return b.start_tick < a.end_tick

if 0:
    print True, is_overlapping(Note( 50,end_tick=100),Note( 50,end_tick=60))
    print True, is_overlapping(Note( 50,end_tick=60), Note( 50,end_tick=100))
    print False,is_overlapping(Note( 50,end_tick=100),Note(100,end_tick=100))
    print True, is_overlapping(Note( 50,end_tick=-1), Note(100,end_tick=-1))
    print True, is_overlapping(Note(100,end_tick=-1), Note( 50,end_tick=-1))
    print True, is_overlapping(Note(100,end_tick=-1), Note(100,end_tick=-1))
    sys.exit(0)


#Unfortunately, python has a very low limit on recursion.
"""
def extract_polyphonic_notes_0(overlap_end, sequence):
    if sequence==[]:
        return []
    
    first_note = sequence[0]
    if first_note.start_tick < overlap_end:
        overlap_end = max(overlap_end, first_note.end_tick)
        return [first_note] + extract_polyphonic_notes_0(overlap_end, sequence[1:])

    if len(sequence)==1:
        return []

    second_note = sequence[1]
    if is_overlapping(first_note, second_note):
        overlap_end = max(first_note.end_tick, second_note.end_tick)
        return [first_note, second_note] + extract_polyphonic_notes_0(overlap_end, sequence[2:])

    else:
        return extract_polyphonic_notes_0(0, sequence[1:])
"""

# Ugly workaround:
def extract_polyphonic_notes_0(overlap_end, sequence):
    ret = []

    while len(sequence)>0:    
        first_note = sequence[0]
        if first_note.start_tick < overlap_end:
            overlap_end = max(overlap_end, first_note.end_tick)
            ret = [first_note] + ret
            sequence = sequence[1:]

        elif len(sequence)==1:
            break

        else:
            second_note = sequence[1]
            if is_overlapping(first_note, second_note):
                overlap_end = max(first_note.end_tick, second_note.end_tick)
                ret = [first_note, second_note] + ret
                sequence = sequence[2:]

            else:
                sequence = sequence[1:]
                overlap_end = 0

    return ret


def extract_polyphonic_notes(sequence):
    return extract_polyphonic_notes_0(0, sequence)


def find_lowest_pitched_note(notes):
    lowest = notes[0]
    for note in notes[1:]:
        if note.notenum < lowest.notenum:
            lowest = note
    return lowest


def remove_overlapping_notes_from_sequence(overlapping_note, sequence):
    result = []
    for n in sequence:
        if n==overlapping_note or not is_overlapping(n, overlapping_note):
            result.append(n)
    return result

if 0:
    notea          = Note(0,0,0,0)
    notea.end_tick = 10
    noteb          = Note(5,0,0,0)
    noteb.end_tick = 15
    print map(lambda n:n.start_tick, remove_overlapping_notes_from_sequence(noteb, [notea, noteb]))
    sys.exit(0)


def extract_monophonic_sequence(sequence):
    polyphonic_notes = extract_polyphonic_notes(sequence)
    if polyphonic_notes==[]:
        return sequence
    else:
        lowest_polyphonic_note = find_lowest_pitched_note(polyphonic_notes)
        improved_sequence      = remove_overlapping_notes_from_sequence(lowest_polyphonic_note, sequence)
        return extract_monophonic_sequence(improved_sequence)

    
def remove_notes_from_sequence(notes, sequence):
    return filter(lambda n:n not in notes, sequence)


def polyphonic_sequence_to_monophonic_sequences(sequence):
    if sequence==[]:
        return []
    else:
        monophonic_sequence = extract_monophonic_sequence(sequence)
        rest_sequence       = remove_notes_from_sequence(monophonic_sequence, sequence)
        return [monophonic_sequence] + polyphonic_sequence_to_monophonic_sequences(rest_sequence)


class Track:
    def __init__(self,
                 channel = -1,
                 preset = 0,
                 notes = [],
                 events = []):
        self.channel      = channel
        self.notes        = notes
        self.events       = events
        self.other_events = []


class Instrument:
    def __init__(self, channel, preset, name, tick):
        self.channel = channel
        self.preset  = preset
        self.name    = name + (" [%d/%d]" % (channel+1,preset+1))
        if self.name=="":
            self.name = "ch "+str(channel)
        self.tick    = tick
        
class Instruments:
    def __init__(self):
        self.current_channel         = -1
        self.current_instrument_name = ""
        self.default_instruments     = map(lambda i:Instrument(i, 0, "", 0), range(16))
        self.instruments             = map(lambda i:[], range(16))
        
    def add_event(self, event):
        if type(event) is midi.ChannelPrefixEvent:
            self.current_channel = event.data[0]
            
        elif type(event) is midi.InstrumentNameEvent:
            self.current_instrument_name = ''.join(map(chr,event.data))
            if self.current_channel!=-1 and event.tick==0:
                self.default_instruments[self.current_channel].name = self.current_instrument_name
                
        elif type(event) is midi.EndOfTrackEvent:
            self.current_channel=-1
            self.current_instrument_name = ""
            
        elif type(event) is midi.ProgramChangeEvent:
            channel             = event.channel
            instrument_name     = ""
            if channel==self.current_channel:
                instrument_name = self.current_instrument_name
            instrument          = Instrument(channel, event.get_value(), instrument_name, event.tick)
            self.instruments[channel].append(instrument)
            
    def get_instrument(self, channel, tick):
        ret = self.default_instruments[channel]

        for instrument in self.instruments[channel]:
            if instrument.tick>tick:
                break
            else:
                ret = instrument

        return ret


class Tempos:
    def __init__(self):
        self.tempos = []
        
    def add_event(self, event):
        if type(event) is midi.SetTempoEvent:
            self.tempos.append(event)

    def send_tempos_to_radium(self, resolution, lpb):
        for tempo in sorted(self.tempos, key=lambda event: event.tick):
            place = tick_to_place(tempo.tick, resolution, lpb)
            radium.addBPM3(int(tempo.bpm), place[0], place[1], place[2])

        
class Signatures:
    def __init__(self):
        self.signatures = []

    def add_event(self, event):
        if type(event) is midi.TimeSignatureEvent:
            self.signatures.append(event)
            print "22222 GAOIJADFGAOIJOAFIJG"
            print event
            #sys.exit()

    def send_signatures_to_radium(self, resolution, lpb):
        for signature in sorted(self.signatures, key=lambda event: event.tick):
            place = tick_to_place(signature.tick, resolution, lpb)
            radium.addSignature3(int(signature.get_numerator()), int(signature.get_denominator()),
                                 place[0], place[1], place[2])

        
class Events:
    def __init__(self):
        self.channel = -1
        self.notes = map(lambda i:[], range(16))
        self.events = map(lambda i:[], range(16))
        self.other_events = []
        self.instrument = False

        
    def split_channels_helper(self):
        """
        Either returns a new Events instance, or False.
        
        The returned events instance will only contain one channel.
        The events in this instance has been removed from 'self'.
        
        If split_channels returns false, the remaining data has been placed in one channel only.
        (i.e. there was nothing more to split.)
        """

        last_used_channel = 0
        num_channels = 0
        
        for channel in range(16):
            if self.notes[channel]!=[] or self.events[channel]!=[]:
                last_used_channel = channel
                num_channels = num_channels + 1
        if num_channels==0 or num_channels==1:
            self.channel = last_used_channel
            return False

        events = Events()
        
        events.channel=last_used_channel
        
        events.notes[events.channel] = self.notes[events.channel]
        events.events[events.channel] = self.events[events.channel]
        
        self.notes[events.channel] = []
        self.events[events.channel] = []
        
        return events


    def split_by_channel(self):
        """
        Returns a list of channel-specific Event instances, replacing 'self'.
        If the instance already only contained events for one channel, it will return a list only containing itself.

        Only the first of the returned Event instances will contain 'other_events'.
        """
        other_channels = []
        
        new = self.split_channels_helper()
        while new:
            other_channels.append(new)
            new = self.split_channels_helper()

        other_channels.reverse()
        
        return [self] + other_channels
    

    def split_by_instrument(self, instruments):
        """
        Does two things:
        1. Returns a list of preset-specific Event instances, replacing 'self'.
        2. Sets self.instrument for every element in the returned list.
        
        If the instance already only contained events for one preset, it will return a list only containing itself.

        Only the first of the returned Event instances will contain 'events' and 'other_events'.

        A preset is the number set by Program change events. Songs are likely to look clearer by splitting
        tracks rather than adding program changes as FX.
        """
        if self.channel==-1:
            raise Exception("Instance has not been splitted by channel")

        if len(self.notes[self.channel])==0:
            self.instrument = instruments.get_instrument(self.channel, 0)
            return [self]

        splitted = []
        events = Events()
        events.channel = self.channel
        last_instrument = False

        for note in self.notes[self.channel]:
            
            instrument = instruments.get_instrument(self.channel, note.start_tick)
            
            if instrument!=last_instrument:
                last_instrument = instrument
                if len(events.notes[self.channel])>0:
                    splitted.append(events)
                events = Events()
                events.channel = self.channel
                
            events.instrument = instrument
            events.notes[self.channel].append(note)

        splitted.append(events)
            
        splitted[0].events = self.events
        splitted[0].other_events = self.other_events
        return splitted

    def split_by_polyphony(self):
        """
        Returns a list of monophonic Event instances, replacing 'self'.
        If the instance is already monophonic, it will return a list only containing itself.

        Only the first of the returned Event instances will contain 'events' and 'other_events'.
        """
        if self.channel==-1:
            raise Exception("Instance has not been splitted by channel")

        ret = []
        for notes in polyphonic_sequence_to_monophonic_sequences(self.notes[self.channel]):
            events = Events()
            events.channel = self.channel
            events.instrument = self.instrument
            events.notes[events.channel] = notes
            ret.append(events)

        if ret==[]:
            events = Events()
            events.channel = self.channel
            events.instrument = self.instrument
            ret.append(events)
            
        ret[0].events = self.events
        ret[0].other_events = self.other_events
        
        return ret


    def get_radium_tracks(self, polyphonic, instruments):
        tracks = []
        
        for channel_track in self.split_by_channel():
            tracks = tracks + channel_track.split_by_instrument(instruments)
            
        if polyphonic:
            return tracks
        else:
            monophonic_tracks = []
            for track in tracks:
                monophonic_tracks = monophonic_tracks + track.split_by_polyphony()
            return monophonic_tracks


    def add_note(self, start_tick, channel, notenum, velocity):
        self.notes[channel].append(Note(start_tick, channel, notenum, velocity))
        

    def set_endnote(self, end_tick, channel, notenum, velocity):
        for note in self.notes[channel]:
            if not note.has_end_tick() and note.notenum==notenum:
                note.set_end(end_tick, velocity)
                return
        print "Could not find",notenum, channel

        
    def add_event(self, event):
        
        #if type(event) is NoteOnEvent:
        #    print event.velocity
        if type(event) is midi.NoteOnEvent and event.velocity>0:
            self.add_note(event.tick, event.channel, event.pitch, event.velocity)
        elif type(event) is midi.NoteOnEvent and event.velocity==0:
            self.set_endnote(event.tick, event.channel, event.pitch, 0)
        elif type(event) is midi.NoteOffEvent:
            self.set_endnote(event.tick, event.channel, event.pitch, event.velocity)
        else:
            print event
            if type(event) is midi.SetTempoEvent:
                print event.get_bpm()
            #self.events[channel] = event


def tick_to_place_simple(tick, resolution, lpb):
    resolution = int(resolution / lpb)
    line       = int(tick / resolution)
    counter    = tick - (line*resolution)
    dividor    = resolution
    gcd        = fractions.gcd(counter,dividor)
    return [line, counter/gcd, dividor/gcd]

def tick_to_line(tick, resolution, lpb):
    return int(lpb * tick/resolution)

def line_to_tick(line, resolution, lpb):
    return int(line*resolution/lpb)

def tick_to_counter(tick, line, resolution, lpb):
    #subtick = tick - line_to_tick(line, resolution, lpb) # inaccurate
    subtick = tick*lpb - line*resolution # accurate
    return subtick

def tick_to_place(tick, resolution, lpb):
    line       = tick_to_line(tick, resolution, lpb)
    counter    = tick_to_counter(tick, line, resolution, lpb)
    dividor    = resolution
    gcd        = fractions.gcd(counter,dividor)
    return [line, counter/gcd, dividor/gcd]

if 0:
    tick = 1234
    resolution = 1943
    lpb = 333
    line = tick_to_line(tick, resolution, lpb)
    counter = tick_to_counter(tick, line, resolution, lpb)
    print tick_to_place(tick, resolution, lpb),lpb
    #print tick_to_place_simple(tick, resolution, lpb)
    # (define (g a b c d) (/ (+ a (/ b c)) d))
    sys.exit(0)

def send_notes_to_radium_track(notes, tracknum, resolution, lpb):
    for note in notes:
        startplace = tick_to_place(note.start_tick, resolution, lpb)

        if not note_has_end_tick():
            endplace = [radium.getNumLines(), 0, 1]
        elif note.has_legal_end_tick():
            endplace = tick_to_place(note.end_tick, resolution, lpb)
        else:
            continue
            
        radium.addNote3(note.notenum, note.velocity / 128.0,
                        startplace[0], startplace[1], startplace[2],
                        endplace[0], endplace[1], endplace[2],
                        tracknum, -1, -1)


def handle_radium_instruments(tracks, port=""):
    instruments = {}
    track_num = 0
    for track in tracks:
        instrument = track.instrument
        if instrument not in instruments:
            instrument_num = radium.createMIDIInstrument(instrument.name) # No need to check return value. createNewMIDIInstrument never fails.
            radium.setInstrumentData(instrument_num, "channel", str(instrument.channel))
            radium.setInstrumentData(instrument_num, "preset", str(instrument.preset))
            radium.setInstrumentData(instrument_num, "port", port)
            if port=="":
                port = radium.getInstrumentData(instrument_num, "port") # if port=="", radium will ask the user. We only want to do that one time.
            instruments[instrument] = instrument_num
        else:
            instrument_num = instruments[instrument]
        radium.setInstrumentForTrack(instrument_num, track_num)
        track_num = track_num + 1


def get_last_tick(tracks):
    ret = 0
    for track in tracks:
        last_tick = track[-1].tick
        if last_tick > ret:
            ret = last_tick
    return ret
    
    
# Quick hack.
def clear_radium_editor():
    while radium.getNumBlocks()>1:
        radium.deleteBlock()
    radium.appendBlock()
    radium.selectPrevBlock()
    radium.deleteBlock()
    

def import_midi_do(tracks, lpb=4, midi_port="", polyphonic=True):
    radium_tracks = []
    resolution = tracks.resolution
    format = tracks.format
    print resolution
    print format
    print "num_tracks: "+str(len(tracks))
    
    tracks.make_ticks_abs()

    num_tracks = 0 #radium.numTracks()
    tracknum = 0

    #clear_radium_editor()    
    radium.appendBlock()
    
    radium.setMainLPB(lpb)
    radium.setMainBPM(120) # Default SMF value.

    last_place = tick_to_place(get_last_tick(tracks), resolution, lpb)
    radium.setNumLines(last_place[0] + 2)
    
    # Init instruments first
    instruments = Instruments()
    for track in tracks:
        for event in track:
            instruments.add_event(event)

    # Init tempos
    tempos = Tempos()
    for track in tracks:
        for event in track:
            tempos.add_event(event)
    tempos.send_tempos_to_radium(resolution, lpb)

    # Init signatures
    signatures = Signatures()
    for track in tracks:
        for event in track:
            signatures.add_event(event)
    signatures.send_signatures_to_radium(resolution, lpb)
    
    # Init notes and fx, plus generate radium tracks
    for track in tracks:
        events = Events()
        for event in track:
            events.add_event(event)

        tracks = events.get_radium_tracks(polyphonic, instruments)
        if len(tracks)>0:
            num_tracks = num_tracks + len(tracks)
            radium.setNumTracks(num_tracks)
            
            for notes in tracks:
                print "VVV instrument: '"+notes.instrument.name+"'. preset:"+str(notes.instrument.preset)
                send_notes_to_radium_track(notes.notes[notes.channel], tracknum, resolution, lpb)
                radium.setTrackVolume(1.0,tracknum)
                radium_tracks.append(notes)
                tracknum = tracknum + 1
   
    handle_radium_instruments(radium_tracks, midi_port)

    return radium_tracks


def get_tracks(filename):
    if filename=="":
        filename = radium.getLoadFilename("Choose midi file", "*.mid *.MID *.midi *.MIDI")
    if not filename or filename=="":
        return False
    try:
        tracks = midi.read_midifile(filename)
    except:
        radium.addMessage("Could not read "+filename+". Either file doesn't exist, or it could not be read as a standard midi file.");
        return False
    
    return tracks

    
def get_parameters(lpb, midi_port, polyphonic):
    try:
        radium.openRequester("") #"Please set Midi File Properties")

        if lpb==0:
            lpb = radium.requestInteger("Lines per beat? (4) ",1,100000)
            if lpb==0:
                lpb = 4

        if polyphonic=="not set":
            polyphonic = radium.requestString("Polyphonic tracks? (Yes) ")
            if polyphonic=="no" or polyphonic=="No" or polyphonic=="NO":
                polyphonic = False
            else:
                polyphonic = True

        while midi_port=="":
            midi_port = radium.requestMidiPort()

    finally:
        radium.closeRequester()

    return lpb,midi_port,polyphonic


def import_midi(filename="", lpb=0, midi_port="", polyphonic="not set"):
    #filename = "sinclair.MID"
    #filename = "/gammelhd/gammelhd/gammel_lyd/gammelhd/amiga/work/gammelhd/music/octamed6/Midi/simpsons.mid"
    tracks = get_tracks(filename)
    if tracks==False:
        return
    
    lpb, midi_port, polyphonic = get_parameters(lpb, midi_port, polyphonic)
    
    return import_midi_do(tracks, lpb, midi_port, polyphonic)


if __name__ == "__main__":
    filename = "/home/kjetil/Downloads/reich.mid"
    #filename = "sinclair.MID"
    #filename = "/gammelhd/gammelhd/gammelhd/home/kjetil/bmod54.mid"
    for channel in import_midi(filename, 4, "port", False):
        print "new channel"
        for note in channel.notes[0]:
            print note.channel,note.notenum,":",note.start_tick/480.0,note.end_tick/480.0
        print
        for sequence in polyphonic_sequence_to_monophonic_sequences(channel.notes[0]):
            print map(lambda n: [n.start_tick,n.end_tick,"pitch:",n.notenum], sequence)
            print
        
    
