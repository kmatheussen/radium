
import sys
sys.path.append("python-midi")

import fractions


#from src import *
import src as midi

import radium

class Note:
    def __init__(self, start_tick, channel, notenum, velocity):
        self.start_tick = start_tick
        self.end_tick = -1
        self.channel = channel
        self.notenum = notenum
        self.velocity = velocity


    def set_end(self, end_tick, end_velocity):
        self.end_tick = end_tick
        self.end_velocity = end_velocity


def is_overlapping(a, b):
    if b.start_tick < a.start_tick:
        return is_overlapping(b, a)
    else:
        return b.start_tick < a.end_tick


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
        self.name    = name + (" [%d/%d]" % (channel,preset))
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

instruments = Instruments()


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
    

    def split_by_instrument(self):
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


    def get_radium_tracks(self, polyphonic):
        tracks = []
        
        for channel_track in self.split_by_channel():
            tracks = tracks + channel_track.split_by_instrument()
            
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
            if note.end_tick==-1 and note.notenum==notenum:
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
        #print "add note",note,tracknum
        startplace = tick_to_place(note.start_tick, resolution, lpb)
        endplace   = tick_to_place(note.end_tick, resolution, lpb)

        radium.addNote(note.notenum, note.velocity,
                       startplace[0], startplace[1], startplace[2],
                       endplace[0], endplace[1], endplace[2],
                       -1, -1, tracknum)


def handle_radium_instruments(tracks, port=""):
    instruments = {}
    track_num = 0
    for track in tracks:
        instrument = track.instrument
        if instrument not in instruments:
            instrument_num = radium.createNewInstrument("midi", instrument.name)
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
    
    
def import_midi_do(filename, lpb=4, midi_port="", polyphonic=True):
    radium_tracks = []
    tracks = midi.read_midifile(filename)
    resolution = tracks.resolution
    format = tracks.format
    print resolution
    print format
    print "num_tracks: "+str(len(tracks))
    
    tracks.make_ticks_abs()

    num_tracks = 0 #radium.numTracks()
    tracknum = 0

    radium.setLPB(lpb)
    radium.setBPM(120) # Default SMF value.

    last_place = tick_to_place(get_last_tick(tracks), resolution, lpb)
    radium.setNumLines(last_place[0] + 2)
    
    # Init instruments first
    for track in tracks:
        for event in track:
            instruments.add_event(event)

    # Generate radium tracks from midi tracks
    for track in tracks:
        events = Events()
        for event in track:
            events.add_event(event)

        tracks = events.get_radium_tracks(polyphonic)
        if len(tracks)>0:
            num_tracks = num_tracks + len(tracks)
            radium.setNumTracks(num_tracks)
            
            for notes in tracks:
                print "VVV instrument: '"+notes.instrument.name+"'. preset:"+str(notes.instrument.preset)
                send_notes_to_radium_track(notes.notes[notes.channel], tracknum, resolution, lpb)
                radium_tracks.append(notes)
                tracknum = tracknum + 1
   
    handle_radium_instruments(radium_tracks, midi_port)

    return radium_tracks


# Quick hack.
def clear_radium_editor():
    while radium.numBlocks()>1:
        radium.deleteBlock()
    radium.appendBlock()
    radium.selectPrevBlock()
    radium.deleteBlock()
    

def import_midi(filename):
    clear_radium_editor()    
    return import_midi_do(filename, 4, "", False)


if __name__ == "__main__":
    def addNote(notenum, velocity,
                line, counter, dividor,
                end_line, end_counter, end_dividor,
                windownum, blocknum, tracknum):
        return
        print "addNote",tracknum,notenum,velocity
    def setLPB(lpb):
        pass
    def setBPM(pbm):
        pass

    radium.addNote = addNote
    radium.setLPB = setLPB
    radium.setBPM = setBPM
    
    for channel in import_midi_do("sinclair.MID", 4, "", False):
        print "new channel"
        for note in channel.notes[0]:
            print note.channel,note.notenum,":",note.start_tick/480.0,note.end_tick/480.0
        print
        for sequence in polyphonic_sequence_to_monophonic_sequences(channel.notes[0]):
            print map(lambda n: [n.start_tick,n.end_tick,"pitch:",n.notenum], sequence)
            print
        
    
