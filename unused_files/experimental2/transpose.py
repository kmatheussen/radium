











def transposetrack(ra,blocknum,tracknum,trans):
    track=blocks(ra,blocknum).getTrack(tracknum)
    notes[]=track.getNoteArray()
    for lokke in range(notes.len):
        notes[lokke].note=notes[lokke]+trans
    track.setNoteArray(notes[])
    return



