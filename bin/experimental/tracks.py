

class note:
    def __init__(self,ra,blocknum,tracknum,notenum):
        self.ra=ra
        self.block=block
        self.track=track
        self.note=ra.RA_getNoteNote(blocknum,tracknum,notenum)
        self.volume=ra.RA_getNoteVel(blocknum,tracknum,notenum)
        self.start=ra.RA_getNoteFloatStart(blocknum,tracknum,notenum)
        self.length=ra.RA_getNoteFloatEnd(blocknum,tracknum,notenum) - self.start
        return
    


class tracks:

    def getNoteArray(self):
        num_notes=self.ra.RA_getNumNotes(self.tracknum,self.blocknum)
        notearray=[]
        for lokke in range(num_notes):
            notarray[lokke]=note(ra,blocknum,self.tracknum,lokke)
        return notearray

    def setNoteArray(self,notearray):
        self.ra.RA_removeNotes(self.blocknum,self.tracknum)
        for lokke in range(num_notes):
            self.ra.RA_addNote_FloatPlace(
                self.blocknum,
                self.tracknum,
                notearray[lokke].start,
                notearray[lokke].note,
                notearray[lokke].volume,
                notearray[lokke].length-notearray[lokke].start
                )

    def __init__(self,ra,blocknum,tracknum):
        self.ra=ra
        self.blocknum=blocknum
        self.tracknum=tracknum
        return



        
