

class blocks:
    def getTrack(self,tracknum):
        return tracks(self.ra,self.blocknum,tracknum)

    def __init__(self,ra,blocknum):
        self.ra=ra
        self.blocknum=blocknum
        return
    
    
