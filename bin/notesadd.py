#/* Copyright 2001 Kjetil S. Matheussen
#
#This program is free software; you can redistribute it and/or
#modify it under the terms of the GNU General Public License
#as published by the Free Software Foundation; either version 2
#of the License, or (at your option) any later version.
#
#This program is distributed in the hope that it will be useful,
#but WITHOUT ANY WARRANTY; without even the implied warranty of
#MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#GNU General Public License for more details.
#
#You should have received a copy of the GNU General Public License
#along with this program; if not, write to the Free Software
#Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA. */


# This file is more like a quick hack to have a more convinient interface
# for python. Its not very good. Have to make something better.
# Same goes for the c-function "ra.addNoteAdds".




from common import *
import radium,types

ra=radium

class NoteAdd:
	def __init__(self,notenum,place,endplace=-1.0,volume=-1.0):
		self.notenum=notenum
		self.place=float(place)
		self.volume=float(volume)
		self.endplace=float(endplace)
#		self.place=place
#		self.volume=volume
#		self.endplace=endplace
		print self.notenum
		print self.place
		print self.volume
		print self.endplace


def addNoteAdds_noteadds(noteadds,track,startplace,transpose,sort):

	for lokke in range(len(noteadds)):
		noteadds[lokke].notenum+=transpose

#	print "track: %d" % track
	ra.addNoteAdds([noteadds],-1,-1,track,startplace,sort)


def addNoteAdds_list(notelist,track,startplace,transpose,sort):

	noteadds=[]

	for lokke in range(len(notelist)):
		if type(notelist[0])!=types.ListType:
			llen=0
		else:
			llen=len(notelist[0])

		if llen==0:
			noteadds.append(NoteAdd(notelist[lokke],lokke))
		if llen==1:
			noteadds.append(NoteAdd(notelist[lokke][0],lokke))
		elif llen==2:
			noteadds.append(NoteAdd(notelist[lokke][0],notelist[lokke][1]))
		elif llen==3:
			noteadds.append(NoteAdd(notelist[lokke][0],notelist[lokke][1],endplace=notelist[lokke][2]))
		elif llen==4:
			noteadds.append(NoteAdd(notelist[lokke][0],notelist[lokke][1],endplace=notelist[lokke][2],volume=notelist[lokke][3]))

	addNoteAdds_noteadds(noteadds,track,startplace,transpose,sort)


# `noteadds` can be a list of either:
# 1. NoteAdd objects
# 2. Integers indicating notenum where the placements are the listnum
# 3. A list. When the length is:
#    1. Same as previous #2
#    2. First is notenum. Second is place.
#    3. First is notenum. Second is place. Third is endplace.
#    4. First is notenum. Second is place. Third is endplace. Fourth is volume
#
# Volumes with values -1.0 is standard volume
# Endplaces with values -1.0 means that it ends at the start of the next note,
# or no defined end if at last position.
#
# If providing a list containing lists, each list in list can either be
# an integer or a list with length [1..4].


def addNoteAdds(noteadds,track=-1,startplace=-1.0,transpose=0,sort=1):
	if isinstance(noteadds[0],NoteAdd):
		addNoteAdds_noteadds(noteadds,track,startplace,transpose,sort)
	else:
		addNoteAdds_list(noteadds,track,startplace,transpose,sort)


def getNoteAdds(windownum=-1,blocknum=-1,tracknum=-1,das_range=false):
	noteadds=[]
	numnotes=ra.getNumNotes(tracknum,blocknum)

	if numnotes==0: return []

	print numnotes
	print "fsred"

	print ra.getNoteNote(windownum,blocknum,tracknum,0)

	for lokke in range(numnotes):
		noteadds.append(
			NoteAdd(
				ra.getNoteNote(windownum,blocknum,tracknum,lokke),
				ra.getNotePlace(windownum,blocknum,tracknum,lokke),
				ra.getNoteVolume(windownum,blocknum,tracknum,lokke),
				ra.getNoteEndPlace(windownum,blocknum,tracknum,lokke)
			)
		)

	return noteadds




