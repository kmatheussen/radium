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





from common import *

import notesadd

def putinrows(rows,row):
	for lokke in range(len(row)):
		rows[row[lokke]].append([lokke])


def bubble():
	row=[7,1,5,3,9,6,2,10,0,8,11,4]
#	row=[9,11,2,4,10,8,1,7,6,0,3,5]
#	row=[9,110,2,4,10,8,1,7,6,0,3,5]

	rows=makeemptylist(len(row))

	putinrows(rows,row)

	end=0
	even=0
	while end==0:
		if even==0:
			even=1
		else:
			even=0

		end=1
		for lokke in range(even,len(row)-1,2):
			if row[lokke]>row[lokke+1]:
				temp=row[lokke+1]
				row[lokke+1]=row[lokke]
				row[lokke]=temp
				end=0

		putinrows(rows,row)
#		print row

	for lokke in range(len(rows)):
		notesadd.addNoteAdds(rows[lokke],track=lokke,transpose=20+lokke*4)
#	notesadd.addNoteAdds(rows[0],track=0,transpose=20)


#print "start"
#bubble()

# Note-expand/shrink (max range is expanded and shrinked, and the pitches are scaled accordingly)

import radium
ra=radium

def pitchexpand(percent):
	for lokke in range(ra.getNumNotes()):
		print lokke
		notenote=ra.getNoteNote(-1,-1,-1,lokke)
		ra.putNoteNote(notenote+6,-1,-1,-1,lokke)
