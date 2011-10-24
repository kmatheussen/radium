
import sys

from X11_BlockSelector import *


BS_StartBlockSelector()
#print sys.stdin.readline()
#BS_SelectBlock(5)
#BS_SelectPlaylistPos(6)

BS_UpdateBlockList("/tmp/gakk","en","to","tre","fire")
BS_UpdatePlayList("/tmp/gakk2","1","3","2","2","2")

BS_SelectBlock(1)
BS_SelectPlaylistPos(2)

print sys.stdin.readline()
BS_EndBlockSelector()

