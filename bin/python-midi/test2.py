import src as midi

z = midi.read_midifile("mary.mid")
print
print dir(z)
print
z.textdump()
