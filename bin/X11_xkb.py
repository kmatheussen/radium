import subprocess,os

global_layout = ""

def get_xkb(setxkbmap):
    process = subprocess.Popen([setxkbmap,"-query"], stdout=subprocess.PIPE)
    output = process.communicate()
    lines = output[0].split("\n")
    matches = [x for x in lines if x[:len("layout:")]=="layout:"][0]
    layout = matches.split(':')[1].strip()
    print "keyboard layout: "+layout
    return layout

def set_xkb(setxkbmap,layout):
    os.system(setxkbmap+" -synch -layout "+layout)

def save_xkb(setxkbmap):
    global global_layout
    global_layout = get_xkb(setxkbmap)

def restore_xkb(setxkbmap):
    global global_layout
    set_xkb(setxkbmap, global_layout)

if __name__=="__main__":
    get_xkb("bin/packages/setxkbmap/setxkbmap")
