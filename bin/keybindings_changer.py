
import os
import sys
import traceback
import platform


class NullWriter(object):
    def write(self, value): pass


class RadiumMock:
    def addMessage(self, message):
        print "MESSAGE: "+message
    def appendFilePaths(self, path1, path2):
        return path1 + "/" + path2
    def getHomePath(self):
        return "/home/kjetil"
    def getPath(self, path):
        return path
    def fileExists(self, filename):
        return True
    def openFileForReading(self, filename):
        return open(filename).readlines()
    def fileAtEnd(self, disk):
        return len(disk)==0
    def readLineFromFile(self, disk):
        return disk.pop(0).rstrip();
    def closeFile(self, disk):
        if not type(disk) == list:
            disk.close()
    def openFileForWriting(self, filename):
        return open(filename, 'w')
    def writeToFile(self, disk, text):
        disk.write(text)
        
if __name__ == "__main__" or sys.g_program_path=='__main__':
    radium = RadiumMock()
else:
    import radium

ra = radium


def get_filename():
    return ra.appendFilePaths(ra.getHomePath(),
                              ra.appendFilePaths(ra.getPath(".radium"),
                                                 ra.getPath("keybindings.conf")))

    #return os.path.join(os.path.expanduser("~"), ".radium", "keybindings.conf")


def get_lines():
    filename = get_filename()
    if ra.fileExists(filename)==False:
        #print "User keyboard configuration file doesn't exist"
        return []

    disk=ra.openFileForReading(filename)

    ret = []

    while ra.fileAtEnd(disk)==False:
        ret += [ra.readLineFromFile(disk)]

    ra.closeFile(disk)

    return ret


def write_lines(lines):
    disk = ra.openFileForWriting(get_filename())

    for line in lines:
        print "line:",line
        ra.writeToFile(disk, line + "\n")

    ra.closeFile(disk)


def has_line(line, lines):
    for aline in lines:
        print "comparing start->",aline,"-",line,"<-end"
        if line==aline:
            return True
    return False


def get_keybinding_and_command(line):
    stripped = line.strip()
    if len(stripped)==0 or stripped[0]=="#":
        return ["",""]
    
    splitted = line.split(" : ")
    if len(splitted)<2:
        return ["",""]
    else:
        return [splitted[0].strip(), splitted[1].strip()]
    
# "CTRL_L A : aasdfsdf"
def get_keybinding_from_line(line):
    return get_keybinding_and_command(line)[0]
    
def get_command_from_line(line):
    return get_keybinding_and_command(line)[1]


#returns the last line with that keybinding ("old_line"), or False.
def has_keybinding(keybinding, lines):
    ret = False

    for line in lines:
        if get_keybinding_from_line(line)==keybinding:
            ret = line

    return ret


# returns changes lines
def change_line(old_line, new_line, lines):
    def maybechangeit(line):
        if line==old_line:
            return new_line
        else:
            return line
    return map(maybechangeit, lines)


def append_line(new_line, lines):
    return lines + [new_line]


"""
Key  : function()
Key2 : function()
Key  : function2()
"""

# Returns new lines, or False, if nothing needs to be changed
#
# Works quite brutally:
# 1. Remove all lines that has either keybinding or command
# 2. Appends new_line
def ensure_has_line(new_line):
    lines = get_lines()

    keybinding = get_keybinding_from_line(new_line)
    command = get_command_from_line(new_line)

    num_removed = 0
    new_lines = []
    
    for line in lines:
        if get_keybinding_from_line(line) != keybinding and get_command_from_line(line) != command:
            new_lines += [line]
        else:
            num_removed += 1

    if num_removed==1 and has_line(new_line, lines):
        return False

    return append_line(new_line, new_lines)

#returns True if configuration file was changed.
def insert_new_line_into_conf_file(new_line):
    lines = ensure_has_line(new_line)
    if lines:
        write_lines(lines)
        return True
    else:
        return False


def insert_new_keybinding_into_conf_file(keybinding, command):
    return insert_new_line_into_conf_file(keybinding + " : " + command)

def FROM_C_insert_new_keybinding_into_conf_file(keybinding, command):            
    old_stdout = sys.stdout
    old_stderr = sys.stderr
    if platform.system() != "Linux": # and os.isatty(sys.stdout.fileno()):
        sys.stdout = NullWriter()
        sys.stderr = NullWriter()

    try:
        insert_new_line_into_conf_file(keybinding + " : " + command)

    except:
        e = sys.exc_info()[0]
        message = traceback.format_exc()
        message2 = "Unable to add keybinding for %s to do %s:<br><pre>%s</pre>" % (keybinding, command, message)
        print message2
        ra.addMessage(message2)
        return

    if platform.system() != "Linux": # and os.isatty(sys.stdout.fileno()):
        sys.stdout = old_stdout
        sys.stderr = old_stderr

    ra.reloadKeybindings()


def remove_keybinding_from_conf_file(keybinding, command):
    line_to_remove = keybinding + " : " + command
    
    lines = get_lines()
    print "lines:",lines
    if has_line(line_to_remove, lines)==False:
        message2 = "Could not remove keybinding \"%s\".<br>It might be a default keybinding, and those can't be removed. They can be overridden to be used for something else though." % line_to_remove
        print message2
        ra.addMessage(message2)
        return

    def keepit(line):
        if line != line_to_remove:
            return True
        else:
            return False

    new_lines = filter(keepit, lines)

    write_lines(new_lines)

def FROM_C_remove_keybinding_from_conf_file(keybinding, command):
    old_stdout = sys.stdout
    old_stderr = sys.stderr
    if platform.system() != "Linux": # and os.isatty(sys.stdout.fileno()):
        sys.stdout = NullWriter()
        sys.stderr = NullWriter()

    try:
        remove_keybinding_from_conf_file(keybinding, command)
    except:
        e = sys.exc_info()[0]
        message = traceback.format_exc()
        message2 = "Unable to remove keybinding %s - %s:<br><pre>%s</pre>" % (keybinding, command, message)
        print message2
        ra.addMessage(message2)
        return

    if platform.system() != "Linux": # and os.isatty(sys.stdout.fileno()):
        sys.stdout = old_stdout
        sys.stderr = old_stderr
        
    ra.reloadKeybindings()



if __name__ == "__main__":
    #update_conf_file("#gakkgakk")
    insert_new_line_into_conf_file("a b : 90")

