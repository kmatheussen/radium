
import os
import tempfile
import shutil
import sys
import traceback

class RadiumMock:
    def addMessage(self, message):
        print "MESSAGE: "+message

if __name__ == "__main__" or sys.g_program_path=='__main__':
    radium = RadiumMock()
else:
    import radium


def get_filename():
    return os.path.join(os.path.expanduser("~"), ".radium", "keybindings.conf")


def get_lines():
    filename = get_filename()
    try:
        filehandle=open(filename,'r')
    except:
        print "Configuration file %s does not seem to exist" % filename
        return []

    content = filehandle.read()
    filehandle.close()

    if len(content)==0:
        return []
    
    if content[-1] != "\n":
        content += "\n"
        
    return content.splitlines()



def write_lines(lines):
    try:
        _,tempfilename = tempfile.mkstemp("radium_temp_conf")
        print "tempfilename",tempfilename
        dastempfile = open(tempfilename, 'w')
        for line in lines:
            print "line:",line
            dastempfile.write(line + "\n")

        dastempfile.close()

    except:
        e = sys.exc_info()[0]
        message = traceback.format_exc()
        print "Unable to create temporary file %s" % tempfilename,message
        radium.addMessage("Unable to create temporary file %s" % tempfilename)
        return
        
    filename = get_filename()
    
    try:
        shutil.copy2(filename, filename + ".bak")
        shutil.copyfile(tempfilename, filename)
    except:
        e = sys.exc_info()[0]
        message = traceback.format_exc()
        print "Could not copy %s to %s" % (tempfilename,filename),message
        radium.addMessage("Could not copy %s to %s:<pre>%s</pre>" % (tempfilename,filename,message))

    os.remove(tempfilename)
    


def has_line(line, lines):
    for aline in lines:
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
    try:
        insert_new_line_into_conf_file(keybinding + " : " + command)
    except:
        e = sys.exc_info()[0]
        message = traceback.format_exc()
        message2 = "Unable to add keybinding for %s to do %s:<br><pre>%s</pre>" % (keybinding, command, message)
        print message2
        radium.addMessage(message2)
        return

    radium.reloadKeybindings()


def remove_keybinding_from_conf_file(keybinding, command):
    line_to_remove = keybinding + " : " + command
    
    lines = get_lines()
    if has_line(line_to_remove, lines)==False:
        message2 = "Could not remove keybinding \"%s\".<br>The reason could be that it's a default keybinding, which can't be removed yet." % line_to_remove
        print message2
        radium.addMessage(message2)
        return

    def keepit(line):
        if line != line_to_remove:
            return True
        else:
            return False

    new_lines = filter(keepit, lines)

    write_lines(new_lines)

def FROM_C_remove_keybinding_from_conf_file(keybinding, command):
    try:
        remove_keybinding_from_conf_file(keybinding, command)
    except:
        e = sys.exc_info()[0]
        message = traceback.format_exc()
        message2 = "Unable to remove keybinding %s - %s:<br><pre>%s</pre>" % (keybinding, command, message)
        print message2
        radium.addMessage(message2)
        return

    radium.reloadKeybindings()
    

if __name__ == "__main__":
    #update_conf_file("#gakkgakk")
    insert_new_line_into_conf_file("a b : 903588")

