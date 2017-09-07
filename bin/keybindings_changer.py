
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

#returns lines, or False, if nothing needs to be changed
def ensure_has_line(new_line):
    lines = get_lines()
    if has_line(new_line, lines):
        return False

    keybinding = get_keybinding_from_line(new_line)
    command = get_command_from_line(new_line)

    def keepit(line):
        if get_keybinding_from_line(line) != keybinding and get_command_from_line(line) != command:
            return True
        else:
            return False

    new_lines = filter(keepit, lines)
    
    return append_line(new_line, new_lines)
            
        
    has_inserted = False
    
    keybinding = get_keybinding_from_line(new_line)

    old_line = has_keybinding(keybinding, lines)
    if old_line:
        lines = change_line(old_line, new_line, lines)
        has_inserted = True
    
    command = get_command_from_line(new_line)

    old_line = has_command(command, lines)
    if old_line:
        if has_inserted:
            lines = remove_line(old_line, lines)
        else:
            lines = change_line(old_line, new_line, lines)
            has_inserted = True

    if has_inserted:
        return lines
    else:
        return append_line(new_line, lines)

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
        print "Unable to add keybinding for %s to do %s" % (keybinding, command)
        radium.addMessage("Unable to add keybinding for %s to do %s" % (keybinding, command))
        return

    radium.reloadKeybindings()


if __name__ == "__main__":
    #update_conf_file("#gakkgakk")
    insert_new_line_into_conf_file("a b : 903588")

