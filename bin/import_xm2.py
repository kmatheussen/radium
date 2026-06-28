#
# XM module parser for Radium.
# Reads XM file, extracts XI instruments to temp files, parses patterns.
# Communicates with Scheme via radium.evalScheme.
#

import sys, os, traceback, struct, platform, tempfile

class NullWriter(object):
    def write(self, value): pass

if __name__ == "__main__":
    pass # not run standalone typically

import radium


# --------------- XM instrument -> XI temp file extraction ---------------

def read_byte(f):
    return ord(f.read(1))

def read_le16(f):
    return struct.unpack("<H", f.read(2))[0]

def read_le32(f):
    return struct.unpack("<I", f.read(4))[0]

def read_signed_byte(f):
    b = ord(f.read(1))
    if b >= 128:
        return b - 256
    return b


def extract_xi_instrument(f, instr_start):
    """Extract a single XM instrument as a temporary .xi file.
    instr_start = f.tell() BEFORE reading the 4-byte instr_size.
    Returns the temp filename or None."""
    
    # Read the 4-byte instrument size (already consumed by caller? No, we read it here)
    instr_size = read_le32(f)
    start_pos = f.tell()
    
    instr_name = f.read(22)     # instrument name
    instr_type = read_byte(f)   # always 0
    num_samples = read_le16(f)  # number of samples
    
    # Validate: reject obviously bad instruments
    if num_samples > 128:
        print("  BAD num_samples=%d, skipping" % num_samples)
        f.seek(instr_start + instr_size)
        return None, "", 64, 0, 0, 0
    if instr_size < 29 or instr_size > 1000000:
        print("  BAD instr_size=%d, skipping" % instr_size)
        f.seek(instr_start + instr_size)
        return None, "", 64, 0, 0, 0
    
    # Extract trimmed name
    name = instr_name.split(b'\x00')[0].decode('latin-1', errors='replace')
    
    print("  name=%r type=%d num_samples=%d instr_size=%d" % (name, instr_type, num_samples, instr_size))
    
    if instr_size <= 29:
        # No samples, skip any extra header bytes
        if instr_size > 29:
            f.read(instr_size - 29)
        f.seek(instr_start + instr_size)
        return None, name, 64, 0, 0, 0
    
    if num_samples == 0:
        # Skip rest of header
        f.seek(instr_start + instr_size)
        return None, name, 64, 0, 0, 0
    
    # Build XI file by copying raw blocks (matching import_mod.py proven approach)
    xi = bytearray()
    
    # XI header
    xi.extend(b"Extended Instrument: ")
    xi.extend(instr_name[:22].ljust(22, b"\x00")[:22])
    xi.append(0x1a)
    xi.extend(b"Radium Tracker")   # 14 bytes tracker name
    xi.extend(b"\x00" * 6)         # pad to 20 bytes total
    
    # Version 0x0102 (2 bytes LE)
    xi.extend(b"\x02\x01")
    
    # Copy 208 bytes from XM offset 33 to 241 (sample map + envelopes + fadeout)
    f.seek(instr_start + 33)
    xi.extend(f.read(241 - 33))
    
    # MIDI / reserved: 22 zero bytes
    xi.extend(b"\x00" * 22)
    
    # num_samples: copy 2 bytes from XM offset 27
    f.seek(instr_start + 27)
    xi.extend(f.read(2))
    
    # Sample headers and data: positioned at instr_start + instr_size
    f.seek(instr_start + instr_size)
    
    # Copy sample headers (40 bytes each), collect byte counts, and extract per-sample data
    num_bytess = []
    sample_volume = 64
    sample_finetune = 0
    sample_loop_start = 0
    sample_loop_length = 0
    for i in range(num_samples):
        sh_data = f.read(40)
        slen = struct.unpack("<I", sh_data[0:4])[0]
        num_bytess.append(slen)
        xi.extend(sh_data)
        # Only use first sample's data for the MOD instrument vector
        if i == 0:
            sample_volume = ord(sh_data[12:13]) if isinstance(sh_data[12], str) else sh_data[12]
            sample_finetune = struct.unpack("<b", sh_data[13:14])[0]  # signed byte
            sample_loop_start = struct.unpack("<I", sh_data[4:8])[0]
            sample_loop_length = struct.unpack("<I", sh_data[8:12])[0]
    
    # Copy sample data (sample_length is in bytes in XM)
    for num_bytes in num_bytess:
        xi.extend(f.read(num_bytes))
    
    # Write to temp file
    fd, temp_path = tempfile.mkstemp(suffix=".xi", prefix="radium_xm_instr_")
    os.close(fd)
    with open(temp_path, "wb") as outf:
        outf.write(bytes(xi))
    
    return temp_path, name, sample_volume, sample_finetune, sample_loop_start, sample_loop_length

# --------------- Pattern parsing ---------------
def parse_xm_pattern_data(data, num_rows, num_channels):
    """Parse packed XM pattern data into a list of (row, channel, note, instrument, volume, effect_type, effect_param)."""
    result = []
    row = 0
    pos = 0
    data_len = len(data)
    
    while row < num_rows and pos < data_len:
        flag = data[pos] if isinstance(data[pos], int) else ord(data[pos])
        pos += 1
        
        if flag == 0:
            # Empty row - all channels have no data, but we still need to track channels
            row += 1
            continue
        
        note = 0
        instrument = 0
        volume = 0
        effect_type = 0
        effect_param = 0
        channel = 0
        
        # Check if this is a "same row" continuation
        # bit 7 set means: remaining channels in this row are skipped, OR this is a compressed row
        # Based on XM spec: if bit 7 is set, it means either "note follows" or packed row
        
        # Actually, let me re-read the spec:
        # Packing: Each row starts with a flag byte. If bits 0-4 are all 0 and bit 7 is 1:
        #   -> next byte is also a flag byte (continue)
        # If bit 7 is 1 and any of bits 0-4 is set:
        #   -> this is a note with the flag bits indicating what follows
        # If bit 7 is 0:
        #   -> The byte is the note value itself (0=no note, 97=key off)
        
        if flag & 0x80:
            # Compressed: flag byte tells us what follows
            if flag & 0x01 and pos < data_len:
                note = data[pos] if isinstance(data[pos], int) else ord(data[pos])
                pos += 1
            if flag & 0x02 and pos < data_len:
                instrument = data[pos] if isinstance(data[pos], int) else ord(data[pos])
                pos += 1
            if flag & 0x04 and pos < data_len:
                volume = data[pos] if isinstance(data[pos], int) else ord(data[pos])
                pos += 1
            if flag & 0x08 and pos < data_len:
                effect_type = data[pos] if isinstance(data[pos], int) else ord(data[pos])
                pos += 1
            if flag & 0x10 and pos < data_len:
                effect_param = data[pos] if isinstance(data[pos], int) else ord(data[pos])
                pos += 1
        else:
            note = flag  # flag byte is actually the note
        
        # Add to result
        if note > 0 or instrument > 0:
            result.append((row, channel, note, instrument, volume, effect_type, effect_param))
        
        channel += 1
        
        # If bits 0-4 are all 0 and bit 7 is set, this flag byte just skips remaining channels
        # Otherwise, we're done with this column
        # For XM, after processing one column's data, we need to continue to the next column
        # using additional flag bytes
        # But actually, the spec says:
        # The pattern data is organized row by row, channel by channel
        # Each cell can be empty or contain data
        # Packed format: flag byte + optional bytes
        
        # Let me re-read the simple spec:
        # The MSB in the note value is never used, so it's used for compression.
        # If the bit is set, the other bits are interpreted as:
        #   bit 0: Note follows
        #   bit 1: Instrument follows
        #   bit 2: Volume column byte follows
        #   bit 3: Effect type follows
        #   bit 4: Effect parameter follows
        
        # So each 5-byte cell starts with the flag byte. If bit 7 is set with any of 0-4,
        # it's a compressed cell. If bit 7 is clear, the byte is the note and the next 4 bytes
        # are instrument/volume/effect (uncompressed).
        
        # Wait, I think the format is:
        # - For each row, for each channel:
        #   - Read a flag byte
        #   - If bit 7 set and any of 0-4 set: compressed cell
        #   - If bit 7 set and 0-4 all 0: skip cell (no data)
        #   - If bit 7 clear: uncompressed, byte is note, next 4 are instrument/volume/effect
        
        # I need to process ALL channels per row. Let me fix this.
    
    # NOTE: The above implementation is simplified. A proper implementation would parse
    # all channels and all rows. For now, let me use a cleaner approach.
    
    return result


def parse_xm_patterns(f, num_patterns, num_channels, pattern_order_table, song_length):
    """Parse all patterns from XM file."""
    patterns = {}

    print("Num patterns: %d" % num_patterns)
    
    for pat_idx in range(num_patterns):
        pat_start = f.tell()
        header_size = read_le32(f)
        packing_type = read_byte(f)
        num_rows = read_le16(f)
        packed_size = read_le16(f)
        
        print("Pattern %d: hdr_sz=%d rows=%d packed_sz=%d file_pos=%d" % (pat_idx, header_size, num_rows, packed_size, f.tell()))
        
        # Skip extra header bytes if header_size > 9
        if header_size > 9:
            f.read(header_size - 9)
        
        if packed_size == 0 or num_rows == 0:
            patterns[pat_idx] = (num_rows, [])
            continue
        
        packed_data = f.read(packed_size)
        
        rows = []
        pos = 0
        row = 0
        channel = 0
        
        while row < num_rows and pos < len(packed_data):
            flag = ord(packed_data[pos])
            pos += 1
            
            note = 0
            instrument = 0
            volume = 0
            effect_type = 0
            effect_param = 0
            
            if flag & 0x80:
                if flag & 0x01 and pos < len(packed_data):
                    note = ord(packed_data[pos]); pos += 1
                if flag & 0x02 and pos < len(packed_data):
                    instrument = ord(packed_data[pos]); pos += 1
                if flag & 0x04 and pos < len(packed_data):
                    volume = ord(packed_data[pos]); pos += 1
                if flag & 0x08 and pos < len(packed_data):
                    effect_type = ord(packed_data[pos]); pos += 1
                if flag & 0x10 and pos < len(packed_data):
                    effect_param = ord(packed_data[pos]); pos += 1
            else:
                note = flag
                if pos < len(packed_data):
                    instrument = ord(packed_data[pos]); pos += 1
                if pos < len(packed_data):
                    volume = ord(packed_data[pos]); pos += 1
                if pos < len(packed_data):
                    effect_type = ord(packed_data[pos]); pos += 1
                if pos < len(packed_data):
                    effect_param = ord(packed_data[pos]); pos += 1
            
            if note > 0 or instrument > 0 or effect_type > 0 or volume > 0:
                rows.append((row, channel, note, instrument, volume, effect_type, effect_param))
            
            channel += 1
            if channel >= num_channels:
                channel = 0
                row += 1
        
        patterns[pat_idx] = (num_rows, rows)
    
    return patterns


def import_xm(filename_base64):
    filename = radium.fromBase64(filename_base64)
    
    old_stdout = sys.stdout
    old_stderr = sys.stderr
    if platform.system() != "Linux":
        sys.stdout = NullWriter()
        sys.stderr = NullWriter()
    
    try:
        print("\n\n\n\n\n\n2=================================== \n\n\n\n\n")
        
        f = open(filename, "rb")
        
        # Read XM header
        magic = f.read(17)
        if magic != b'Extended module: ' and magic != b'Extended Module: ':
            radium.addMessage("Not an XM file: " + filename)
            print("\n\n\n\n\n\n3=================================== \n\n\n\n\n")
            return

        print("\n\n\n\n\n\n4=================================== \n\n\n\n\n")
        
        module_name = f.read(20).split(b'\x00')[0].decode('latin-1', errors='replace')
        f.read(1)  # 0x1a
        tracker_name = f.read(20).split(b'\x00')[0].decode('latin-1', errors='replace')
        version = read_le16(f)
        header_size = read_le32(f)
        song_length = read_le16(f)
        restart_pos = read_le16(f)
        num_channels = read_le16(f)
        num_patterns = read_le16(f)
        num_instruments = read_le16(f)
        flags = read_le16(f)
        default_tempo = read_le16(f)
        default_bpm = read_le16(f)
        pattern_order = [ord(b) for b in f.read(256)]
        
        print("XM: ch=%d pat=%d instr=%d tempo=%d bpm=%d file_pos=%d" % (num_channels, num_patterns, num_instruments, default_tempo, default_bpm, f.tell()))
        
        # Only use song_length entries from pattern order table
        playlist = pattern_order[:song_length]

        print("\n\n\n\n\n\n5===================================\n\n\n\n\n")
        
        # Parse patterns (patterns come before instruments in XM format)
        patterns = parse_xm_patterns(f, num_patterns, num_channels, pattern_order, song_length)

        print("XM: After patterns, file_pos=%d" % f.tell())

        # Extract instruments
        instrument_files = []
        for i in range(num_instruments):
            start = f.tell()
            print("Instrument %d: start=%d" % (i, start))
            temp_path, name, vol, fine, loop_start, loop_len = extract_xi_instrument(f, start)
            instrument_files.append((temp_path, name, vol, fine, loop_start, loop_len))
        
        f.close()
        
        # Send data to Scheme
        radium.evalScheme('(start-adding-xm-events!)')
        
        # Set up frequency table before processing events
        linear_freq = (flags & 1) != 0
        if linear_freq:
            radium.evalScheme('(setup-xm-period-system!)')
        else:
            radium.evalScheme('(setup-xm-amiga-period-system!)')
        
        # Instrument list (name + xi file path + sample data for MOD pipeline)
        code = "(set-xm-instrumentlist! (vector"
        for temp_path, name, vol, fine, loop_start, loop_len in instrument_files:
            code += "(vector "
            # Filter out non-ASCII characters from name for base64 encoding
            ascii_name = ''.join(c for c in name if ord(c) < 128)
            code += '"' + radium.toBase64(ascii_name) + '" '           # 0: name
            code += '"' + radium.toBase64(temp_path or "") + '" '      # 1: temp path (XI file)
            code += "#f "                                              # 2: placeholder for radium num
            code += str(int(vol)) + " "                                # 3: sample volume (0-64)
            code += str(int(fine)) + " "                               # 4: sample finetune (-128..127)
            code += str(int(loop_start)) + " "                         # 5: loop start (frames)
            code += str(int(loop_len)) + " "                           # 6: loop length (frames)
            code += ")"
        code += "))"
        radium.evalScheme(code)
        
        # Playlist
        code = "(set-xm-playlist! '("
        for p in playlist:
            code += " " + str(p) + " "
        code += "))"
        radium.evalScheme(code)
        
        # Pattern format
        code = "(set-xm-pattern-format " + str(num_channels) + " " + str(num_patterns) + ")"
        radium.evalScheme(code)
        
        # Default tempo/BPM
        code = "(set-xm-default-tempo! " + str(default_tempo) + " " + str(default_bpm) + ")"
        radium.evalScheme(code)
        
        # Patterns (including F effect rows with no note)
        for pat_idx, (num_rows, rows) in patterns.items():
            for (row, channel, note, instrument, volume, effect_type, effect_param) in rows:
                # XM notes: 0=no note, 1-96=C-0 to C-8, 97=note off
                # Radium expects MIDI-style notes
                if note > 0 and note <= 96:
                    # XM note 1 = C-0 = MIDI 12. Radium uses MIDI notes.
                    radium_note = note - 1 # + 11
                    code = "(add-xm-trackline " + str(pat_idx) + " "
                    code += str(channel) + " "
                    code += str(row) + " "
                    code += str(radium_note) + " "
                    code += str(instrument) + " "
                    code += str(volume) + " "
                    code += str(effect_type) + " "
                    code += str(effect_param) + ")"
                    radium.evalScheme(code)
                elif note == 97:
                    # XM note byte 97 = stop note (key-off): release note on this channel
                    code = "(add-xm-trackline " + str(pat_idx) + " "
                    code += str(channel) + " "
                    code += str(row) + " "
                    code += "97 "      # note=97 in Scheme triggers :stop
                    code += "0 "       # instrument=0
                    code += "0 "       # volume=0
                    code += "0 "       # effect_type=0
                    code += "0)"       # effect_param=0
                    radium.evalScheme(code)
                elif effect_type > 0 or volume > 0:
                    # Effect/volume-only row (e.g. F effect for speed, C for volume, or volume column byte)
                    code = "(add-xm-trackline " + str(pat_idx) + " "
                    code += str(channel) + " "
                    code += str(row) + " "
                    code += "0 "       # note=0
                    code += "0 "       # instrument=0
                    code += str(volume) + " "
                    code += str(effect_type) + " "
                    code += str(effect_param) + ")"
                    radium.evalScheme(code)
        
        radium.evalScheme('(stop-adding-xm-events!)')
        
    except Exception:
        e = sys.exc_info()[0]
        message = traceback.format_exc()
        print(message)
        radium.addMessage("Loading " + filename + " failed. " + str(e))
        if platform.system() == "Linux":
            radium.addMessage(message)
        else:
            for m in message.split("\n"):
                radium.addMessage(m)
    
    finally:
        if platform.system() != "Linux":
            sys.stdout = old_stdout
            sys.stderr = old_stderr


if __name__ == "__main__":
    import sys
    if len(sys.argv) > 1:
        import_xm(sys.argv[1])
    else:
        print("Usage: python import_xm2.py <filename>")
