
#include "SoundFonts_proc.h"

#include "../common/hashmap_proc.h"


static bool load_sf2_instrument(Data *data, filepath_t filename, int preset_bag_number, bool set_loop_on_off)
{
	EVENTLOG_add_event(talloc_format("load_sf2_instrument -%S-", filename.id));
    
	hash_t *info = SF2_get_info(filename);
	if(info==NULL)
	{
		//GFX_Message(NULL, "Unable to open soundfont file \"%S\"\n", filename.id);
		return false;
	}

	//HASH_save(info,stdout);
	hash_t *sample_infos = HASH_get_hash(info,"samples");

	hash_t *presets = HASH_get_hash(info,"presets");
	hash_t *preset = HASH_get_hash_at(presets, "", preset_bag_number);
	if(preset==NULL)
	{
		GFX_Message(NULL, "No such preset number %d in instrument \"%S\"\n", preset_bag_number, filename.id);
		return false;
	}

	int bank_num = HASH_get_int32(preset,"bank");

	// A preset may use several instruments (e.g. drum kits split drums across
	// instruments). We need to load all of them, not just the first.
	hash_t *instruments = HASH_get_hash(info, "instruments");
	hash_t *preset_regions = HASH_get_hash(preset, "regions");

	for(int i=0;i<128;i++)
	{
		Note *note = new Note;
		data->notes[i] = note;
		data->note_storage.push_back(note);
	}

	int num_samples = 0;

	bool is_percussion = data->sf2_midi_note_convention && (bank_num == 128);
	int key_offset = data->sf2_midi_note_convention ? -12 : 0;

	int num_pr = HASH_get_array_size(preset_regions, "");

	for(int pr = 0; pr < num_pr; pr++)
	{
		hash_t *pregion = HASH_get_hash_at(preset_regions, "", pr);

		if (HASH_has_key(pregion, "instrument") == false)
			continue;

		const char *instr_name = HASH_get_chars(pregion, "instrument");
		hash_t *instrument = HASH_get_hash(instruments, instr_name);
		if (instrument == NULL)
			continue;

		hash_t *regions = HASH_get_hash(instrument, "regions");
		int num_regions = HASH_get_array_size(regions, "");

		// Pitch overrides from this preset region (e.g. overridingRootKey/fixedKey)
		int pr_root_key_set = HASH_has_key(pregion, "root key");
		int pr_root_key = pr_root_key_set ? HASH_get_int32(pregion, "root key") : 0;
		int pr_coarsetune = HASH_get_int32(pregion, "coarse tune");
		int pr_finetune  = HASH_get_int32(pregion, "fine tune");

		for(int ir = 0; ir < num_regions; ir++)
		{
			hash_t *region = HASH_get_hash_at(regions, "", ir);
			const char *sample_name = HASH_get_chars(region, "sample_name");

			if (!strcmp(sample_name, "<no sample!>"))
				continue;

			hash_t *sample_info = HASH_get_hash(sample_infos, sample_name);
			int sample_num = HASH_get_int32(sample_info, "num");

			Sample &sample = data->samples[num_samples++];
			
			R_ASSERT(num_samples < MAX_NUM_SAMPLES); // For now. TODO: Handle better.

			sample.data = data;
			sample.volume = 1.0f;
			sample.num_frames = HASH_get_int(sample_info, "num_frames");

			set_legal_loop_points(sample, -1, -1, set_loop_on_off); // By default, loop all.
			set_legal_loop_points(sample,
				HASH_get_int(sample_info, "loop start"),
				HASH_get_int(sample_info, "loop end"),
				set_loop_on_off);

			{
				const char *type = HASH_get_chars(sample_info, "type");
				if(!strcmp(type,"Left Sample") || !strcmp(type,"ROM Left Sample"))
					sample.ch = 0;
				else if(!strcmp(type,"Right Sample") || !strcmp(type,"ROM Right Sample"))
					sample.ch = 1;
				else
					sample.ch = -1;
			}

			sample.sound = SF2_load_sample(filename, sample_num, sample.ch);

			int root_key = pr_root_key_set ? pr_root_key : HASH_get_int32(region, "root key");
			int coarsetune = pr_coarsetune + HASH_get_int32(region, "coarse tune");
			int finetune   = pr_finetune   + HASH_get_int32(region, "fine tune");
			int pitch_correction = HASH_get_int32(sample_info, "pitch correction");

			for(int note = 0; note < 128; note++)
			{
				if (is_percussion || HASH_get_int(sample_info, "pitch") == 255)
				{
					sample.frequency_table[note] = HASH_get_int(sample_info, "samplerate");
				}
				else
				{
					// SoundFont OriginalPitch uses MIDI note numbering (C4=60), Radium
					// uses tracker numbering (C4=48). The note_offset converts between them.
					int note_offset = data->sf2_midi_note_convention ? 12 : 0;
					sample.frequency_table[note] = HASH_get_int(sample_info, "samplerate")
						* midi_to_hz(note + note_offset + coarsetune + (finetune + pitch_correction) / 100.0f)
						/ midi_to_hz(root_key);
				}
			}

			int key_start = HASH_get_int32(region, "key start") + key_offset;
			int key_end   = HASH_get_int32(region, "key end")   + key_offset;

			if (key_start < 0)   key_start = 0;
			if (key_end   > 127) key_end   = 127;

			for(int note_num = key_start; note_num <= key_end; note_num++)
			{
				Note *note = const_cast<Note*>(data->notes[note_num]);
				note->samples.push_back(&sample);
			}
		}
	}

	if (num_samples == 0)
	{
		GFX_Message(NULL, "load_sf2_instrument: No samples found in preset \"%S\"\n",
		            HASH_get_string(preset, "name"));
		return false;
	}

	printf("   load_sf2: Preset \"%S\" (bank %d) loaded %d samples.\n",
	       HASH_get_string(preset, "name"), bank_num, num_samples);
	
	for(int i=0;i<128;i++)
	{
		Note *old_note = const_cast<Note*>(data->notes[i]);
		old_note->sort_samples();
    
		for(int i2=0;i2<i;i2++)
		{
			if(data->notes[i]->is_equal(data->notes[i2]))
			{        
				data->notes[i] = data->notes[i2];

				data->note_storage.remove(old_note);
				delete old_note;

				printf("   Load SF2: Move notes %d to %d. Size of note_storage: %d\n", i, i2, data->note_storage.size());

				break;
			}
		}				   
	}


	return true;
}
