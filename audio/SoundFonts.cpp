/* Copyright 2012 Kjetil S. Matheussen

This program is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License
as published by the Free Software Foundation; either version 2
of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA. */



#include <iostream>
#include <cstdlib>
#include <string>
#include <sstream>

#include <QFile>

#if __GNUC__ >= 5
#  pragma GCC diagnostic push
#  pragma GCC diagnostic ignored "-Wsuggest-override"
#endif
#include "../bin/packages/libgig/src/SF.h"
#if __GNUC__ >= 5
#  pragma GCC diagnostic pop
#endif

#define DEBUG_ALLOWED 1 // <- SF.h sets DEBUG to 0
#include "../common/nsmtracker.h"

#include "../common/hashmap_proc.h"
#include "../common/visual_proc.h"
#include "../common/read_binary.h"

#include "SoundFonts_proc.h"

using namespace std;

// Taken from sf2dump.cpp by Grigor Iliev
template<class T> static inline std::string ToString(T o) {
    std::stringstream ss;
    ss << o;
    return ss.str();
}


// Taken from sf2dump.cpp by Grigor Iliev
template<class T> static inline string GetValue(T val) {
  if (val == (T)sf2::NONE) return "NONE";
    return ToString(val);
}

// Taken from sf2dump.cpp by Grigor Iliev.
static string GetSampleType(uint16_t type) {
    switch(type) {
        case sf2::Sample::MONO_SAMPLE       : return "Mono Sample";
        case sf2::Sample::RIGHT_SAMPLE      : return "Right Sample";
        case sf2::Sample::LEFT_SAMPLE       : return "Left Sample";
        case sf2::Sample::LINKED_SAMPLE     : return "Linked Sample";
        case sf2::Sample::ROM_MONO_SAMPLE   : return "ROM Mono Sample";
        case sf2::Sample::ROM_RIGHT_SAMPLE  : return "ROM Right Sample";
        case sf2::Sample::ROM_LEFT_SAMPLE   : return "ROM Left Sample";
        case sf2::Sample::ROM_LINKED_SAMPLE : return "ROM Linked Sample";
        default: return "Unknown";
    }
}


#if 0

// PrintPeresets/PrintRegion/PrintSamples/PrintInstruments is from sf2dump.cpp by Grigor Iliev.
// This code is practical to look at. It's kept here to avoid spending time finding the sf2dump.cpp file when editing this file.

void PrintPeresets(sf2::File* sf) {
    cout << "Presets (" << sf->GetPresetCount() << "): " << endl;
    for (int i = 0; i < sf->GetPresetCount(); i++) { /* exclude the terminal header - EOP */
        sf2::Preset* p = sf->GetPreset(i);
        cout << "\t" << p->Name << " (Preset: " << p->PresetNum << ", Bank: " << p->Bank;
        cout << ", Preset bag: " << p->PresetBagNdx << ")" << endl;

        if (p->pGlobalRegion) PrintRegion(-1, p->pGlobalRegion);

        for (int j = 0; j < p->GetRegionCount(); j++) {
            PrintRegion(j, p->GetRegion(j));
        }
        cout << endl;
    }
}

void PrintRegion(int idx, sf2::Region* reg) {
    if (idx == -1) cout << "\t\tGlobal Region " << endl;
    else cout << "\t\tRegion " << idx << endl;
    sf2::Sample* s = reg->GetSample();
    if (s != NULL) {
        cout << "\t\t    Sample: " << s->Name << ", Fine Tune: " << reg->fineTune;
        if (reg->coarseTune) cout  << ", Coarse Tune: " << reg->coarseTune;
        if (reg->overridingRootKey != -1) cout  << ", Overriding Root Key: " << reg->overridingRootKey;
        if (reg->HasLoop) {
            cout << ", Loop Start: " << reg->LoopStart << ", Loop End: " << reg->LoopEnd;
        }
        cout << endl;
    }
    cout << "\t\t    Key range=";
    if (reg->loKey == (int)::sf2::NONE && reg->hiKey == (int)::sf2::NONE) cout << "None";
    else cout << reg->loKey << "-" << reg->hiKey;
    cout << ", Velocity range=";
    if (reg->minVel == (int)::sf2::NONE && reg->maxVel == (int)::sf2::NONE) cout << "None";
    else cout << reg->minVel << "-" << reg->maxVel;
    cout << endl;

    cout << "\t\t    Initial cutoff frequency=";
    if (reg->initialFilterFc == (int)::sf2::NONE) cout << "None" << endl;
    else cout << reg->initialFilterFc << "cents" << endl;

    cout << "\t\t    Initial resonance=";
    if (reg->initialFilterQ == (int)::sf2::NONE) cout << "None" << endl;
    else cout << (reg->initialFilterQ / 10.0) << "dB" << endl;

    if (reg->exclusiveClass) cout << ", Exclusive group=" << reg->exclusiveClass;
    cout << endl;

    if (reg->pInstrument != NULL) {
        cout << "\t\t    Instrument: " << reg->pInstrument->Name << endl << endl;
    }

    cout << "\t\t    Volume Envelope Generator" << endl;
    cout << "\t\t\tEG1PreAttackDelay=" << GetValue(reg->GetEG1PreAttackDelay());
    cout << "s, EG1Attack=" << GetValue(reg->GetEG1Attack());
    cout << "s, EG1Hold=" << GetValue(reg->GetEG1Hold()) << "s, EG1Decay=";
    cout << GetValue(reg->GetEG1Decay()) << "s,  EG1Sustain=" << GetValue(reg->GetEG1Sustain() / 10);
    cout << "dB, EG1Release=" << GetValue(reg->GetEG1Release()) << "s" << endl << endl;

    cout << "\t\t    Modulation Envelope Generator" << endl;
    cout << "\t\t\tEG2PreAttackDelay=" << GetValue(reg->GetEG2PreAttackDelay());
    cout << "s, EG2Attack=" << GetValue(reg->GetEG2Attack());
    cout << "s, EG2Hold=" << GetValue(reg->GetEG2Hold()) << "s, EG2Decay=";
    cout << GetValue(reg->GetEG2Decay()) << "s,  EG2Sustain=";
    cout << GetValue(reg->GetEG2Sustain()) << "permille, EG2Release=";
    cout << GetValue(reg->GetEG2Release()) << "s" << endl;
    cout << "\t\t\tPitch=" << GetValue(reg->modEnvToPitch) << "cents, Cutoff=";
    cout << GetValue(reg->modEnvToFilterFc) << "cents" << endl << endl;

#if 0
     cout << "\t\t    Modulation LFO: Delay=" << ::sf2::ToSeconds(reg->delayModLfo) << "s, Frequency=";
     cout << ::sf2::ToHz(reg->freqModLfo) << "Hz, LFO to Volume=" << (reg->modLfoToVolume / 10) << "dB";
     cout << ", LFO to Filter Cutoff=" << reg->modLfoToFilterFc;
     cout << ", LFO to Pitch=" << reg->modLfoToPitch << endl;

     cout << "\t\t    Vibrato LFO:    Delay=" << ::sf2::ToSeconds(reg->delayVibLfo) << "s, Frequency=";
     cout << ::sf2::ToHz(reg->freqVibLfo) << "Hz, LFO to Pitch=" << reg->vibLfoToPitch << endl;

    cout << "\t\t\tModulators (" << reg->modulators.size() << ")" << endl;

    for (unsigned int i = 0; i < reg->modulators.size(); i++) {
        cout << "\t\t\tModulator " << i << endl;
        PrintModulatorItem(&reg->modulators[i]);
    }
#endif
}


void PrintSamples(sf2::File* sf) {
    cout << "Samples (" << sf->GetSampleCount() << "): " << endl;
    for (int i = 0; i < sf->GetSampleCount(); i++) {
        sf2::Sample* s = sf->GetSample(i);
        cout << "\t" << s->Name << " (Depth: " << ((s->GetFrameSize() / s->GetChannelCount()) * 8);
        cout << ", SampleRate: " << s->SampleRate;
        cout << ", Pitch: " << ((int)s->OriginalPitch);
        cout << ", Pitch Correction: " << ((int)s->PitchCorrection )<< endl;
        cout << "\t\tStart: " << s->Start << ", End: " << s->End;
        cout << ", Start Loop: " << s->StartLoop << ", End Loop: " << s->EndLoop << endl;
        cout << "\t\tSample Type: " << GetSampleType(s->SampleType) << ", Sample Link: " << s->SampleLink << ")" << endl;
    }
}

void PrintInstruments(sf2::File* sf) {
    cout << "Instruments (" << sf->GetInstrumentCount() << "): " << endl;
    for (int i = 0; i < sf->GetInstrumentCount(); i++) {
        sf2::Instrument* instr = sf->GetInstrument(i);
        cout << "\t" << instr->Name << " (";
        cout << "Instrument bag: " << instr->InstBagNdx << ")" << endl;
        cout << "\t    Regions (" << instr->GetRegionCount() << ")" << endl;

        if (instr->pGlobalRegion) PrintRegion(-1, instr->pGlobalRegion);

        for (int j = 0; j < instr->GetRegionCount(); j++) {
            PrintRegion(j, instr->GetRegion(j));
        }
        cout << endl;
    }
}
#endif // 0


static hash_t *get_region_info(sf2::Region *reg){
  hash_t *info = HASH_create(10);

  if (reg->loKey == (int)::sf2::NONE && reg->hiKey == (int)::sf2::NONE){
    HASH_put_int(info, "key start", 0);
    HASH_put_int(info, "key end", 127);
  }else{
    HASH_put_int(info, "key start", reg->loKey);
    HASH_put_int(info, "key end", reg->hiKey);
  }

  if (reg->pInstrument != NULL)
    HASH_put_chars(info, "instrument", reg->pInstrument->Name.c_str());

  sf2::Sample* s = reg->GetSample();
  if(s!=NULL){
    HASH_put_chars(info, "sample_name", s->Name.c_str());
    HASH_put_int(info, "fine tune", reg->fineTune);
    HASH_put_int(info, "coarse tune", reg->coarseTune);
    HASH_put_int(info, "root key", reg->GetUnityNote()); // TODO: 60 might be wrong. Maybe it's 48? (it's 48)
    if(reg->HasLoop){
      HASH_put_int(info, "loop start", reg->LoopStart); // What's the difference between these and sample->loopStart/loopEnd?
      HASH_put_int(info, "loop end", reg->LoopEnd);
    }
  }else{
    HASH_put_chars(info, "sample_name", "<no sample!>");
  }

  HASH_put_float(info, "attack", reg->GetEG1Attack());
  HASH_put_float(info, "hold", reg->GetEG1Hold());
  HASH_put_float(info, "decay", reg->GetEG1Decay());
  HASH_put_float(info, "sustain", reg->GetEG1Sustain());
  HASH_put_float(info, "release", reg->GetEG1Release());

  return info;
}

static hash_t *get_instrument_info(sf2::Instrument *instr){
  hash_t *info = HASH_create(5);
  HASH_put_chars(info, "name", instr->Name.c_str());
  HASH_put_int(info, "bag", instr->InstBagNdx);

  hash_t *regions = HASH_create(instr->GetRegionCount());
  HASH_put_hash(info, "regions", regions);
  for(int i=0;i<instr->GetRegionCount();i++)
    HASH_put_hash_at(regions,"",i,get_region_info(instr->GetRegion(i)));

  return info;
}

static hash_t *get_instruments_info(sf2::File *file){
  hash_t *instruments = HASH_create(file->GetInstrumentCount());

  for (int i = 0; i < file->GetInstrumentCount(); i++) {
    sf2::Instrument* instr = file->GetInstrument(i);
    HASH_put_hash(instruments, instr->Name.c_str(), get_instrument_info(instr));
  }

  return instruments;
}

static hash_t *get_preset_info(sf2::Preset *preset){
  hash_t *info = HASH_create(5);
  HASH_put_chars(info,"name",preset->Name.c_str());
  HASH_put_int(info,"bank",preset->Bank);
  HASH_put_int(info,"num",preset->PresetNum);
  HASH_put_int(info,"bag",preset->PresetBagNdx); // what is this?
  
  {
    hash_t *regions = HASH_create(2);
    HASH_put_hash(info,"regions",regions);

    if(preset->pGlobalRegion)
      HASH_put_hash_at(regions, "", preset->GetRegionCount(), get_region_info(preset->pGlobalRegion));

    for(int i=0 ; i<preset->GetRegionCount() ; i++)
      HASH_put_hash_at(regions, "", i, get_region_info(preset->GetRegion(i)));
  }

  return info;
}

static hash_t *get_presets_info(sf2::File *file){
  hash_t *presets = HASH_create(file->GetPresetCount());

  for(int i=0;i<file->GetPresetCount();i++){
    sf2::Preset *preset = file->GetPreset(i);
    HASH_put_hash_at(presets, "", preset->PresetBagNdx, get_preset_info(preset));
  }
  
  return presets;
}

static hash_t *get_sample_info(sf2::Sample *sample, int i){
  hash_t *info = HASH_create(5);
  HASH_put_int(info,"num",i);
  HASH_put_int(info,"num_bits",(sample->GetFrameSize() / sample->GetChannelCount()) * 8); // always 16, i think, at least according to soundfont spec
  HASH_put_int(info,"num_frames",sample->GetTotalFrameCount());
  HASH_put_int(info,"num_channels",sample->GetChannelCount());
  HASH_put_int(info,"samplerate",sample->SampleRate);
  HASH_put_int(info,"loop start",sample->StartLoop - sample->Start);
  HASH_put_int(info,"loop end",sample->EndLoop - sample->Start);
  HASH_put_int(info,"pitch",sample->OriginalPitch);
  HASH_put_int(info,"pitch correction",sample->PitchCorrection);
  HASH_put_chars(info,"type",GetSampleType(sample->SampleType).c_str());
  HASH_put_int(info,"sibling",sample->SampleLink);
  return info;
}

static hash_t *get_samples_info(sf2::File *file){
  hash_t *samples = HASH_create(50);
  for(int i=0; i<file->GetSampleCount();i++){
    sf2::Sample *sample = file->GetSample(i);
    HASH_put_hash(samples, sample->Name.c_str(), get_sample_info(sample, i));
  }
  return samples;
}

static hash_t *get_menu(hash_t *info){
  hash_t   *menu        = HASH_create(50);

  dynvec_t  presets     = HASH_get_values(HASH_get_hash(info,"presets"));
  int       num_presets = presets.num_elements;

  for(int i=0;i<num_presets;i++){
    const dyn_t dyn             = presets.elements[i];
    R_ASSERT_RETURN_IF_FALSE2(dyn.type==HASH_TYPE, menu);

    hash_t      *preset         = dyn.hash;
    int          preset_num     = HASH_get_int32(preset,"num");
    int          bank_num       = HASH_get_int32(preset,"bank");
    const char  *preset_name    = HASH_get_chars(preset,"name");

    char bank_display[512];
    {
      sprintf(bank_display,"Bank %d",bank_num);

      if(HASH_has_key(menu,bank_display)==false){
        HASH_put_hash(menu,bank_display,HASH_create(num_presets));
        printf("Creating new hash entry for \"%s\"\n",bank_display);
      }
    }
    hash_t *bank = HASH_get_hash(menu,bank_display);

    {
      char preset_display[512];
      //sprintf(filename,"%s.%0*d.wav",base_filename,leading_zeros+1,0);
      sprintf(preset_display,"%03d. %s",preset_num,preset_name);
      HASH_put_hash(bank,preset_display,preset);
    }
  }

  return menu;
}

// samples are stored by name
// instruments are stored by name
// presets are stored by preset bag (the preset bag seems to be a unique number for the preset. Very practical, since presets names and preset numbers doesn't have to be unique.)
// menues are organized by a list of banks, and then each bank has a list of presets.
//
// regions are stored within both instruments and presets. They are stored as an array within each of those.
//
hash_t *SF2_get_info(filepath_t filename){
  try {

    const char *osfilename = talloc_strdup(QFile::encodeName(STRING_get_qstring(filename.id)).constData());
#if defined(FOR_WINDOWS)
    RIFF::File riff(filename.id, std::string(osfilename));
#else
    RIFF::File riff(osfilename);
#endif
    
    sf2::File file(&riff);
      
    hash_t *hash = HASH_create(5);
    HASH_put_hash(hash,"samples",get_samples_info(&file));
    HASH_put_hash(hash,"instruments",get_instruments_info(&file));
    HASH_put_hash(hash,"presets",get_presets_info(&file));
    HASH_put_hash(hash,"menu",get_menu(hash));
    return hash;

  }catch (RIFF::Exception &e) {
    //GFX_Message(NULL,"Unable to parse soundfont file %s: %s", STRING_get_chars(filename), e.Message.c_str());
    fprintf(stderr, "SoundFonts.cpp: Caught RIFF::Exception exception:\n");
    e.PrintMessage();
  }catch (...) {
    GFX_Message(NULL,"Unknown exception while trying to parse file: %S", filename.id);
  }

  return NULL;
}

#if 0
hash_t *SF2_get_displayable_preset_names(hash_t *info){
  hash_t *presets = HASH_get_hash(info,"presets");
  
  int num_presets = HASH_get_array_size(presets, "");
  hash_t *displayable_names = HASH_create(num_presets);

  for(int i=0;i<num_presets;i++){
    hash_t *preset = HASH_get_hash_at(presets,"",i);
    char display[512];
    //sprintf(filename,"%s.%0*d.wav",base_filename,leading_zeros+1,0);
    sprintf(display,"%03d. %s",(int)HASH_get_int(preset,"num"),HASH_get_chars(preset,"name"));
    HASH_put_chars_at(displayable_names,"",i,display);
  }

  return displayable_names;
}
#endif


float *SF2_load_sample(filepath_t filename, int sample_num){
  const char *osfilename = talloc_strdup(QFile::encodeName(STRING_get_qstring(filename.id)).constData());
#if defined(FOR_WINDOWS)
  RIFF::File riff(filename.id, std::string(osfilename));
#else
  RIFF::File riff(osfilename);
#endif

  sf2::File file(&riff);
  
  sf2::Sample *sample = file.GetSample(sample_num);
  sf2::Sample::buffer_t buffer = sample->LoadSampleData();

  int16_t *s16=(int16_t*)buffer.pStart;

  int num_frames = int(buffer.Size / sizeof(int16_t));

  float *ret=(float*)V_calloc((int)sizeof(float),num_frames);
  if(ret==NULL){
    GFX_Message(NULL, "Out of memory? Failed to allocate %d bytes\n",num_frames*4);
    return NULL;
  }

  for(int i=0;i<num_frames;i++){
    int16_t val = get_le_16((unsigned char*)&s16[i]);
    ret[i] = val / 32768.0f;
  }

  sample->ReleaseSampleData();
  file.DeleteSample(sample);

  return ret;
}

