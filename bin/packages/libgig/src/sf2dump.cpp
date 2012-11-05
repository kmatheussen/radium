/***************************************************************************
 *                                                                         *
 *   libgig - C++ cross-platform Gigasampler format file access library    *
 *                                                                         *
 *   Copyright (C) 2003-2009 by Christian Schoenebeck                      *
 *                              <cuse@users.sourceforge.net>               *
 *   Copyright (C) 2009 by Grigor Iliev  <grigor@grigoriliev.com>          *
 *                                                                         *
 *   This program is part of libsf2.                                       *
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 *   This program is distributed in the hope that it will be useful,       *
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of        *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *
 *   GNU General Public License for more details.                          *
 *                                                                         *
 *   You should have received a copy of the GNU General Public License     *
 *   along with this program; if not, write to the Free Software           *
 *   Foundation, Inc., 59 Temple Place, Suite 330, Boston,                 *
 *   MA  02111-1307  USA                                                   *
 ***************************************************************************/

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <iostream>
#include <cstdlib>
#include <string>
#include <sstream>

#include "SF.h"
#include "helper.h"

using namespace std;

string Revision();
void PrintVersion();
void PrintSamples(sf2::File* sf);
void PrintInstruments(sf2::File* sf);
void PrintPeresets(sf2::File* sf);
void PrintRegion(int idx, sf2::Region* reg);
void PrintModulatorItem(sf2::ModulatorItem* mod);
void PrintModulator(sf2::Modulator& mod);
void PrintUsage();
void PrintSfInfo(sf2::File* sf);

string GetControllerType(sf2::Modulator& mod);
string GetControllerSource(sf2::Modulator& mod);
string GetSampleType(uint16_t type);

template<class T> inline string GetValue(T val) {
    if (val == sf2::NONE) return "NONE";
    return ToString(val);
}

int main(int argc, char *argv[])
{
    if (argc <= 1) {
        PrintUsage();
        return EXIT_FAILURE;
    }
    if (argv[1][0] == '-') {
        switch (argv[1][1]) {
            case 'v':
                PrintVersion();
                return EXIT_SUCCESS;
        }
    }
    FILE* hFile = fopen(argv[1], "r");
    if (!hFile) {
        cout << "Invalid file argument!" << endl << endl;
        void PrintUsage();
        return EXIT_FAILURE;
    }
    fclose(hFile);
    try {
        RIFF::File* riff = new RIFF::File(argv[1]);
        sf2::File*  sf  = new sf2::File(riff);
        PrintSfInfo(sf);
        PrintSamples(sf);
        cout << endl;
        PrintInstruments(sf);
        PrintPeresets(sf);
        delete sf;
        delete riff;
    }
    catch (RIFF::Exception e) {
        e.PrintMessage();
        return EXIT_FAILURE;
    }
    catch (...) {
        cout << "Unknown exception while trying to parse file." << endl;
        return EXIT_FAILURE;
    }

    return EXIT_SUCCESS;
}

void PrintSfInfo(sf2::File* sf) {
    cout << "File info:" << endl;
    cout << "\tVersion: " << sf->pInfo->pVer->Major << "." << sf->pInfo->pVer->Minor << endl;
    cout << "\tBank Name: " << sf->pInfo->BankName << endl;
    cout << "\tSound Engine: " << sf->pInfo->SoundEngine << endl;
    cout << "\tSound ROM Name: " << sf->pInfo->RomName << endl;
    cout << "\tSound ROM Version: " << sf->pInfo->pRomVer->Major << "." << sf->pInfo->pRomVer->Minor << endl;
    cout << "\tCreation Date: " << sf->pInfo->CreationDate << endl;
    cout << "\tEngineers: " << sf->pInfo->Engineers << endl;
    cout << "\tProduct: " << sf->pInfo->Product << endl;
    cout << "\tCopyright: " << sf->pInfo->Copyright << endl;
    cout << "\tComments: " << sf->pInfo->Comments << endl;
    cout << "\tSoftware: " << sf->pInfo->Software << endl << endl;
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
    if (reg->loKey == ::sf2::NONE && reg->hiKey == ::sf2::NONE) cout << "None";
    else cout << reg->loKey << "-" << reg->hiKey;
    cout << ", Velocity range=";
    if (reg->minVel == ::sf2::NONE && reg->maxVel == ::sf2::NONE) cout << "None";
    else cout << reg->minVel << "-" << reg->maxVel;
    cout << endl;

    cout << "\t\t    Initial cutoff frequency=";
    if (reg->initialFilterFc == ::sf2::NONE) cout << "None" << endl;
    else cout << reg->initialFilterFc << "cents" << endl;

    cout << "\t\t    Initial resonance=";
    if (reg->initialFilterQ == ::sf2::NONE) cout << "None" << endl;
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

     cout << "\t\t    Modulation LFO: Delay=" << ::sf2::ToSeconds(reg->delayModLfo) << "s, Frequency=";
     cout << ::sf2::ToHz(reg->freqModLfo) << "Hz, LFO to Volume=" << (reg->modLfoToVolume / 10) << "dB";
     cout << ", LFO to Filter Cutoff=" << reg->modLfoToFilterFc;
     cout << ", LFO to Pitch=" << reg->modLfoToPitch << endl;

     cout << "\t\t    Vibrato LFO:    Delay=" << ::sf2::ToSeconds(reg->delayVibLfo) << "s, Frequency=";
     cout << ::sf2::ToHz(reg->freqVibLfo) << "Hz, LFO to Pitch=" << reg->vibLfoToPitch << endl;

    cout << "\t\t\tModulators (" << reg->modulators.size() << ")" << endl;

    for (int i = 0; i < reg->modulators.size(); i++) {
        cout << "\t\t\tModulator " << i << endl;
        PrintModulatorItem(&reg->modulators[i]);
    }
}

void PrintModulatorItem(sf2::ModulatorItem* mod) {
    cout << "\t\t\t    ModSrcOper" << endl;
    PrintModulator(mod->ModSrcOper);

    cout << "\t\t\t    ModAmtSrcOper" << endl;
    PrintModulator(mod->ModAmtSrcOper);
    cout << "\t\t\t    Amount: " << mod->ModAmount << endl;

    if (mod->ModDestOper & (1 << 15)) {
        cout << "\t\t\t    ModDestOper: " << (mod->ModDestOper ^ (1 << 15)) << endl;
    } else {
        cout << "\t\t\t    ModDestOper: " << mod->ModDestOper << endl;
    }
}

void PrintModulator(sf2::Modulator& mod) {
    cout << "\t\t\t\tController Type:    " << GetControllerType(mod) << endl;
    cout << "\t\t\t\tController Source:  " << GetControllerSource(mod) << endl;
    cout << "\t\t\t\tDirection:          ";
    cout << (mod.Direction ? "max -> min" : "min -> max") << endl;
    cout << "\t\t\t\tPolarity:           ";
    cout << (mod.Polarity ? "Bipolar" : "Unipolar") << endl;
}

string GetControllerType(sf2::Modulator& mod) {
    string s;
    switch(mod.Type) {
        case sf2::Modulator::LINEAR:
            s = "Linear"; break;
        case sf2::Modulator::CONCAVE:
            s = "Concave"; break;
        case sf2::Modulator::CONVEX:
            s = "Convex"; break;
        case sf2::Modulator::SWITCH:
            s = "Switch"; break;
    }

    return s;
}

string GetControllerSource(sf2::Modulator& mod) {
    if (mod.MidiPalete) {
        stringstream ss;
        ss << "MIDI controller " << mod.Index;
        return ss.str();
    }

    string s;
    switch(mod.Index) {
        case sf2::Modulator::NO_CONTROLLER:
            s = "No controller"; break;
        case sf2::Modulator::NOTE_ON_VELOCITY:
            s = "Note-On Velocity"; break;
        case sf2::Modulator::NOTE_ON_KEY_NUMBER:
            s = "Note-On Key Number"; break;
        case sf2::Modulator::POLY_PRESSURE:
            s = "Poly Pressure"; break;
        case sf2::Modulator::CHANNEL_PRESSURE:
            s = "Channel Pressure"; break;
        case sf2::Modulator::PITCH_WHEEL:
            s = "Pitch Wheel"; break;
        case sf2::Modulator::PITCH_WHEEL_SENSITIVITY:
            s = "Pitch Wheel Sensitivity"; break;
        case sf2::Modulator::LINK:
            s = "Link"; break;
        default: s = "Unknown controller source";
    }

    return s;
}

string Revision() {
    string s = "$Revision: 1.3 $";
    return s.substr(11, s.size() - 13); // cut dollar signs, spaces and CVS macro keyword
}

void PrintVersion() {
    cout << "sf2dump revision " << Revision() << endl;
    cout << "using " << sf2::libraryName() << " " << sf2::libraryVersion() << endl;
}

void PrintUsage() {
    cout << "sf2dump - parses SF2 files and prints out the content." << endl;
    cout << endl;
    cout << "Usage: sf2dump [-v] FILE" << endl;
    cout << endl;
    cout << "	-v  Print version and exit." << endl;
    cout << endl;
}

string GetSampleType(uint16_t type) {
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
