/***************************************************************************
 *                                                                         *
 *   libgig - C++ cross-platform Gigasampler format file access library    *
 *                                                                         *
 *   Copyright (C) 2003-2009 by Christian Schoenebeck                      *
 *                              <cuse@users.sourceforge.net>               *
 *                                                                         *
 *   This program is part of libgig.                                       *
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

#include "DLS.h"

using namespace std;

string Revision();
void PrintVersion();
void PrintSamples(DLS::File* dls);
void PrintInstruments(DLS::File* dls);
void PrintRegions(DLS::Instrument* instr);
void PrintUsage();

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
        cout << "Invalid file argument!" << endl;
        return EXIT_FAILURE;
    }
    fclose(hFile);
    try {
        RIFF::File* riff = new RIFF::File(argv[1]);
        DLS::File*  dls  = new DLS::File(riff);
        if (dls->pInfo->Name != "") cout << "File Name: \"" << dls->pInfo->Name << "\"\n";
        PrintSamples(dls);
        cout << endl;
        PrintInstruments(dls);
        delete dls;
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

void PrintSamples(DLS::File* dls) {
    int samples = 0;
    cout << "ALL Available Samples (as there might be more than referenced by Instruments):" << endl;
    DLS::Sample* pSample = dls->GetFirstSample();
    while (pSample) {
        samples++;
        string name = pSample->pInfo->Name;
        if (name == "") name = "<NO NAME>";
        else            name = '\"' + name + '\"';
        cout << "    Sample " << samples << ") " << name << ", ";
        cout << pSample->SamplesPerSecond << "Hz, " << pSample->Channels << " Channels" << endl;
        pSample = dls->GetNextSample();
    }
}

void PrintInstruments(DLS::File* dls) {
    int instruments = 0;
    cout << "Available Instruments:" << endl;
    DLS::Instrument* pInstrument = dls->GetFirstInstrument();
    while (pInstrument) {
        instruments++;
        string name = pInstrument->pInfo->Name;
        if (name == "") name = "<NO NAME>";
        else            name = '\"' + name + '\"';
        cout << "    Instrument " << instruments << ") " << name << ", ";

        cout << " MIDIBank=" << pInstrument->MIDIBank << ", MIDIProgram=" << pInstrument->MIDIProgram << endl;
        PrintRegions(pInstrument);

        pInstrument = dls->GetNextInstrument();
    }
}

void PrintRegions(DLS::Instrument* instr) {
    int regions = 0;
    DLS::Region* pRegion = instr->GetFirstRegion();
    while (pRegion) {
        regions++;

        cout << "        Region " << regions << ") ";
        DLS::Sample* pSample = pRegion->GetSample();
        if (pSample) {
            cout << "Sample: ";
            if (pSample->pInfo->Name != "") {
                cout << "\"" << pSample->pInfo->Name << "\", ";
            }
            cout << pSample->SamplesPerSecond << "Hz, ";
        }
        else {
            cout << "<NO_VALID_SAMPLE_REFERENCE> ";
        }
        cout << "KeyRange=" << pRegion->KeyRange.low << "-" << pRegion->KeyRange.high << ", ";
        cout << "VelocityRange=" << pRegion->VelocityRange.low << "-" << pRegion->VelocityRange.high << ", Layer=" << pRegion->Layer << endl;
        cout << "            Loops=" << pRegion->SampleLoops << endl;

        pRegion = instr->GetNextRegion();
    }
}

string Revision() {
    string s = "$Revision: 1.6 $";
    return s.substr(11, s.size() - 13); // cut dollar signs, spaces and CVS macro keyword
}

void PrintVersion() {
    cout << "dlsdump revision " << Revision() << endl;
    cout << "using " << DLS::libraryName() << " " << DLS::libraryVersion() << endl;
}

void PrintUsage() {
    cout << "dlsdump - parses DLS (Downloadable Sounds) Level 1 and Level 2 files and prints out the content." << endl;
    cout << endl;
    cout << "Usage: dlsdump [-v] FILE" << endl;
    cout << endl;
    cout << "	-v  Print version and exit." << endl;
    cout << endl;
}
