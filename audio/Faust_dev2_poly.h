/* Copyright 2026 Kjetil S. Matheussen

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


#ifndef AUDIO_FAUST_DEV2_POLY_H
#define AUDIO_FAUST_DEV2_POLY_H

#include "../bin/packages/faust/architecture/faust/dsp/poly-dsp.h"


// One voice of polyphony. Same role as Faust's dsp_voice, but stores the
// pitch as a double so microtonal (fractional) notes survive the trip from
// note_t.pitch to the freq/key control zones. There is no legato state:
// voices are never stolen (nonstealing_microtonal_poly_dsp simply refuses
// new notes when all voices are busy), so fCurNote is either kFreeVoice,
// kReleaseVoice, or the (possibly fractional) pitch of the playing note.
struct FaustDev2PolyVoice : public MapUI, public decorator_dsp
{
	typedef std::function<double(double)> TransformFunction;

	// Convert MIDI note to frequency. Accepts fractional notes.
	static double midiToFreq(double note)
	{
		return 440.0 * std::pow(2.0, (note - 69.0) / 12.0);
	}

	double fCurNote;                     // kFreeVoice, kReleaseVoice, or the playing pitch
	int fDate;                           // Voice allocation date
	FAUSTFLOAT fLevel;                   // Last audio block level
	std::vector<std::string> fGatePath;  // Paths of 'gate' control
	std::vector<std::string> fGainPath;  // Paths of 'gain/vel|velocity' control
	std::vector<std::string> fFreqPath;  // Paths of 'freq/key' control
	TransformFunction fKeyFun;           // MIDI key to freq conversion function
	TransformFunction fVelFun;           // MIDI velocity to gain conversion function

	FaustDev2PolyVoice(::dsp *dsp)
		: decorator_dsp(dsp)
		, fCurNote(kFreeVoice)
		, fDate(0)
		, fLevel(FAUSTFLOAT(0))
	{
		fVelFun = [](double velocity) { return double(velocity) / 127.0; };
		fKeyFun = [](double pitch) { return midiToFreq(pitch); };
		dsp->buildUserInterface(this);
		extractPaths(fGatePath, fFreqPath, fGainPath);
	}

	// Extract control paths from fullpath map
	void extractPaths(std::vector<std::string> &gate,
					  std::vector<std::string> &freq,
					  std::vector<std::string> &gain)
	{
		// Keep gain/vel|velocity, freq/key and gate labels
		for (const auto &it : getFullpathMap())
		{
			std::string path = it.first;
			if (endsWith(path, "/gate"))
			{
				gate.push_back(path);
			}
			else if (endsWith(path, "/freq"))
			{
				fKeyFun = [](double pitch) { return midiToFreq(pitch); };
				freq.push_back(path);
			}
			else if (endsWith(path, "/key"))
			{
				fKeyFun = [](double pitch) { return pitch; };
				freq.push_back(path);
			}
			else if (endsWith(path, "/gain"))
			{
				fVelFun = [](double velocity) { return double(velocity) / 127.0; };
				gain.push_back(path);
			}
			else if (endsWith(path, "/vel") || endsWith(path, "/velocity"))
			{
				fVelFun = [](double velocity) { return double(velocity); };
				gain.push_back(path);
			}
		}
	}

	// KeyOn with normalized MIDI velocity [0..1]
	void keyOn(double pitch, double velocity)
	{
		for (size_t i = 0; i < fFreqPath.size(); i++)
			setParamValue(fFreqPath[i], fKeyFun(pitch));

		for (size_t i = 0; i < fGatePath.size(); i++)
			setParamValue(fGatePath[i], FAUSTFLOAT(1));

		for (size_t i = 0; i < fGainPath.size(); i++)
			setParamValue(fGainPath[i], velocity);

		fCurNote = pitch;
	}

	// Change the pitch of the playing note (glide / pitch-line automation).
	// Writes the same freq/key zones as keyOn, but deliberately does not
	// touch fCurNote: note-off events carry the original note-on pitch, so
	// fCurNote must keep the original value for note matching to work.
	void setPitch(double pitch)
	{
		for (size_t i = 0; i < fFreqPath.size(); i++)
			setParamValue(fFreqPath[i], fKeyFun(pitch));
	}

	void keyOff(bool hard = false)
	{
		// No use of velocity for now...
		for (size_t i = 0; i < fGatePath.size(); i++)
			setParamValue(fGatePath[i], FAUSTFLOAT(0));

		if (hard)
		{
			// Immediately stop voice
			fCurNote = kFreeVoice;
		}
		else
		{
			// Release voice
			fCurNote = kReleaseVoice;
		}
	}

	void instanceClear()
	{
		decorator_dsp::instanceClear();
		fCurNote = kFreeVoice;
		fLevel = FAUSTFLOAT(0);
		fDate = 0;
	}
};


// Radium's polyphonic wrapper around a Faust DSP, replacing Faust's
// mydsp_poly. Two differences from mydsp_poly:
//
//  1. Never steals voices. Faust's mydsp_poly steals the oldest voice when
//     all voices are busy, and the legato crossfade that comes with stealing
//     (kLegatoVoice) only runs during one audio block (64 frames by default),
//     which is far too short to fade cleanly and produces an audible click on
//     every steal. Instead of stealing, this class keeps the requested number
//     of voices and drops new notes when they are all busy (keyOn returns
//     NULL).
//
//  2. Supports microtonal notes. Pitches are doubles all the way from keyOn
//     to the voice's freq/key control zone, so fractional note_t.pitch values
//     (e.g. 60.5 = 50 cents above middle C) are not rounded to the nearest
//     semitone.
//
// The user interface tree matches mydsp_poly (grouped voices under a
// "Polyphonic" tab with a "Panic" button), so effect numbering, parameter
// addresses, saved state and the QTGUI dialog stay compatible.
class nonstealing_microtonal_poly_dsp : public decorator_dsp
{
private:
	FAUSTFLOAT **fMixBuffer; // Intermediate buffer for mixing voices
	FAUSTFLOAT **fOutBuffer; // Intermediate buffer for output

public:
	std::vector<FaustDev2PolyVoice *> fVoiceTable; // owned; one entry per voice
	FAUSTFLOAT fPanic;                             // Panic button value
	GroupUI fGroups;                               // GUI group for controlling voice parameters
	::dsp *fVoiceGroup;                            // owned; UI proxy for the first voice

	nonstealing_microtonal_poly_dsp(::dsp *dsp, int nvoices)
		: decorator_dsp(dsp)
		, fMixBuffer(NULL)
		, fOutBuffer(NULL)
		, fPanic(FAUSTFLOAT(0))
		, fGroups(&fPanic, panic, this)
		, fVoiceGroup(NULL)
	{
		for (int i = 0; i < nvoices; i++)
			fVoiceTable.push_back(new FaustDev2PolyVoice(dsp->clone()));

		fMixBuffer = new FAUSTFLOAT *[getNumOutputs()];
		fOutBuffer = new FAUSTFLOAT *[getNumOutputs()];
		for (int chan = 0; chan < getNumOutputs(); chan++)
		{
			fMixBuffer[chan] = new FAUSTFLOAT[MIX_BUFFER_SIZE];
			fOutBuffer[chan] = new FAUSTFLOAT[MIX_BUFFER_SIZE];
		}

		// Groups all uiItem for a given path (same wiring as Faust's
		// dsp_voice_group::init).
		fVoiceGroup = new proxy_dsp(fVoiceTable[0]);
		fVoiceGroup->buildUserInterface(&fGroups);
		for (size_t i = 0; i < fVoiceTable.size(); i++)
			fVoiceTable[i]->buildUserInterface(&fGroups);
	}

	virtual ~nonstealing_microtonal_poly_dsp()
	{
		for (size_t i = 0; i < fVoiceTable.size(); i++)
			delete fVoiceTable[i];

		delete fVoiceGroup;

		for (int chan = 0; chan < getNumOutputs(); chan++)
		{
			delete[] fMixBuffer[chan];
			delete[] fOutBuffer[chan];
		}
		delete[] fMixBuffer;
		delete[] fOutBuffer;
	}

	// Callback for the panic button
	static void panic(FAUSTFLOAT val, void *arg)
	{
		if (val == FAUSTFLOAT(1))
			static_cast<nonstealing_microtonal_poly_dsp *>(arg)->allNotesOff(true);
	}

	void buildUserInterface(UI *ui_interface)
	{
		if (fVoiceTable.size() > 1)
		{
			ui_interface->openTabBox("Polyphonic");
			ui_interface->openVerticalBox("Voices");
			ui_interface->addButton("Panic", &fPanic);
			fVoiceGroup->buildUserInterface(ui_interface);
			ui_interface->closeBox();
			ui_interface->closeBox();
		}
		else
		{
			fVoiceTable[0]->buildUserInterface(ui_interface);
		}
	}

	void init(int sample_rate)
	{
		decorator_dsp::init(sample_rate);
		fVoiceGroup->init(sample_rate);
		fPanic = FAUSTFLOAT(0);
		for (size_t i = 0; i < fVoiceTable.size(); i++)
			fVoiceTable[i]->init(sample_rate);
	}

	void instanceInit(int samplingFreq)
	{
		instanceConstants(samplingFreq);
		instanceResetUserInterface();
		instanceClear();
	}

	void instanceConstants(int sample_rate)
	{
		decorator_dsp::instanceConstants(sample_rate);
		fVoiceGroup->instanceConstants(sample_rate);
		for (size_t i = 0; i < fVoiceTable.size(); i++)
			fVoiceTable[i]->instanceConstants(sample_rate);
	}

	void instanceResetUserInterface()
	{
		decorator_dsp::instanceResetUserInterface();
		fVoiceGroup->instanceResetUserInterface();
		fPanic = FAUSTFLOAT(0);
		for (size_t i = 0; i < fVoiceTable.size(); i++)
			fVoiceTable[i]->instanceResetUserInterface();
	}

	void instanceClear()
	{
		decorator_dsp::instanceClear();
		fVoiceGroup->instanceClear();
		for (size_t i = 0; i < fVoiceTable.size(); i++)
			fVoiceTable[i]->instanceClear();
	}

	virtual nonstealing_microtonal_poly_dsp *clone()
	{
		return new nonstealing_microtonal_poly_dsp(fDSP->clone(), int(fVoiceTable.size()));
	}

	// Mix the audio from the mix buffer to the output buffer
	FAUSTFLOAT mixCheckVoice(int count, FAUSTFLOAT **mixBuffer, FAUSTFLOAT **outBuffer)
	{
		FAUSTFLOAT sumSquares = 0;
		int numOutputs = getNumOutputs();

		for (int chan = 0; chan < numOutputs; chan++)
		{
			FAUSTFLOAT *mixChannel = mixBuffer[chan];
			FAUSTFLOAT *outChannel = outBuffer[chan];
			for (int frame = 0; frame < count; frame++)
			{
				FAUSTFLOAT sample = mixChannel[frame];
				sumSquares += sample * sample;
				outChannel[frame] += sample;
			}
		}

		// RMS is sqrt of mean of sum of squares across all samples in all channels
		FAUSTFLOAT meanSquare = sumSquares / (count * numOutputs);
		return std::sqrt(meanSquare);
	}

	// Clear the audio buffer
	void clear(int count, FAUSTFLOAT **outBuffer)
	{
		for (int chan = 0; chan < getNumOutputs(); chan++)
			memset(outBuffer[chan], 0, count * sizeof(FAUSTFLOAT));
	}

	// Copy the audio from one buffer to another
	void copy(int count, FAUSTFLOAT **mixBuffer, FAUSTFLOAT **outBuffer)
	{
		for (int chan = 0; chan < getNumOutputs(); chan++)
			memcpy(outBuffer[chan], mixBuffer[chan], count * sizeof(FAUSTFLOAT));
	}

	// Get the index of the oldest voice currently playing a specific pitch
	int getPlayingVoice(double pitch)
	{
		int voice_playing = kNoVoice;
		int oldest_date_playing = INT_MAX;

		for (size_t i = 0; i < fVoiceTable.size(); i++)
		{
			double curNote = fVoiceTable[i]->fCurNote;
			if (curNote == pitch)
			{
				// Keeps oldest playing voice
				if (fVoiceTable[i]->fDate < oldest_date_playing)
				{
					oldest_date_playing = fVoiceTable[i]->fDate;
					voice_playing = int(i);
				}
			}
		}

		return voice_playing;
	}

	// Note-on. Returns the voice (a MapUI whose control zones can be updated
	// directly), or NULL if all voices are busy.
	MapUI *keyOn(int channel, double pitch, double velocity)
	{
		for (size_t i = 0; i < fVoiceTable.size(); i++)
		{
			FaustDev2PolyVoice *voice = fVoiceTable[i];
			if (voice->fCurNote == kFreeVoice)
			{
				voice->fDate++;
				voice->keyOn(pitch, voice->fVelFun(velocity));
				return voice;
			}
		}

		return NULL;
	}

	// Note-off. Releases the oldest voice playing 'pitch' (used as a fallback
	// when the caller has lost track of the exact voice).
	void keyOff(int channel, double pitch, int velocity = 127)
	{
		int voice = getPlayingVoice(pitch);
		if (voice != kNoVoice)
			fVoiceTable[voice]->keyOff();
	}

	// Terminate all active voices, gently or immediately (depending of 'hard' value).
	//
	// On the gentle path, only release voices that are actually in use. Free
	// voices must not be marked as releasing: compute() then starts running
	// them, and a voice with a never-played (or just reset) soundfile read
	// index plays its one-shot sample from the beginning, so instruments like
	// so.sound(...).play(gain, gate) would burst into sound when the player
	// stops. (keyOff(true) keeps free voices free, so the hard/panic path
	// stays unconditional.)
	void allNotesOff(bool hard = false)
	{
		for (size_t i = 0; i < fVoiceTable.size(); i++)
		{
			FaustDev2PolyVoice *voice = fVoiceTable[i];

			if (hard)
				voice->keyOff(true);
			else if (voice->fCurNote != kFreeVoice)
				voice->keyOff();
		}
	}

	void compute(int count, FAUSTFLOAT **inputs, FAUSTFLOAT **outputs)
	{
		assert(count <= MIX_BUFFER_SIZE);

		// First clear the intermediate fOutBuffer
		clear(count, fOutBuffer);

		// Mix all playing voices
		for (size_t i = 0; i < fVoiceTable.size(); i++)
		{
			FaustDev2PolyVoice *voice = fVoiceTable[i];
			if (voice->fCurNote != kFreeVoice)
			{
				voice->compute(count, inputs, fMixBuffer);
				voice->fLevel = mixCheckVoice(count, fMixBuffer, fOutBuffer);
				if (voice->fCurNote == kReleaseVoice && voice->fLevel < VOICE_STOP_LEVEL)
					voice->fCurNote = kFreeVoice;
			}
		}

		// Finally copy intermediate buffer to outputs
		copy(count, fOutBuffer, outputs);
	}

	void compute(double date_usec, int count, FAUSTFLOAT **inputs, FAUSTFLOAT **outputs)
	{
		compute(count, inputs, outputs);
	}
};

#endif
