/***************************************************************************
 *                                                                         *
 *   libsf2 - C++ cross-platform SF2 format file access library            *
 *                                                                         *
 *   Copyright (C) 2009-2010 Grigor Iliev  <grigor@grigoriliev.com>,       *
 *    Christian Schoenebeck and Andreas Persson                            *
 *                                                                         *
 *   This library is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 *   This library is distributed in the hope that it will be useful,       *
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of        *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *
 *   GNU General Public License for more details.                          *
 *                                                                         *
 *   You should have received a copy of the GNU General Public License     *
 *   along with this library; if not, write to the Free Software           *
 *   Foundation, Inc., 59 Temple Place, Suite 330, Boston,                 *
 *   MA  02111-1307  USA                                                   *
 ***************************************************************************/

#include "RIFF.h"

#include "SF.h"

#include "helper.h"
#include <math.h>

#define _1200TH_ROOT_OF_2 1.000577789506555
#define _200TH_ROOT_OF_10 1.011579454259899

namespace sf2 {
    double ToSeconds(int Timecents) {
        if (Timecents == NONE) return NONE;
        if (Timecents == 0) return 1.0;
        if (Timecents == -32768) return 0.0;
        return pow(_1200TH_ROOT_OF_2, Timecents);
    }

    double ToRatio(int Centibels) {
        if (Centibels == NONE) return NONE;
        if (Centibels == 0) return 1.0;
        return pow(_200TH_ROOT_OF_10, Centibels);
    }

    double ToHz(int cents) {
        if (cents == NONE) return NONE;
        if (cents == 0) return 8.176;
        return pow(_1200TH_ROOT_OF_2, cents) * 8.176;
    }

    RIFF::Chunk* GetMandatoryChunk(RIFF::List* list, uint32_t chunkId) {
        RIFF::Chunk* ck = list->GetSubChunk(chunkId);
        if(ck == NULL) throw Exception("Mandatory chunk in RIFF list chunk not found: " + ToString(chunkId));
        return ck;
    }

    void LoadString(RIFF::Chunk* ck, std::string& s, int strLength) {
        if(ck == NULL) return;
        char* buf = new char[strLength];
        int len = 0;
        for(int i = 0; i < strLength; i++) {
            buf[i] = ck->ReadInt8();
            if(buf[i] == 0 && !len) len = i;
        }
        if(!len) len = strLength;
        s.assign(buf, len);
        delete [] buf;
    }

    /**
     * Throws an error if the chunk is NULL or
     * the chunk data size is less than size (in bytes).
     */
    void VerifySize(RIFF::Chunk* ck, int size) {
        if (ck == NULL) throw Exception("NULL chunk");
        if (ck->GetSize() < size) {
            throw Exception("Invalid chunk size. Chunk ID: " + ToString(ck->GetChunkID()));
        }
    }

    Modulator::Modulator(SFModulator mod) {
        Type = mod >> 10; // The last 6 bits
        Polarity = mod & (1 << 9);
        Direction = mod & (1 << 8);
        MidiPalete = mod & (1 << 7); // two paletes - general or MIDI
        Index = mod & 0x7f; /* index field */;

    }

    ModulatorItem::ModulatorItem(ModList& mod) :
        ModSrcOper(Modulator(mod.ModSrcOper)),
        ModAmtSrcOper(Modulator(mod.ModAmtSrcOper))
    {

    }

    Version::Version(RIFF::Chunk* ck) {
        if(ck != NULL) VerifySize(ck, 4);
        Major = ck ? ck->ReadUint16() : 0;
        Minor = ck ? ck->ReadUint16() : 0;
    }

    // *************** Info  ***************
    // *

    /** @brief Constructor.
     *
     * Initializes the info strings with values provided by an INFO list chunk.
     *
     * @param list - pointer to a list chunk which contains an INFO list chunk
     */
    Info::Info(RIFF::List* list) {
        if (list) {
            RIFF::List* lstINFO = list->GetSubList(LIST_TYPE_INFO);
            if (lstINFO) {
                pVer = new Version(GetMandatoryChunk(lstINFO, CHUNK_ID_IFIL));
                LoadString(CHUNK_ID_ISNG, lstINFO, SoundEngine);
                LoadString(CHUNK_ID_INAM, lstINFO, BankName);
                LoadString(CHUNK_ID_IROM, lstINFO, RomName);
                pRomVer = new Version(lstINFO->GetSubChunk(CHUNK_ID_IVER));
                LoadString(CHUNK_ID_ICRD, lstINFO, CreationDate);
                LoadString(CHUNK_ID_IENG, lstINFO, Engineers);
                LoadString(CHUNK_ID_IPRD, lstINFO, Product);
                LoadString(CHUNK_ID_ICOP, lstINFO, Copyright);
                LoadString(CHUNK_ID_ICMT, lstINFO, Comments);
                LoadString(CHUNK_ID_ISFT, lstINFO, Software);

            }
        }
    }

    Info::~Info() {
        delete pVer;
        delete pRomVer;
    }

    /** @brief Load given INFO field.
     *
     * Load INFO field from INFO chunk with chunk ID \a ChunkID from INFO
     * list chunk \a lstINFO and save value to \a s.
     */
    void Info::LoadString(uint32_t ChunkID, RIFF::List* lstINFO, String& s) {
        RIFF::Chunk* ck = lstINFO->GetSubChunk(ChunkID);
        ::LoadString(ck, s); // function from helper.h
    }

    Sample::Sample(RIFF::Chunk* ck, RIFF::Chunk* pCkSmpl, RIFF::Chunk* pCkSm24) {
        this->pCkSmpl = pCkSmpl;
        this->pCkSm24 = pCkSm24;

        LoadString(ck, Name, 20);
        Start = ck->ReadInt32();
        End = ck->ReadInt32();
        StartLoop = ck->ReadInt32();
        EndLoop = ck->ReadInt32();
        SampleRate = ck->ReadInt32();
        OriginalPitch = ck->ReadInt8();
        PitchCorrection = ck->ReadInt8();
        SampleLink = ck->ReadInt16();
        SampleType = ck->ReadInt16();

        if (Start > End || !pCkSmpl || pCkSmpl->GetSize() <= End) {
            throw Exception("Broken SF2 file (invalid sample info)");
        }

        ChannelCount = 1;
        switch(SampleType) {
            case 0                              : // terminal sample
            case sf2::Sample::MONO_SAMPLE       :
            case sf2::Sample::ROM_MONO_SAMPLE   : break;
            case sf2::Sample::RIGHT_SAMPLE      :
            case sf2::Sample::LEFT_SAMPLE       :
            case sf2::Sample::ROM_RIGHT_SAMPLE  :
            case sf2::Sample::ROM_LEFT_SAMPLE   : ChannelCount = 2; break;
            case sf2::Sample::LINKED_SAMPLE     :
            case sf2::Sample::ROM_LINKED_SAMPLE : std::cerr << "Linked samples not implemented yet"; break;
            default: throw Exception("Broken SF2 file (invalid sample type)");
        }

        RAMCache.Size              = 0;
        RAMCache.pStart            = NULL;
        RAMCache.NullExtensionSize = 0;
    }

    int Sample::GetChannelCount() {
        return ChannelCount;
    }

    long Sample::GetTotalFrameCount() {
        return (End - Start);
    }

    /**
     * @returns The frame size in bytes
     */
    int Sample::GetFrameSize() {
        return ChannelCount * ((pCkSm24 != NULL) ? 3 : 2);
    }

    bool Sample::HasLoops() {
        return StartLoop != 0 && EndLoop != 0;
    }

    /**
     * Reads \a SampleCount number of sample points from the position stored
     * in \a pPlaybackState into the buffer pointed by \a pBuffer and moves
     * the position within the sample respectively, this method honors the
     * looping informations of the sample (if any). Use this
     * method if you don't want to load the sample into RAM, thus for disk
     * streaming. All this methods needs to know to proceed with streaming
     * for the next time you call this method is stored in \a pPlaybackState.
     * You have to allocate and initialize the playback_state_t structure by
     * yourself before you use it to stream a sample:
     * @code
     * PlaybackState playbackstate;
     * playbackstate.position         = 0;
     * playbackstate.reverse          = false;
     * playbackstate.loop_cycles_left = pSample->LoopPlayCount;
     * @endcode
     * You don't have to take care of things like if there is actually a loop
     * defined or if the current read position is located within a loop area.
     * The method already handles such cases by itself.
     *
     * @param pBuffer          destination buffer
     * @param FrameCount       number of sample points to read
     * @param pPlaybackState   will be used to store and reload the playback
     *                         state for the next ReadAndLoop() call
     * @returns                number of successfully read sample points
     */
    unsigned long Sample::ReadAndLoop (
        void*           pBuffer,
        unsigned long   FrameCount,
        PlaybackState*  pPlaybackState,
        Region*         pRegion
    ) {
        // TODO: startAddrsCoarseOffset, endAddrsCoarseOffset
        unsigned long samplestoread = FrameCount, totalreadsamples = 0, readsamples, samplestoloopend;
        uint8_t* pDst = (uint8_t*) pBuffer;
        SetPos(pPlaybackState->position);
        if (pRegion->HasLoop) {
            do {
                samplestoloopend  = pRegion->LoopEnd - GetPos();
                readsamples       = Read(&pDst[totalreadsamples * GetFrameSize()], Min(samplestoread, samplestoloopend));
                samplestoread    -= readsamples;
                totalreadsamples += readsamples;
                if (readsamples == samplestoloopend) {
                    SetPos(pRegion->LoopStart);
                }
            } while (samplestoread && readsamples);
        } else {
            totalreadsamples = Read(pBuffer, FrameCount);
        }

        pPlaybackState->position = GetPos();

        return totalreadsamples;
    }

    Region::Region() {
        pSample = NULL;
        pInstrument = NULL;
        pParentInstrument = NULL;
        loKey = hiKey = NONE;
        minVel = maxVel = NONE;
        startAddrsOffset = startAddrsCoarseOffset = endAddrsOffset = endAddrsCoarseOffset = 0;
        startloopAddrsOffset = startloopAddrsCoarseOffset = endloopAddrsOffset = endloopAddrsCoarseOffset = 0;
        pan = fineTune = coarseTune = 0;
        overridingRootKey = -1; // -1 means not used

        HasLoop = false;
        LoopStart = LoopEnd = 0;

        EG1PreAttackDelay = EG1Attack = EG1Hold = EG1Decay = EG1Release = -12000;
        EG1Sustain = 0;
        EG2PreAttackDelay = EG2Attack = EG2Hold = EG2Decay = EG2Release = -12000;
        EG2Sustain = 0;

        modEnvToPitch = modLfoToPitch = modEnvToFilterFc = modLfoToFilterFc = modLfoToVolume = 0;
        freqModLfo = 0;
        delayModLfo = -12000;
        vibLfoToPitch = 0;
        freqVibLfo = 0;
        delayVibLfo = -12000;

        exclusiveClass = 0;
        
        initialFilterFc = 13500;
        initialFilterQ = 0;
    }

    int Region::GetUnityNote() {
        return overridingRootKey != -1 ? overridingRootKey : pSample->OriginalPitch;
    }
    
    int CheckRange(std::string genName, int min, int max, int& gen) {
        if (gen == NONE) return gen;

        if (gen < min) {
            std::cerr << "sf2: " << genName;
            std::cerr << " is below the minimum allowed value (min=" << min << "): " << gen << std::endl;
            gen = min;
        }
        if (gen > max) {
            std::cerr << "sf2: " << genName;
            std::cerr << " is above the maximum allowed value (max=" << max << "): " << gen << std::endl;
            gen = max;
        }
        
        return gen;
    }

    void Region::SetGenerator(sf2::File* pFile, GenList& Gen) {
        switch(Gen.GenOper) {
            case START_ADDRS_OFFSET:
                startAddrsOffset = Gen.GenAmount.wAmount;
                break;
            case END_ADDRS_OFFSET:
                if (Gen.GenAmount.shAmount <= 0) {
                    endAddrsOffset = Gen.GenAmount.shAmount;
                } else {
                    std::cerr << "Ignoring invalid endAddrsOffset" << std::endl;
                }
                break;
            case STARTLOOP_ADDRS_OFFSET:
                startloopAddrsOffset = Gen.GenAmount.shAmount;
                LoopStart += startloopAddrsOffset;
                break;
            case ENDLOOP_ADDRS_OFFSET:
                endloopAddrsOffset = Gen.GenAmount.shAmount;
                LoopEnd += endloopAddrsOffset;
                break;
            case START_ADDRS_COARSE_OFFSET:
                startAddrsCoarseOffset = Gen.GenAmount.wAmount;
                break;
            case MOD_LFO_TO_PITCH:
                modLfoToPitch = Gen.GenAmount.shAmount;
                CheckRange("modLfoToPitch", -12000, 12000, modLfoToPitch);
                break;
            case VIB_LFO_TO_PITCH:
                vibLfoToPitch = Gen.GenAmount.shAmount;
                CheckRange("vibLfoToPitch", -12000, 12000, vibLfoToPitch);
                break;
            case MOD_ENV_TO_PITCH:
                modEnvToPitch = Gen.GenAmount.shAmount;
                CheckRange("modEnvToPitch", -12000, 12000, modEnvToPitch);
                break;
            case INITIAL_FILTER_FC:
                initialFilterFc = Gen.GenAmount.wAmount;
                CheckRange("initialFilterFc", 1500, 13500, initialFilterFc);
                break;
            case INITIAL_FILTER_Q:
                initialFilterQ = Gen.GenAmount.wAmount;
                CheckRange("initialFilterQ", 0, 960, initialFilterQ);
                break;
            case MOD_LFO_TO_FILTER_FC:
                modLfoToFilterFc = Gen.GenAmount.shAmount;
                CheckRange("modLfoToFilterFc", -12000, 12000, modLfoToFilterFc);
                break;
            case MOD_ENV_TO_FILTER_FC:
                modEnvToFilterFc = Gen.GenAmount.shAmount;
                CheckRange("modEnvToFilterFc", -12000, 12000, modEnvToFilterFc);
                break;
            case END_ADDRS_COARSE_OFFSET:
                endAddrsCoarseOffset = Gen.GenAmount.wAmount;
                break;
            case MOD_LFO_TO_VOLUME:
                modLfoToVolume = Gen.GenAmount.shAmount;
                CheckRange("modLfoToVolume", -960, 960, modLfoToVolume);
                break;
            case CHORUS_EFFECTS_SEND:
                break;
            case REVERB_EFFECTS_SEND:
                break;
            case PAN:
                pan = Gen.GenAmount.shAmount;
                CheckRange("pan", -500, 500, pan);
                pan * 64; pan /= 500;
                if (pan < -64) pan = -64;
                if (pan >  63) pan =  63;
                break;
            case DELAY_MOD_LFO:
                delayModLfo = Gen.GenAmount.shAmount;
                CheckRange("delayModLfo", -12000, 5000, delayModLfo);
                break;
            case FREQ_MOD_LFO:
                freqModLfo = Gen.GenAmount.shAmount;
                CheckRange("freqModLfo", -16000, 4500, freqModLfo);
                break;
            case DELAY_VIB_LFO:
                delayVibLfo = Gen.GenAmount.shAmount;
                CheckRange("delayVibLfo", -12000, 5000, delayVibLfo);
                break;
            case FREQ_VIB_LFO:
                freqVibLfo = Gen.GenAmount.shAmount;
                CheckRange("freqModLfo", -16000, 4500, freqModLfo);
                break;
            case DELAY_MOD_ENV:
                EG2PreAttackDelay = Gen.GenAmount.shAmount;
                CheckRange("delayModEnv", -12000, 5000, EG2PreAttackDelay);
                break;
            case ATTACK_MOD_ENV:
                EG2Attack = Gen.GenAmount.shAmount;
                CheckRange("attackModEnv", -12000, 8000, EG2Attack);
                break;
            case HOLD_MOD_ENV:
                EG2Hold = Gen.GenAmount.shAmount;
                CheckRange("holdModEnv", -12000, 5000, EG2Hold);
                break;
            case DECAY_MOD_ENV:
                EG2Decay = Gen.GenAmount.shAmount;
                CheckRange("decayModEnv", -12000, 8000, EG2Decay);
                break;
            case SUSTAIN_MOD_ENV:
                EG2Sustain = Gen.GenAmount.shAmount;
                CheckRange("sustainModEnv", 0, 1000, EG2Sustain);
                break;
            case RELEASE_MOD_ENV:
                EG2Release = Gen.GenAmount.shAmount;
                CheckRange("releaseModEnv", -12000, 8000, EG2Release);
                break;
            case KEYNUM_TO_MOD_ENV_HOLD:
                break;
            case KEYNUM_TO_MOD_ENV_DECAY:
                break;
            case DELAY_VOL_ENV:
                EG1PreAttackDelay = Gen.GenAmount.shAmount;
                CheckRange("delayVolEnv", -12000, 5000, EG1PreAttackDelay);
                break;
            case ATTACK_VOL_ENV:
                EG1Attack = Gen.GenAmount.shAmount;
                CheckRange("attackVolEnv", -12000, 8000, EG1Attack);
                break;
            case HOLD_VOL_ENV:
                EG1Hold = Gen.GenAmount.shAmount;
                CheckRange("holdVolEnv", -12000, 5000, EG1Hold);
                break;
            case DECAY_VOL_ENV:
                EG1Decay = Gen.GenAmount.shAmount;
                CheckRange("decayVolEnv", -12000, 8000, EG1Decay);
                break;
            case SUSTAIN_VOL_ENV:
                EG1Sustain = Gen.GenAmount.shAmount;
                CheckRange("sustainVolEnv", 0, 1440, EG1Sustain);
                break;
            case RELEASE_VOL_ENV:
                EG1Release = Gen.GenAmount.shAmount;
                CheckRange("releaseVolEnv", -12000, 8000, EG1Release);
                break;
            case KEYNUM_TO_VOL_ENV_HOLD:
                break;
            case KEYNUM_TO_VOL_ENV_DECAY:
                break;
            case INSTRUMENT: {
                uint16_t id = Gen.GenAmount.wAmount;
                if (id >= pFile->Instruments.size()) {
                    throw Exception("Broken SF2 file (missing instruments)");
                }
                pInstrument = pFile->Instruments[id];
                break;
            }
            case KEY_RANGE:
                loKey = Gen.GenAmount.ranges.byLo;
                CheckRange("loKey", 0, 127, loKey);
                hiKey = Gen.GenAmount.ranges.byHi;
                CheckRange("hiKey", 0, 127, hiKey);
                break;
            case VEL_RANGE:
                minVel = Gen.GenAmount.ranges.byLo;
                CheckRange("minVel", 0, 127, minVel);
                maxVel = Gen.GenAmount.ranges.byHi;
                CheckRange("maxVel", 0, 127, maxVel);
                break;
            case STARTLOOP_ADDRS_COARSE_OFFSET:
                startloopAddrsCoarseOffset = Gen.GenAmount.wAmount;
                LoopStart += startloopAddrsCoarseOffset * 32768;
                break;
            case KEYNUM:
                break;
            case VELOCITY:
                break;
            case INITIAL_ATTENUATION:
                break;
            case ENDLOOP_ADDRS_COARSE_OFFSET:
                endloopAddrsCoarseOffset = Gen.GenAmount.wAmount;
                LoopEnd += endloopAddrsCoarseOffset * 32768;
                break;
            case COARSE_TUNE:
                coarseTune = Gen.GenAmount.shAmount;
                CheckRange("coarseTune", -120, 120, coarseTune);
                break;
            case FINE_TUNE:
                fineTune = Gen.GenAmount.shAmount;
                CheckRange("fineTune", -99, 99, fineTune);
                break;
            case SAMPLE_ID: {
                uint16_t sid = Gen.GenAmount.wAmount;
                if (sid >= pFile->Samples.size()) {
                    throw Exception("Broken SF2 file (missing samples)");
                }
                pSample = pFile->Samples[sid];

                if (HasLoop) {
                    LoopStart += pSample->StartLoop;
                    LoopEnd   += pSample->EndLoop;
                    if ( LoopStart < pSample->Start || LoopStart > pSample->End ||
                         LoopStart > LoopEnd        || LoopEnd   > pSample->End    ) {
                        throw Exception("Broken SF2 file (invalid loops)");
                    }
                    LoopStart -= pSample->Start; // Relative to the sample start
                    LoopEnd   -= pSample->Start; // Relative to the sample start
                }
                break;
            }
            case SAMPLE_MODES:
                HasLoop = Gen.GenAmount.wAmount & 1;
                // TODO: 3 indicates a sound which loops for the duration of key depression
                //       then proceeds to play the remainder of the sample.
                break;
            case SCALE_TUNING:
                break;
            case EXCLUSIVE_CLASS:
                exclusiveClass = Gen.GenAmount.wAmount;
                break;
            case OVERRIDING_ROOT_KEY:
                overridingRootKey = Gen.GenAmount.shAmount;
                CheckRange("overridingRootKey", -1, 127, overridingRootKey);
                break;
        }
    }

    void Region::SetModulator(sf2::File* pFile, ModList& Mod) {
        modulators.push_back(ModulatorItem(Mod));
        /*switch(srcType) {
            case NO_CONTROLLER:
                break;
            case NOTE_ON_VELOCITY:
                break;
            case NOTE_ON_KEY_NUMBER:
                break;
            case POLY_PRESSURE:
                break;
            case CHANNEL_PRESSURE:
                break;
            case PITCH_WHEEL:
                break;
            case PITCH_WHEEL_SENSITIVITY:
                break;
            case LINK:
                break;
            default: std::cout << "Unknown controller source: " << srcType << std::endl;
        }*/
    }

    int Region::GetPan(Region* pPresetRegion) {
        if (pPresetRegion == NULL) return pan;
        int p = pPresetRegion->pan + pan;
        if (p < -64) p = -64;
        if (p >  63) p =  63;
        return p;
    }

    int Region::GetFineTune(Region* pPresetRegion) {
        if (pPresetRegion == NULL) return fineTune;
        int t = pPresetRegion->fineTune + fineTune;
        if (t < -99) t = -99;
        if (t >  99) t =  99;
        return t;
    }

    int Region::GetCoarseTune(Region* pPresetRegion) {
         if (pPresetRegion == NULL) return coarseTune;
        int t = pPresetRegion->coarseTune + coarseTune;
        if (t < -120) t = -120;
        if (t >  120) t =  120;
        return t;
    }

    double Region::GetEG1PreAttackDelay(Region* pPresetRegion) {
        int val = (pPresetRegion == NULL || pPresetRegion->EG1PreAttackDelay == NONE) ?
                  EG1PreAttackDelay : pPresetRegion->EG1PreAttackDelay + EG1PreAttackDelay;
        return ToSeconds(CheckRange("GetEG1PreAttackDelay()", -12000, 5000, val));
    }

    double Region::GetEG1Attack(Region* pPresetRegion) {
        int val = (pPresetRegion == NULL || pPresetRegion->EG1Attack == NONE) ?
                  EG1Attack : pPresetRegion->EG1Attack + EG1Attack;
        return ToSeconds(CheckRange("GetEG1Attack()", -12000, 8000, val));
    }

    double Region::GetEG1Hold(Region* pPresetRegion) {
        int val = (pPresetRegion == NULL || pPresetRegion->EG1Hold == NONE) ?
                  EG1Hold : pPresetRegion->EG1Hold + EG1Hold;
        return ToSeconds(CheckRange("GetEG1Hold()", -12000, 5000, val));
    }

    double Region::GetEG1Decay(Region* pPresetRegion) {
        int val = (pPresetRegion == NULL || pPresetRegion->EG1Decay == NONE) ?
                  EG1Decay : pPresetRegion->EG1Decay + EG1Decay;
        return ToSeconds(CheckRange("GetEG1Decay()", -12000, 8000, val));
    }

    int Region::GetEG1Sustain(Region* pPresetRegion) {
        int val = (pPresetRegion == NULL || pPresetRegion->EG1Sustain == NONE) ?
                  EG1Sustain : pPresetRegion->EG1Sustain + EG1Sustain;
        return CheckRange("GetEG1Sustain()", 0, 1440, val);
    }

    double Region::GetEG1Release(Region* pPresetRegion) {
        int val = (pPresetRegion == NULL || pPresetRegion->EG1Release == NONE) ?
                  EG1Release : pPresetRegion->EG1Release + EG1Release;
        return ToSeconds(CheckRange("GetEG1Release()", -12000, 8000, val));
    }

    double Region::GetEG2PreAttackDelay(Region* pPresetRegion) {
        int val = (pPresetRegion == NULL || pPresetRegion->EG2PreAttackDelay == NONE) ?
                  EG2PreAttackDelay : pPresetRegion->EG2PreAttackDelay + EG2PreAttackDelay;
        return ToSeconds(CheckRange("GetEG2PreAttackDelay()", -12000, 5000, val));
    }

    double Region::GetEG2Attack(Region* pPresetRegion) {
        int val = (pPresetRegion == NULL || pPresetRegion->EG2Attack == NONE) ?
                  EG2Attack : pPresetRegion->EG2Attack + EG2Attack;
        return ToSeconds(CheckRange("GetEG2Attack()", -12000, 8000, val));
    }

    double Region::GetEG2Hold(Region* pPresetRegion) {
        int val = (pPresetRegion == NULL || pPresetRegion->EG2Hold == NONE) ?
                  EG2Hold : pPresetRegion->EG2Hold + EG2Hold;
        return ToSeconds(CheckRange("GetEG2Hold()", -12000, 5000, val));
    }

    double Region::GetEG2Decay(Region* pPresetRegion) {
        int val = (pPresetRegion == NULL || pPresetRegion->EG2Decay == NONE) ?
                  EG2Decay : pPresetRegion->EG2Decay + EG2Decay;
        return ToSeconds(CheckRange("GetEG2Decay()", -12000, 8000, val));
    }

    int Region::GetEG2Sustain(Region* pPresetRegion) {
        int val = (pPresetRegion == NULL || pPresetRegion->EG2Sustain == NONE) ?
                  EG2Sustain : pPresetRegion->EG2Sustain + EG2Sustain;
        return CheckRange("GetEG2Sustain()", 0, 1000, val);
    }

    double Region::GetEG2Release(Region* pPresetRegion) {
        int val = (pPresetRegion == NULL || pPresetRegion->EG2Release == NONE) ?
                  EG2Release : pPresetRegion->EG2Release + EG2Release;
        return ToSeconds(CheckRange("GetEG2Release()", -12000, 8000, val));
    }

    int Region::GetModEnvToPitch(Region* pPresetRegion) {
        int val = (pPresetRegion == NULL || pPresetRegion->modEnvToPitch == NONE) ?
                   modEnvToPitch : pPresetRegion->modEnvToPitch + modEnvToPitch;
        return CheckRange("GetModEnvToPitch()", -12000, 12000, val);
    }

    int Region::GetModLfoToPitch(Region* pPresetRegion) {
        int val = (pPresetRegion == NULL || pPresetRegion->modLfoToPitch == NONE) ?
                   modLfoToPitch : pPresetRegion->modLfoToPitch + modLfoToPitch;
        return CheckRange("GetModLfoToPitch()", -12000, 12000, val);
    }

    int Region::GetModEnvToFilterFc(Region* pPresetRegion) {
        int val = (pPresetRegion == NULL || pPresetRegion->modEnvToFilterFc == NONE) ?
                   modEnvToFilterFc : pPresetRegion->modEnvToFilterFc + modEnvToFilterFc;
        return CheckRange("GetModEnvToFilterFc()", -12000, +12000, val);
    }

    int Region::GetModLfoToFilterFc(Region* pPresetRegion) {
        int val = (pPresetRegion == NULL || pPresetRegion->modLfoToFilterFc == NONE) ?
                   modLfoToFilterFc : pPresetRegion->modLfoToFilterFc + modLfoToFilterFc;
        return CheckRange("GetModLfoToFilterFc()", -12000, +12000, val);
    }

    double Region::GetModLfoToVolume(Region* pPresetRegion) {
        int val = (pPresetRegion == NULL || pPresetRegion->modLfoToVolume == NONE) ?
                   modLfoToVolume : pPresetRegion->modLfoToVolume + modLfoToVolume;
        return CheckRange("GetModLfoToVolume()", -960, 960, val);
    }

    double Region::GetFreqModLfo(Region* pPresetRegion) {
        int val = (pPresetRegion == NULL || pPresetRegion->freqModLfo == NONE) ?
                  freqModLfo : pPresetRegion->freqModLfo + freqModLfo;
        return ToHz(CheckRange("GetFreqModLfo()", -16000, 4500, val));
    }

    double Region::GetDelayModLfo(Region* pPresetRegion) {
        int val = (pPresetRegion == NULL || pPresetRegion->delayModLfo == NONE) ?
                  delayModLfo : pPresetRegion->delayModLfo + delayModLfo;
        return ToSeconds(CheckRange("GetDelayModLfo()", -12000, 5000, val));
    }

    int Region::GetVibLfoToPitch(Region* pPresetRegion) {
        int val = (pPresetRegion == NULL || pPresetRegion->vibLfoToPitch == NONE) ?
                   vibLfoToPitch : pPresetRegion->vibLfoToPitch + vibLfoToPitch;
        return CheckRange("GetVibLfoToPitch()", -12000, 12000, val);
    }

    double Region::GetFreqVibLfo(Region* pPresetRegion) {
        int val = (pPresetRegion == NULL || pPresetRegion->freqVibLfo == NONE) ?
                  freqVibLfo : pPresetRegion->freqVibLfo + freqVibLfo;
        return ToHz(CheckRange("GetFreqVibLfo()", -16000, 4500, val));
    }

    double Region::GetDelayVibLfo(Region* pPresetRegion) {
        int val = (pPresetRegion == NULL || pPresetRegion->delayVibLfo == NONE) ?
                  delayVibLfo : pPresetRegion->delayVibLfo + delayVibLfo;
        return ToSeconds(CheckRange("GetDelayVibLfo()", -12000, 5000, val));
    }

    int Region::GetInitialFilterFc(Region* pPresetRegion) {
        if (pPresetRegion == NULL || pPresetRegion->initialFilterFc == NONE) return initialFilterFc;
        int val = pPresetRegion->initialFilterFc + initialFilterFc;
        return CheckRange("GetInitialFilterFc()", 1500, 13500, val);
    }

    int Region::GetInitialFilterQ(Region* pPresetRegion) {
        int val = (pPresetRegion == NULL || pPresetRegion->initialFilterQ == NONE) ?
                   initialFilterQ : pPresetRegion->initialFilterQ + initialFilterQ;
        return CheckRange("GetInitialFilterQ()", 0, 960, val);
    }

    InstrumentBase::InstrumentBase(sf2::File* pFile) {
        this->pFile = pFile;
        pGlobalRegion = NULL;
    }

    InstrumentBase::~InstrumentBase() {
        if (pGlobalRegion) delete pGlobalRegion;
        for (int i = regions.size() - 1; i >= 0; i--) {
            if (regions[i]) delete (regions[i]);
        }
    }

    int InstrumentBase::GetRegionCount() {
        return regions.size();
    }

    Region* InstrumentBase::GetRegion(int idx) {
         if (idx < 0 || idx >= GetRegionCount()) {
            throw Exception("Region index out of bounds");
        }

        return regions[idx];
    }

    Query::Query(InstrumentBase& instrument) : instrument(instrument) {
        i = 0;
    }

    Region* Query::next() {
        while (i < instrument.GetRegionCount()) {
            Region* r = instrument.GetRegion(i++);
            if (((r->loKey  == NONE && r->hiKey  == NONE) || (key >= r->loKey && key <= r->hiKey)) &&
                ((r->minVel == NONE && r->maxVel == NONE) || (vel >= r->minVel && vel <= r->maxVel))) {
                return r;
            }
        }
        return 0;
    }

    Instrument::Instrument(sf2::File* pFile, RIFF::Chunk* ck) : InstrumentBase(pFile) {
        this->pFile = pFile;
        LoadString(ck, Name, 20);
        InstBagNdx = ck->ReadInt16();
    }

    Instrument::~Instrument() {
    }

    Region* Instrument::CreateRegion() {
        Region* r = new Region;
        r->pParentInstrument = this;

        if (pGlobalRegion != NULL) {
            r->loKey       = pGlobalRegion->loKey;
            r->hiKey       = pGlobalRegion->hiKey;
            r->minVel      = pGlobalRegion->minVel;
            r->maxVel      = pGlobalRegion->maxVel;
            r->pan         = pGlobalRegion->pan;
            r->fineTune    = pGlobalRegion->fineTune;
            r->coarseTune  = pGlobalRegion->coarseTune;
            r->overridingRootKey = pGlobalRegion->overridingRootKey;
            r->startAddrsOffset            = pGlobalRegion->startAddrsOffset;
            r->startAddrsCoarseOffset      = pGlobalRegion->startAddrsCoarseOffset;
            r->endAddrsOffset              = pGlobalRegion->endAddrsOffset;
            r->endAddrsCoarseOffset        = pGlobalRegion->endAddrsCoarseOffset;
            r->startloopAddrsOffset        = pGlobalRegion->startloopAddrsOffset;
            r->startloopAddrsCoarseOffset  = pGlobalRegion->startloopAddrsCoarseOffset;
            r->endloopAddrsOffset          = pGlobalRegion->endloopAddrsOffset;
            r->endloopAddrsCoarseOffset    = pGlobalRegion->endloopAddrsCoarseOffset;

            r->EG1PreAttackDelay  = pGlobalRegion->EG1PreAttackDelay;
            r->EG1Attack          = pGlobalRegion->EG1Attack;
            r->EG1Hold            = pGlobalRegion->EG1Hold;
            r->EG1Decay           = pGlobalRegion->EG1Decay;
            r->EG1Sustain         = pGlobalRegion->EG1Sustain;
            r->EG1Release         = pGlobalRegion->EG1Release;

            r->EG2PreAttackDelay  = pGlobalRegion->EG2PreAttackDelay;
            r->EG2Attack          = pGlobalRegion->EG2Attack;
            r->EG2Hold            = pGlobalRegion->EG2Hold;
            r->EG2Decay           = pGlobalRegion->EG2Decay;
            r->EG2Sustain         = pGlobalRegion->EG2Sustain;
            r->EG2Release         = pGlobalRegion->EG2Release;

            r->modEnvToPitch     = pGlobalRegion->modEnvToPitch;
            r->modLfoToPitch     = pGlobalRegion->modLfoToPitch;
            r->modEnvToFilterFc  = pGlobalRegion->modEnvToFilterFc;
            r->modLfoToFilterFc  = pGlobalRegion->modLfoToFilterFc;
            r->modLfoToVolume    = pGlobalRegion->modLfoToVolume;
            r->freqModLfo        = pGlobalRegion->freqModLfo;
            r->delayModLfo       = pGlobalRegion->delayModLfo;
            r->vibLfoToPitch     = pGlobalRegion->vibLfoToPitch;
            r->freqVibLfo        = pGlobalRegion->freqVibLfo;
            r->delayVibLfo       = pGlobalRegion->delayVibLfo;
            r->initialFilterFc   = pGlobalRegion->initialFilterFc;
            r->initialFilterQ    = pGlobalRegion->initialFilterQ;

            r->HasLoop    = pGlobalRegion->HasLoop;
            r->LoopStart  = pGlobalRegion->LoopStart;
            r->LoopEnd    = pGlobalRegion->LoopEnd;

            r->exclusiveClass = pGlobalRegion->exclusiveClass;
        }

        return r;
    }

    void Instrument::DeleteRegion(Region* pRegion) {
        for (int i = 0; i < regions.size(); i++) {
            if (regions[i] == pRegion) {
                delete pRegion;
                regions[i] = NULL;
                return;
            }
        }

        std::cerr << "Can't remove unknown Region" << std::endl;
    }

    void Instrument::LoadRegions(int idx1, int idx2) {
        for (int i = idx1; i < idx2; i++) {
            int gIdx1 = pFile->InstBags[i].InstGenNdx;
            int gIdx2 = pFile->InstBags[i + 1].InstGenNdx;

            if (gIdx1 < 0 || gIdx2 < 0 || gIdx1 > gIdx2 || gIdx2 >= pFile->InstGenLists.size()) {
                throw Exception("Broken SF2 file (invalid InstGenNdx)");
            }

            int mIdx1 = pFile->InstBags[i].InstModNdx;
            int mIdx2 = pFile->InstBags[i + 1].InstModNdx;

            if (mIdx1 < 0 || mIdx2 < 0 || mIdx1 > mIdx2 || mIdx2 >= pFile->InstModLists.size()) {
                throw Exception("Broken SF2 file (invalid InstModNdx)");
            }

            Region* reg = CreateRegion();

            for (int j = gIdx1; j < gIdx2; j++) {
                reg->SetGenerator(pFile, pFile->InstGenLists[j]);
                // TODO: ignore generators following a sampleID generator
            }

            for (int j = mIdx1; j < mIdx2; j++) {
                reg->SetModulator(pFile, pFile->InstModLists[j]);
            }

            if (reg->pSample == NULL) {
                if (i == idx1 && idx2 - idx1 > 1) {
                    pGlobalRegion = reg;  // global zone
                } else {
                    std::cerr << "Ignoring instrument's region without sample" << std::endl;
                    delete reg;
                }
            } else {
                regions.push_back(reg);
            }
        }
    }

    Preset::Preset(sf2::File* pFile, RIFF::Chunk* ck): InstrumentBase(pFile) {
        this->pFile = pFile;
        LoadString(ck, Name, 20);
        PresetNum = ck->ReadInt16();
        Bank = ck->ReadInt16();
        PresetBagNdx = ck->ReadInt16();
        Library = ck->ReadInt32();
        Genre = ck->ReadInt32();
        Morphology = ck->ReadInt32();
    }

    Preset::~Preset() {
    }

    Region* Preset::CreateRegion() {
        Region* r = new Region;

        r->EG1PreAttackDelay = r->EG1Attack = r->EG1Hold = r->EG1Decay = r->EG1Sustain = r->EG1Release = NONE;
        r->EG2PreAttackDelay = r->EG2Attack = r->EG2Hold = r->EG2Decay = r->EG2Sustain = r->EG2Release = NONE;
        r->freqModLfo = r->delayModLfo = r->freqVibLfo = r->delayVibLfo = NONE;
        r->initialFilterFc = r->initialFilterQ = NONE;

        if (pGlobalRegion != NULL) {
            r->pan         = pGlobalRegion->pan;
            r->fineTune    = pGlobalRegion->fineTune;
            r->coarseTune  = pGlobalRegion->coarseTune;

            r->EG1PreAttackDelay  = pGlobalRegion->EG1PreAttackDelay;
            r->EG1Attack          = pGlobalRegion->EG1Attack;
            r->EG1Hold            = pGlobalRegion->EG1Hold;
            r->EG1Decay           = pGlobalRegion->EG1Decay;
            r->EG1Sustain         = pGlobalRegion->EG1Sustain;
            r->EG1Release         = pGlobalRegion->EG1Release;

            r->EG2PreAttackDelay  = pGlobalRegion->EG2PreAttackDelay;
            r->EG2Attack          = pGlobalRegion->EG2Attack;
            r->EG2Hold            = pGlobalRegion->EG2Hold;
            r->EG2Decay           = pGlobalRegion->EG2Decay;
            r->EG2Sustain         = pGlobalRegion->EG2Sustain;
            r->EG2Release         = pGlobalRegion->EG2Release;

            r->modEnvToPitch     = pGlobalRegion->modEnvToPitch;
            r->modLfoToPitch     = pGlobalRegion->modLfoToPitch;
            r->modEnvToFilterFc  = pGlobalRegion->modEnvToFilterFc;
            r->modLfoToFilterFc  = pGlobalRegion->modLfoToFilterFc;
            r->modLfoToVolume    = pGlobalRegion->modLfoToVolume;
            r->freqModLfo        = pGlobalRegion->freqModLfo;
            r->delayModLfo       = pGlobalRegion->delayModLfo;
            r->vibLfoToPitch     = pGlobalRegion->vibLfoToPitch;
            r->freqVibLfo        = pGlobalRegion->freqVibLfo;
            r->delayVibLfo       = pGlobalRegion->delayVibLfo;
            r->initialFilterFc   = pGlobalRegion->initialFilterFc;
            r->initialFilterQ    = pGlobalRegion->initialFilterQ;
        }

        return r;
    }

    void Preset::LoadRegions(int idx1, int idx2) {
        for (int i = idx1; i < idx2; i++) {
            int gIdx1 = pFile->PresetBags[i].GenNdx;
            int gIdx2 = pFile->PresetBags[i + 1].GenNdx;

            if (gIdx1 < 0 || gIdx2 < 0 || gIdx1 > gIdx2 || gIdx2 >= pFile->PresetGenLists.size()) {
                throw Exception("Broken SF2 file (invalid PresetGenNdx)");
            }

            Region* reg = CreateRegion();

            for (int j = gIdx1; j < gIdx2; j++) {
                reg->SetGenerator(pFile, pFile->PresetGenLists[j]);
            }
            if (reg->pInstrument == NULL) {
                if (i == idx1 && idx2 - idx1 > 1) {
                    pGlobalRegion = reg;  // global zone
                } else {
                    std::cerr << "Ignoring preset's region without instrument" << std::endl;
                    delete reg;
                }
            } else {
                regions.push_back(reg);
            }
        }
    }

    /** @brief Constructor.
     *
     * Load an existing SF2 file.
     *
     * @param pRIFF - pointer to a RIFF file which is actually the SF2 file
     *                to load
     * @throws Exception if given file is not a SF2 file, expected chunks
     *                   are missing
     */
    File::File(RIFF::File* pRIFF) {
        if (!pRIFF) throw Exception("NULL pointer reference to RIFF::File object.");
        this->pRIFF = pRIFF;

        if (pRIFF->GetListType() != RIFF_TYPE_SF2) {
            throw Exception("Not a SF2 file");
        }

        pInfo = new Info(pRIFF);
        if (pInfo->pVer->Major != 2) {
            throw Exception("Unsupported version: " + ToString(pInfo->pVer->Major));
        }

        RIFF::List* lstSDTA = pRIFF->GetSubList(LIST_TYPE_SDTA);
        if (lstSDTA == NULL) {
            throw Exception("Broken SF2 file (missing sdta)");
        }

        RIFF::Chunk* pCkSmpl = lstSDTA->GetSubChunk(CHUNK_ID_SMPL);
        RIFF::Chunk* pCkSm24 = lstSDTA->GetSubChunk(CHUNK_ID_SM24);
        if (pCkSmpl != NULL && pCkSm24 != NULL) {
            long l = pCkSmpl->GetSize() / 2;
            if (l%2) l++;
            if (pCkSm24->GetSize() != l) {
                pCkSm24 = NULL; // ignoring sm24 due to invalid size
            }
        }

        RIFF::List* lstPDTA = pRIFF->GetSubList(LIST_TYPE_PDTA);
        if (lstPDTA == NULL) {
            throw Exception("Broken SF2 file (missing pdta)");
        }

        RIFF::Chunk* ck = lstPDTA->GetSubChunk(CHUNK_ID_PHDR);
        if (ck->GetSize() < 38) {
            throw Exception("Broken SF2 file (broken phdr)");
        }

        int count = ck->GetSize() / 38;
        for (int i = 0; i < count; i++) {
            Presets.push_back(new Preset(this, ck));
        }

        ck = GetMandatoryChunk(lstPDTA, CHUNK_ID_PBAG);
        if (ck->GetSize() < 4 || (ck->GetSize() % 4)) {
            throw Exception("Broken SF2 file (broken pbag)");
        }

        count = ck->GetSize() / 4;
        for (int i = 0; i < count; i++) {
            PresetBag pb;
            pb.GenNdx = ck->ReadInt16();
            pb.ModNdx = ck->ReadInt16();
            PresetBags.push_back(pb);
        }
        //std::cout << "Preset bags: " << PresetBags.size() << std::endl;

        ck = GetMandatoryChunk(lstPDTA, CHUNK_ID_PMOD);
        if (ck->GetSize() % 10) {
            throw Exception("Broken SF2 file (broken pmod)");
        }

        count = ck->GetSize() / 10;
        for (int i = 0; i < count; i++) {
            ModList ml;
            ml.ModSrcOper = ck->ReadInt16();
            ml.ModDestOper = ck->ReadInt16();
            ml.ModAmount = ck->ReadInt16();
            ml.ModAmtSrcOper = ck->ReadInt16();
            ml.ModTransOper = ck->ReadInt16();
            PresetModLists.push_back(ml);
        }
        //std::cout << "Preset mod lists: " << PresetModLists.size() << std::endl;

        ck = GetMandatoryChunk(lstPDTA, CHUNK_ID_PGEN);
        if (ck->GetSize() < 4 || (ck->GetSize() % 4)) {
            throw Exception("Broken SF2 file (broken pgen)");
        }

        count = ck->GetSize() / 4;
        for (int i = 0; i < count; i++) {
            GenList gl;
            gl.GenOper = ck->ReadInt16();
            gl.GenAmount.wAmount = ck->ReadInt16();
            PresetGenLists.push_back(gl);
        }
        //std::cout << "Preset gen lists: " << PresetGenLists.size() << std::endl;

        ck = GetMandatoryChunk(lstPDTA, CHUNK_ID_INST);
        if (ck->GetSize() < (22 * 2) || (ck->GetSize() % 22)) {
            throw Exception("Broken SF2 file (broken inst)");
        }
        count = ck->GetSize() / 22;
        for (int i = 0; i < count; i++) {
            Instruments.push_back(new Instrument(this, ck));
        }

        ck = GetMandatoryChunk(lstPDTA, CHUNK_ID_IBAG);
        if (ck->GetSize() < 4 || (ck->GetSize() % 4)) {
            throw Exception("Broken SF2 file (broken ibag)");
        }

        count = ck->GetSize() / 4;
        for (int i = 0; i < count; i++) {
            InstBag ib;
            ib.InstGenNdx = ck->ReadInt16();
            ib.InstModNdx = ck->ReadInt16();
            InstBags.push_back(ib);
        }
        //std::cout << "Instrument bags: " << InstBags.size() << std::endl;

        ck = GetMandatoryChunk(lstPDTA, CHUNK_ID_IMOD);
        if (ck->GetSize() % 10) {
            throw Exception("Broken SF2 file (broken imod)");
        }

        count = ck->GetSize() / 10;
        for (int i = 0; i < count; i++) {
            ModList ml;
            ml.ModSrcOper = ck->ReadInt16();
            ml.ModDestOper = ck->ReadInt16();
            ml.ModAmount = ck->ReadInt16();
            ml.ModAmtSrcOper = ck->ReadInt16();
            ml.ModTransOper = ck->ReadInt16();
            InstModLists.push_back(ml);
        }
        //std::cout << "Instrument mod lists: " << InstModLists.size() << std::endl;

        ck = GetMandatoryChunk(lstPDTA, CHUNK_ID_IGEN);
        if (ck->GetSize() < 4 || (ck->GetSize() % 4)) {
            throw Exception("Broken SF2 file (broken igen)");
        }

        count = ck->GetSize() / 4;
        for (int i = 0; i < count; i++) {
            GenList gl;
            gl.GenOper = ck->ReadInt16();
            gl.GenAmount.wAmount = ck->ReadInt16();
            InstGenLists.push_back(gl);
        }
        //std::cout << "Instrument gen lists: " << InstGenLists.size() << std::endl;

        ck = GetMandatoryChunk(lstPDTA, CHUNK_ID_SHDR);
        if ((ck->GetSize() % 46)) {
            throw Exception("Broken SF2 file (broken shdr)");
        }
        count = ck->GetSize() / 46;
        for (int i = 0; i < count; i++) {
            Samples.push_back(new Sample(ck, pCkSmpl, pCkSm24));
        }

        // Loading instrument regions
        for (int i = 0; i < Instruments.size() - 1; i++) {
            Instrument* instr = Instruments[i];
            int x1 = instr->InstBagNdx;
            int x2 = Instruments[i + 1]->InstBagNdx;
            if (x1 < 0 || x2 < 0 || x1 > x2 || x2 >= InstBags.size()) {
                throw Exception("Broken SF2 file (invalid InstBagNdx)");
            }

            instr->LoadRegions(x1, x2);
        }

        // Loading preset regions
        for (int i = 0; i < Presets.size() - 1; i++) {
            Preset* preset = Presets[i];
            int x1 = preset->PresetBagNdx;
            int x2 = Presets[i + 1]->PresetBagNdx;
            if (x1 < 0 || x2 < 0 || x1 > x2 || x2 >= PresetBags.size()) {
                throw Exception("Broken SF2 file (invalid PresetBagNdx)");
            }

            preset->LoadRegions(x1, x2);
        }
    }

    File::~File() {
        delete pInfo;
        for (int i = Presets.size() - 1; i >= 0; i--) {
            if (Presets[i]) delete (Presets[i]);
        }
        for (int i = Instruments.size() - 1; i >= 0; i--) {
            if (Instruments[i]) delete (Instruments[i]);
        }
        for (int i = Samples.size() - 1; i >= 0; i--) {
            if (Samples[i]) delete (Samples[i]);
        }
    }

    int File::GetPresetCount() {
        return Presets.size() - 1; // exclude terminal preset (EOP)
    }

    Preset* File::GetPreset(int idx) {
        if (idx < 0 || idx >= GetPresetCount()) {
            throw Exception("Preset index out of bounds");
        }

        return Presets[idx];
    }

    int File::GetInstrumentCount() {
        return Instruments.size() - 1; // exclude terminal instrument (EOI)
    }

    Instrument* File::GetInstrument(int idx) {
        if (idx < 0 || idx >= GetInstrumentCount()) {
            throw Exception("Instrument index out of bounds");
        }

        return Instruments[idx];
    }

    void File::DeleteInstrument(Instrument* pInstrument) {
        for (int i = 0; i < GetPresetCount(); i++) {
            Preset* p = GetPreset(i);
            if (p == NULL) continue;
            for (int j = p->GetRegionCount() - 1; j >= 0 ; j--) {
                if (p->GetRegion(j) && p->GetRegion(j)->pInstrument == pInstrument) {
                    p->GetRegion(j)->pInstrument = NULL;
                }
            }
        }

        for (int i = 0; i < GetInstrumentCount(); i++) {
            if (GetInstrument(i) == pInstrument) {
                Instruments[i] = NULL;
                delete pInstrument;
            }
        }
    }

    int File::GetSampleCount() {
        return Samples.size() - 1; // exclude terminal sample (EOS)
    }

    Sample* File::GetSample(int idx) {
        if (idx < 0 || idx >= GetSampleCount()) {
            throw Exception("Sample index out of bounds");
        }

        return Samples[idx];
    }

    void File::DeleteSample(Sample* pSample) {
        // Sanity check
        for (int i = GetInstrumentCount() - 1; i >= 0; i--) {
            Instrument* pInstr = GetInstrument(i);
            if (pInstr == NULL) continue;

            for (int j = pInstr->GetRegionCount() - 1; j >= 0 ; j--) {
                if (pInstr->GetRegion(j) && pInstr->GetRegion(j)->GetSample() == pSample) {
                    std::cerr << "Deleting sample which is still in use" << std::endl;
                }
            }
        }
        ///////

        for (int i = 0; i < GetSampleCount(); i++) {
            if (Samples[i] == pSample) {
                delete pSample;
                Samples[i] = NULL;
                return;
            }
        }

        throw Exception("Unknown sample: " + pSample->Name);
    }

    bool File::HasSamples() {
        for (int i = 0; i < GetSampleCount(); i++) {
            if (Samples[i] != NULL) return true;
        }

        return false;
    }

    /**
     * Loads the whole sample wave into RAM. Use
     * ReleaseSampleData() to free the memory if you don't need the cached
     * sample data anymore.
     *
     * @returns  buffer_t structure with start address and size of the buffer
     *           in bytes
     * @see      ReleaseSampleData(), Read(), SetPos()
     */
    Sample::buffer_t Sample::LoadSampleData() {
        return LoadSampleDataWithNullSamplesExtension(GetTotalFrameCount(), 0); // 0 amount of NullSamples
    }

    /**
     * Reads and caches the first \a SampleCount
     * numbers of SamplePoints in RAM. Use ReleaseSampleData() to free the
     * memory space if you don't need the cached samples anymore.
     * Read the <i>Size</i> member of the <i>buffer_t</i> structure
     * that will be returned to determine the actual cached samples, but note
     * that the size is given in bytes! You get the number of actually cached
     * samples by dividing it by the frame size of the sample:
     * @code
     *  buffer_t buf       = pSample->LoadSampleData(acquired_samples);
     *  long cachedsamples = buf.Size / pSample->FrameSize;
     * @endcode
     *
     * @param SampleCount - number of sample points to load into RAM
     * @returns             buffer_t structure with start address and size of
     *                      the cached sample data in bytes
     * @see                 ReleaseSampleData(), Read(), SetPos()
     */
    Sample::buffer_t Sample::LoadSampleData(unsigned long SampleCount) {
        return LoadSampleDataWithNullSamplesExtension(SampleCount, 0); // 0 amount of NullSamples
    }

    /**
     * Loads the whole sample wave into RAM. Use
     * ReleaseSampleData() to free the memory if you don't need the cached
     * sample data anymore.
     * The method will add \a NullSamplesCount silence samples past the
     * official buffer end (this won't affect the 'Size' member of the
     * buffer_t structure, that means 'Size' always reflects the size of the
     * actual sample data, the buffer might be bigger though). Silence
     * samples past the official buffer are needed for differential
     * algorithms that always have to take subsequent samples into account
     * (resampling/interpolation would be an important example) and avoids
     * memory access faults in such cases.
     *
     * @param NullSamplesCount - number of silence samples the buffer should
     *                           be extended past it's data end
     * @returns                  buffer_t structure with start address and
     *                           size of the buffer in bytes
     * @see                      ReleaseSampleData(), Read(), SetPos()
     */
    Sample::buffer_t Sample::LoadSampleDataWithNullSamplesExtension(uint NullSamplesCount) {
        return LoadSampleDataWithNullSamplesExtension(GetTotalFrameCount(), NullSamplesCount);
    }

    /**
     * Reads and caches the first \a SampleCount
     * numbers of SamplePoints in RAM. Use ReleaseSampleData() to free the
     * memory space if you don't need the cached samples anymore.
     * Read the <i>Size</i> member of the <i>buffer_t</i> structure
     * that will be returned to determine the actual cached samples, but note
     * that the size is given in bytes! You get the number of actually cached
     * samples by dividing it by the frame size of the sample:
     * @code
     *  buffer_t buf       = pSample->LoadSampleDataWithNullSamplesExtension(acquired_samples, null_samples);
     *  long cachedsamples = buf.Size / pSample->FrameSize;
     * @endcode
     * The method will add \a NullSamplesCount silence samples past the
     * official buffer end (this won't affect the 'Size' member of the
     * buffer_t structure, that means 'Size' always reflects the size of the
     * actual sample data, the buffer might be bigger though). Silence
     * samples past the official buffer are needed for differential
     * algorithms that always have to take subsequent samples into account
     * (resampling/interpolation would be an important example) and avoids
     * memory access faults in such cases.
     *
     * @param SampleCount      - number of sample points to load into RAM
     * @param NullSamplesCount - number of silence samples the buffer should
     *                           be extended past it's data end
     * @returns                  buffer_t structure with start address and
     *                           size of the cached sample data in bytes
     * @see                      ReleaseSampleData(), Read(), SetPos()
     */
    Sample::buffer_t Sample::LoadSampleDataWithNullSamplesExtension(unsigned long SampleCount, uint NullSamplesCount) {
        if (SampleCount > GetTotalFrameCount()) SampleCount = GetTotalFrameCount();
        if (RAMCache.pStart) delete[] (int8_t*) RAMCache.pStart;
        unsigned long allocationsize = (SampleCount + NullSamplesCount) * GetFrameSize();
        SetPos(0); // reset read position to begin of sample
        RAMCache.pStart            = new int8_t[allocationsize];
        RAMCache.Size              = Read(RAMCache.pStart, SampleCount) * GetFrameSize();
        RAMCache.NullExtensionSize = allocationsize - RAMCache.Size;
        // fill the remaining buffer space with silence samples
        memset((int8_t*)RAMCache.pStart + RAMCache.Size, 0, RAMCache.NullExtensionSize);
        return GetCache();
    }

    /**
     * Returns current cached sample points. A buffer_t structure will be
     * returned which contains address pointer to the begin of the cache and
     * the size of the cached sample data in bytes. Use
     * <i>LoadSampleData()</i> to cache a specific amount of sample points in
     * RAM.
     *
     * @returns  buffer_t structure with current cached sample points
     * @see      LoadSampleData();
     */
    Sample::buffer_t Sample::GetCache() {
        // return a copy of the buffer_t structure
        buffer_t result;
        result.Size              = this->RAMCache.Size;
        result.pStart            = this->RAMCache.pStart;
        result.NullExtensionSize = this->RAMCache.NullExtensionSize;
        return result;
    }

    /**
     * Frees the cached sample from RAM if loaded with
     * <i>LoadSampleData()</i> previously.
     *
     * @see  LoadSampleData();
     */
    void Sample::ReleaseSampleData() {
        if (RAMCache.pStart) delete[] (int8_t*) RAMCache.pStart;
        RAMCache.pStart = NULL;
        RAMCache.Size   = 0;
        RAMCache.NullExtensionSize = 0;
    }

    /**
     * Sets the position within the sample (in sample points, not in
     * bytes). Use this method and <i>Read()</i> if you don't want to load
     * the sample into RAM, thus for disk streaming.
     *
     * @param SampleCount  number of sample points to jump
     * @returns            the new sample position
     * @see                Read()
     */
    unsigned long Sample::SetPos(unsigned long SampleCount) {
        pCkSmpl->SetPos((Start * 2) + (SampleCount * 2), RIFF::stream_start);
        if(pCkSm24) pCkSm24->SetPos(Start + SampleCount, RIFF::stream_start);
        return SampleCount;
    }

    /**
     * Returns the current position in the sample (in sample points).
     */
    unsigned long Sample::GetPos() {
        return (pCkSmpl->GetPos() - (Start * 2)) / 2;
    }

    /**
     * Reads \a SampleCount number of sample points from the current
     * position into the buffer pointed by \a pBuffer and increments the
     * position within the sample. Use this method
     * and <i>SetPos()</i> if you don't want to load the sample into RAM,
     * thus for disk streaming.
     *
     * For 16 bit samples, the data in the buffer will be int16_t
     * (using native endianness). For 24 bit, the buffer will
     * contain three bytes per sample, little-endian.
     *
     * @param pBuffer      destination buffer
     * @param SampleCount  number of sample points to read
     * @returns            number of successfully read sample points
     * @see                SetPos()
     */
    unsigned long Sample::Read(void* pBuffer, unsigned long SampleCount) {
        // TODO: startAddrsCoarseOffset, endAddrsCoarseOffset
        if (SampleCount == 0) return 0;
        long pos = GetPos();
        if (pos + SampleCount > GetTotalFrameCount()) SampleCount = GetTotalFrameCount() - pos;

        if (GetFrameSize() / GetChannelCount() == 3 /* 24 bit */) {
            uint8_t* pBuf = (uint8_t*)pBuffer;
            if (SampleType == MONO_SAMPLE || SampleType == ROM_MONO_SAMPLE) {
                pCkSmpl->Read(pBuf, SampleCount, 2);
                pCkSm24->Read(pBuf + SampleCount * 2, SampleCount, 1);
                for (int i = SampleCount - 1; i >= 0; i--) {
                    pBuf[i*3] = pBuf[(SampleCount * 2) + i];
                    pBuf[i*3 + 2] = pBuf[i*2 + 1];
                    pBuf[i*3 + 1] = pBuf[i*2];
                }
            } else if (SampleType == LEFT_SAMPLE || SampleType == ROM_LEFT_SAMPLE) {
                pCkSmpl->Read(pBuf, SampleCount, 2);
                pCkSm24->Read(pBuf + SampleCount * 2, SampleCount, 1);
                for (int i = SampleCount - 1; i >= 0; i--) {
                    pBuf[i*6] = pBuf[(SampleCount * 2) + i];
                    pBuf[i*6 + 2] = pBuf[i*2 + 1];
                    pBuf[i*6 + 1] = pBuf[i*2];
                    pBuf[i*6 + 3] = pBuf[i*6 + 4] = pBuf[i*6 + 5] = 0;
                }
            } else if (SampleType == RIGHT_SAMPLE || SampleType == ROM_RIGHT_SAMPLE) {
                pCkSmpl->Read(pBuf, SampleCount, 2);
                pCkSm24->Read(pBuf + SampleCount * 2, SampleCount, 1);
                for (int i = SampleCount - 1; i >= 0; i--) {
                    pBuf[i*6 + 3] = pBuf[(SampleCount * 2) + i];
                    pBuf[i*6 + 5] = pBuf[i*2 + 1];
                    pBuf[i*6 + 4] = pBuf[i*2];
                    pBuf[i*6] = pBuf[i*6 + 1] = pBuf[i*6 + 2] = 0;
                }
            }
        } else {
            if (SampleType == MONO_SAMPLE || SampleType == ROM_MONO_SAMPLE) {
                return pCkSmpl->Read(pBuffer, SampleCount, 2);
            }

            int16_t* pBuf = (int16_t*)pBuffer;
            if (SampleType == LEFT_SAMPLE || SampleType == ROM_LEFT_SAMPLE) {
                pCkSmpl->Read(pBuf, SampleCount, 2);
                for (int i = SampleCount - 1; i >= 0; i--) {
                    pBuf[i*2] = pBuf[i];
                    pBuf[i*2 + 1] = 0;
                }
            } else if (SampleType == RIGHT_SAMPLE || SampleType == ROM_RIGHT_SAMPLE) {
                pCkSmpl->Read(pBuf, SampleCount, 2);
                for (int i = SampleCount - 1; i >= 0; i--) {
                    pBuf[i*2] = 0;
                    pBuf[i*2 + 1] = pBuf[i];
                }
            }
        }

        if (pCkSmpl->GetPos() > (End * 2)) {
            std::cerr << "Read after the sample end. This is a BUG!" << std::endl;
            std::cerr << "Current position: " << GetPos() << std::endl;
            std::cerr << "Total number of frames: " << GetTotalFrameCount() << std::endl << std::endl;
        }
        return SampleCount;
    }


// *************** functions ***************
// *

    /**
     * Returns the name of this C++ library.
     */
    String libraryName() {
        return PACKAGE;
    }

    /**
     * Returns version of this C++ library.
     */
    String libraryVersion() {
        return VERSION;
    }

} // namespace sf2
