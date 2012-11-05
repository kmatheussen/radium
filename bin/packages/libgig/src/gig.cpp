/***************************************************************************
 *                                                                         *
 *   libgig - C++ cross-platform Gigasampler format file access library    *
 *                                                                         *
 *   Copyright (C) 2003-2009 by Christian Schoenebeck                      *
 *                              <cuse@users.sourceforge.net>               *
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

#include "gig.h"

#include "helper.h"

#include <algorithm>
#include <math.h>
#include <iostream>

/// Initial size of the sample buffer which is used for decompression of
/// compressed sample wave streams - this value should always be bigger than
/// the biggest sample piece expected to be read by the sampler engine,
/// otherwise the buffer size will be raised at runtime and thus the buffer
/// reallocated which is time consuming and unefficient.
#define INITIAL_SAMPLE_BUFFER_SIZE              512000 // 512 kB

/** (so far) every exponential paramater in the gig format has a basis of 1.000000008813822 */
#define GIG_EXP_DECODE(x)                       (pow(1.000000008813822, x))
#define GIG_EXP_ENCODE(x)                       (log(x) / log(1.000000008813822))
#define GIG_PITCH_TRACK_EXTRACT(x)              (!(x & 0x01))
#define GIG_PITCH_TRACK_ENCODE(x)               ((x) ? 0x00 : 0x01)
#define GIG_VCF_RESONANCE_CTRL_EXTRACT(x)       ((x >> 4) & 0x03)
#define GIG_VCF_RESONANCE_CTRL_ENCODE(x)        ((x & 0x03) << 4)
#define GIG_EG_CTR_ATTACK_INFLUENCE_EXTRACT(x)  ((x >> 1) & 0x03)
#define GIG_EG_CTR_DECAY_INFLUENCE_EXTRACT(x)   ((x >> 3) & 0x03)
#define GIG_EG_CTR_RELEASE_INFLUENCE_EXTRACT(x) ((x >> 5) & 0x03)
#define GIG_EG_CTR_ATTACK_INFLUENCE_ENCODE(x)   ((x & 0x03) << 1)
#define GIG_EG_CTR_DECAY_INFLUENCE_ENCODE(x)    ((x & 0x03) << 3)
#define GIG_EG_CTR_RELEASE_INFLUENCE_ENCODE(x)  ((x & 0x03) << 5)

namespace gig {

// *************** progress_t ***************
// *

    progress_t::progress_t() {
        callback    = NULL;
        custom      = NULL;
        __range_min = 0.0f;
        __range_max = 1.0f;
    }

    // private helper function to convert progress of a subprocess into the global progress
    static void __notify_progress(progress_t* pProgress, float subprogress) {
        if (pProgress && pProgress->callback) {
            const float totalrange    = pProgress->__range_max - pProgress->__range_min;
            const float totalprogress = pProgress->__range_min + subprogress * totalrange;
            pProgress->factor         = totalprogress;
            pProgress->callback(pProgress); // now actually notify about the progress
        }
    }

    // private helper function to divide a progress into subprogresses
    static void __divide_progress(progress_t* pParentProgress, progress_t* pSubProgress, float totalTasks, float currentTask) {
        if (pParentProgress && pParentProgress->callback) {
            const float totalrange    = pParentProgress->__range_max - pParentProgress->__range_min;
            pSubProgress->callback    = pParentProgress->callback;
            pSubProgress->custom      = pParentProgress->custom;
            pSubProgress->__range_min = pParentProgress->__range_min + totalrange * currentTask / totalTasks;
            pSubProgress->__range_max = pSubProgress->__range_min + totalrange / totalTasks;
        }
    }


// *************** Internal functions for sample decompression ***************
// *

namespace {

    inline int get12lo(const unsigned char* pSrc)
    {
        const int x = pSrc[0] | (pSrc[1] & 0x0f) << 8;
        return x & 0x800 ? x - 0x1000 : x;
    }

    inline int get12hi(const unsigned char* pSrc)
    {
        const int x = pSrc[1] >> 4 | pSrc[2] << 4;
        return x & 0x800 ? x - 0x1000 : x;
    }

    inline int16_t get16(const unsigned char* pSrc)
    {
        return int16_t(pSrc[0] | pSrc[1] << 8);
    }

    inline int get24(const unsigned char* pSrc)
    {
        const int x = pSrc[0] | pSrc[1] << 8 | pSrc[2] << 16;
        return x & 0x800000 ? x - 0x1000000 : x;
    }

    inline void store24(unsigned char* pDst, int x)
    {
        pDst[0] = x;
        pDst[1] = x >> 8;
        pDst[2] = x >> 16;
    }

    void Decompress16(int compressionmode, const unsigned char* params,
                      int srcStep, int dstStep,
                      const unsigned char* pSrc, int16_t* pDst,
                      unsigned long currentframeoffset,
                      unsigned long copysamples)
    {
        switch (compressionmode) {
            case 0: // 16 bit uncompressed
                pSrc += currentframeoffset * srcStep;
                while (copysamples) {
                    *pDst = get16(pSrc);
                    pDst += dstStep;
                    pSrc += srcStep;
                    copysamples--;
                }
                break;

            case 1: // 16 bit compressed to 8 bit
                int y  = get16(params);
                int dy = get16(params + 2);
                while (currentframeoffset) {
                    dy -= int8_t(*pSrc);
                    y  -= dy;
                    pSrc += srcStep;
                    currentframeoffset--;
                }
                while (copysamples) {
                    dy -= int8_t(*pSrc);
                    y  -= dy;
                    *pDst = y;
                    pDst += dstStep;
                    pSrc += srcStep;
                    copysamples--;
                }
                break;
        }
    }

    void Decompress24(int compressionmode, const unsigned char* params,
                      int dstStep, const unsigned char* pSrc, uint8_t* pDst,
                      unsigned long currentframeoffset,
                      unsigned long copysamples, int truncatedBits)
    {
        int y, dy, ddy, dddy;

#define GET_PARAMS(params)                      \
        y    = get24(params);                   \
        dy   = y - get24((params) + 3);         \
        ddy  = get24((params) + 6);             \
        dddy = get24((params) + 9)

#define SKIP_ONE(x)                             \
        dddy -= (x);                            \
        ddy  -= dddy;                           \
        dy   =  -dy - ddy;                      \
        y    += dy

#define COPY_ONE(x)                             \
        SKIP_ONE(x);                            \
        store24(pDst, y << truncatedBits);      \
        pDst += dstStep

        switch (compressionmode) {
            case 2: // 24 bit uncompressed
                pSrc += currentframeoffset * 3;
                while (copysamples) {
                    store24(pDst, get24(pSrc) << truncatedBits);
                    pDst += dstStep;
                    pSrc += 3;
                    copysamples--;
                }
                break;

            case 3: // 24 bit compressed to 16 bit
                GET_PARAMS(params);
                while (currentframeoffset) {
                    SKIP_ONE(get16(pSrc));
                    pSrc += 2;
                    currentframeoffset--;
                }
                while (copysamples) {
                    COPY_ONE(get16(pSrc));
                    pSrc += 2;
                    copysamples--;
                }
                break;

            case 4: // 24 bit compressed to 12 bit
                GET_PARAMS(params);
                while (currentframeoffset > 1) {
                    SKIP_ONE(get12lo(pSrc));
                    SKIP_ONE(get12hi(pSrc));
                    pSrc += 3;
                    currentframeoffset -= 2;
                }
                if (currentframeoffset) {
                    SKIP_ONE(get12lo(pSrc));
                    currentframeoffset--;
                    if (copysamples) {
                        COPY_ONE(get12hi(pSrc));
                        pSrc += 3;
                        copysamples--;
                    }
                }
                while (copysamples > 1) {
                    COPY_ONE(get12lo(pSrc));
                    COPY_ONE(get12hi(pSrc));
                    pSrc += 3;
                    copysamples -= 2;
                }
                if (copysamples) {
                    COPY_ONE(get12lo(pSrc));
                }
                break;

            case 5: // 24 bit compressed to 8 bit
                GET_PARAMS(params);
                while (currentframeoffset) {
                    SKIP_ONE(int8_t(*pSrc++));
                    currentframeoffset--;
                }
                while (copysamples) {
                    COPY_ONE(int8_t(*pSrc++));
                    copysamples--;
                }
                break;
        }
    }

    const int bytesPerFrame[] =      { 4096, 2052, 768, 524, 396, 268 };
    const int bytesPerFrameNoHdr[] = { 4096, 2048, 768, 512, 384, 256 };
    const int headerSize[] =         { 0, 4, 0, 12, 12, 12 };
    const int bitsPerSample[] =      { 16, 8, 24, 16, 12, 8 };
}



// *************** Internal CRC-32 (Cyclic Redundancy Check) functions  ***************
// *

    static uint32_t* __initCRCTable() {
        static uint32_t res[256];

        for (int i = 0 ; i < 256 ; i++) {
            uint32_t c = i;
            for (int j = 0 ; j < 8 ; j++) {
                c = (c & 1) ? 0xedb88320 ^ (c >> 1) : c >> 1;
            }
            res[i] = c;
        }
        return res;
    }

    static const uint32_t* __CRCTable = __initCRCTable();

    /**
     * Initialize a CRC variable.
     *
     * @param crc - variable to be initialized
     */
    inline static void __resetCRC(uint32_t& crc) {
        crc = 0xffffffff;
    }

    /**
     * Used to calculate checksums of the sample data in a gig file. The
     * checksums are stored in the 3crc chunk of the gig file and
     * automatically updated when a sample is written with Sample::Write().
     *
     * One should call __resetCRC() to initialize the CRC variable to be
     * used before calling this function the first time.
     *
     * After initializing the CRC variable one can call this function
     * arbitrary times, i.e. to split the overall CRC calculation into
     * steps.
     *
     * Once the whole data was processed by __calculateCRC(), one should
     * call __encodeCRC() to get the final CRC result.
     *
     * @param buf     - pointer to data the CRC shall be calculated of
     * @param bufSize - size of the data to be processed
     * @param crc     - variable the CRC sum shall be stored to
     */
    static void __calculateCRC(unsigned char* buf, int bufSize, uint32_t& crc) {
        for (int i = 0 ; i < bufSize ; i++) {
            crc = __CRCTable[(crc ^ buf[i]) & 0xff] ^ (crc >> 8);
        }
    }

    /**
     * Returns the final CRC result.
     *
     * @param crc - variable previously passed to __calculateCRC()
     */
    inline static uint32_t __encodeCRC(const uint32_t& crc) {
        return crc ^ 0xffffffff;
    }



// *************** Other Internal functions  ***************
// *

    static split_type_t __resolveSplitType(dimension_t dimension) {
        return (
            dimension == dimension_layer ||
            dimension == dimension_samplechannel ||
            dimension == dimension_releasetrigger ||
            dimension == dimension_keyboard ||
            dimension == dimension_roundrobin ||
            dimension == dimension_random ||
            dimension == dimension_smartmidi ||
            dimension == dimension_roundrobinkeyboard
        ) ? split_type_bit : split_type_normal;
    }

    static int __resolveZoneSize(dimension_def_t& dimension_definition) {
        return (dimension_definition.split_type == split_type_normal)
        ? int(128.0 / dimension_definition.zones) : 0;
    }



// *************** Sample ***************
// *

    unsigned int Sample::Instances = 0;
    buffer_t     Sample::InternalDecompressionBuffer;

    /** @brief Constructor.
     *
     * Load an existing sample or create a new one. A 'wave' list chunk must
     * be given to this constructor. In case the given 'wave' list chunk
     * contains a 'fmt', 'data' (and optionally a '3gix', 'smpl') chunk, the
     * format and sample data will be loaded from there, otherwise default
     * values will be used and those chunks will be created when
     * File::Save() will be called later on.
     *
     * @param pFile          - pointer to gig::File where this sample is
     *                         located (or will be located)
     * @param waveList       - pointer to 'wave' list chunk which is (or
     *                         will be) associated with this sample
     * @param WavePoolOffset - offset of this sample data from wave pool
     *                         ('wvpl') list chunk
     * @param fileNo         - number of an extension file where this sample
     *                         is located, 0 otherwise
     */
    Sample::Sample(File* pFile, RIFF::List* waveList, unsigned long WavePoolOffset, unsigned long fileNo) : DLS::Sample((DLS::File*) pFile, waveList, WavePoolOffset) {
        static const DLS::Info::string_length_t fixedStringLengths[] = {
            { CHUNK_ID_INAM, 64 },
            { 0, 0 }
        };
        pInfo->SetFixedStringLengths(fixedStringLengths);
        Instances++;
        FileNo = fileNo;

        __resetCRC(crc);

        pCk3gix = waveList->GetSubChunk(CHUNK_ID_3GIX);
        if (pCk3gix) {
            uint16_t iSampleGroup = pCk3gix->ReadInt16();
            pGroup = pFile->GetGroup(iSampleGroup);
        } else { // '3gix' chunk missing
            // by default assigned to that mandatory "Default Group"
            pGroup = pFile->GetGroup(0);
        }

        pCkSmpl = waveList->GetSubChunk(CHUNK_ID_SMPL);
        if (pCkSmpl) {
            Manufacturer  = pCkSmpl->ReadInt32();
            Product       = pCkSmpl->ReadInt32();
            SamplePeriod  = pCkSmpl->ReadInt32();
            MIDIUnityNote = pCkSmpl->ReadInt32();
            FineTune      = pCkSmpl->ReadInt32();
            pCkSmpl->Read(&SMPTEFormat, 1, 4);
            SMPTEOffset   = pCkSmpl->ReadInt32();
            Loops         = pCkSmpl->ReadInt32();
            pCkSmpl->ReadInt32(); // manufByt
            LoopID        = pCkSmpl->ReadInt32();
            pCkSmpl->Read(&LoopType, 1, 4);
            LoopStart     = pCkSmpl->ReadInt32();
            LoopEnd       = pCkSmpl->ReadInt32();
            LoopFraction  = pCkSmpl->ReadInt32();
            LoopPlayCount = pCkSmpl->ReadInt32();
        } else { // 'smpl' chunk missing
            // use default values
            Manufacturer  = 0;
            Product       = 0;
            SamplePeriod  = uint32_t(1000000000.0 / SamplesPerSecond + 0.5);
            MIDIUnityNote = 60;
            FineTune      = 0;
            SMPTEFormat   = smpte_format_no_offset;
            SMPTEOffset   = 0;
            Loops         = 0;
            LoopID        = 0;
            LoopType      = loop_type_normal;
            LoopStart     = 0;
            LoopEnd       = 0;
            LoopFraction  = 0;
            LoopPlayCount = 0;
        }

        FrameTable                 = NULL;
        SamplePos                  = 0;
        RAMCache.Size              = 0;
        RAMCache.pStart            = NULL;
        RAMCache.NullExtensionSize = 0;

        if (BitDepth > 24) throw gig::Exception("Only samples up to 24 bit supported");

        RIFF::Chunk* ewav = waveList->GetSubChunk(CHUNK_ID_EWAV);
        Compressed        = ewav;
        Dithered          = false;
        TruncatedBits     = 0;
        if (Compressed) {
            uint32_t version = ewav->ReadInt32();
            if (version == 3 && BitDepth == 24) {
                Dithered = ewav->ReadInt32();
                ewav->SetPos(Channels == 2 ? 84 : 64);
                TruncatedBits = ewav->ReadInt32();
            }
            ScanCompressedSample();
        }

        // we use a buffer for decompression and for truncating 24 bit samples to 16 bit
        if ((Compressed || BitDepth == 24) && !InternalDecompressionBuffer.Size) {
            InternalDecompressionBuffer.pStart = new unsigned char[INITIAL_SAMPLE_BUFFER_SIZE];
            InternalDecompressionBuffer.Size   = INITIAL_SAMPLE_BUFFER_SIZE;
        }
        FrameOffset = 0; // just for streaming compressed samples

        LoopSize = LoopEnd - LoopStart + 1;
    }

    /**
     * Apply sample and its settings to the respective RIFF chunks. You have
     * to call File::Save() to make changes persistent.
     *
     * Usually there is absolutely no need to call this method explicitly.
     * It will be called automatically when File::Save() was called.
     *
     * @throws DLS::Exception if FormatTag != DLS_WAVE_FORMAT_PCM or no sample data
     *                        was provided yet
     * @throws gig::Exception if there is any invalid sample setting
     */
    void Sample::UpdateChunks() {
        // first update base class's chunks
        DLS::Sample::UpdateChunks();

        // make sure 'smpl' chunk exists
        pCkSmpl = pWaveList->GetSubChunk(CHUNK_ID_SMPL);
        if (!pCkSmpl) {
            pCkSmpl = pWaveList->AddSubChunk(CHUNK_ID_SMPL, 60);
            memset(pCkSmpl->LoadChunkData(), 0, 60);
        }
        // update 'smpl' chunk
        uint8_t* pData = (uint8_t*) pCkSmpl->LoadChunkData();
        SamplePeriod = uint32_t(1000000000.0 / SamplesPerSecond + 0.5);
        store32(&pData[0], Manufacturer);
        store32(&pData[4], Product);
        store32(&pData[8], SamplePeriod);
        store32(&pData[12], MIDIUnityNote);
        store32(&pData[16], FineTune);
        store32(&pData[20], SMPTEFormat);
        store32(&pData[24], SMPTEOffset);
        store32(&pData[28], Loops);

        // we skip 'manufByt' for now (4 bytes)

        store32(&pData[36], LoopID);
        store32(&pData[40], LoopType);
        store32(&pData[44], LoopStart);
        store32(&pData[48], LoopEnd);
        store32(&pData[52], LoopFraction);
        store32(&pData[56], LoopPlayCount);

        // make sure '3gix' chunk exists
        pCk3gix = pWaveList->GetSubChunk(CHUNK_ID_3GIX);
        if (!pCk3gix) pCk3gix = pWaveList->AddSubChunk(CHUNK_ID_3GIX, 4);
        // determine appropriate sample group index (to be stored in chunk)
        uint16_t iSampleGroup = 0; // 0 refers to default sample group
        File* pFile = static_cast<File*>(pParent);
        if (pFile->pGroups) {
            std::list<Group*>::iterator iter = pFile->pGroups->begin();
            std::list<Group*>::iterator end  = pFile->pGroups->end();
            for (int i = 0; iter != end; i++, iter++) {
                if (*iter == pGroup) {
                    iSampleGroup = i;
                    break; // found
                }
            }
        }
        // update '3gix' chunk
        pData = (uint8_t*) pCk3gix->LoadChunkData();
        store16(&pData[0], iSampleGroup);
    }

    /// Scans compressed samples for mandatory informations (e.g. actual number of total sample points).
    void Sample::ScanCompressedSample() {
        //TODO: we have to add some more scans here (e.g. determine compression rate)
        this->SamplesTotal = 0;
        std::list<unsigned long> frameOffsets;

        SamplesPerFrame = BitDepth == 24 ? 256 : 2048;
        WorstCaseFrameSize = SamplesPerFrame * FrameSize + Channels; // +Channels for compression flag

        // Scanning
        pCkData->SetPos(0);
        if (Channels == 2) { // Stereo
            for (int i = 0 ; ; i++) {
                // for 24 bit samples every 8:th frame offset is
                // stored, to save some memory
                if (BitDepth != 24 || (i & 7) == 0) frameOffsets.push_back(pCkData->GetPos());

                const int mode_l = pCkData->ReadUint8();
                const int mode_r = pCkData->ReadUint8();
                if (mode_l > 5 || mode_r > 5) throw gig::Exception("Unknown compression mode");
                const unsigned long frameSize = bytesPerFrame[mode_l] + bytesPerFrame[mode_r];

                if (pCkData->RemainingBytes() <= frameSize) {
                    SamplesInLastFrame =
                        ((pCkData->RemainingBytes() - headerSize[mode_l] - headerSize[mode_r]) << 3) /
                        (bitsPerSample[mode_l] + bitsPerSample[mode_r]);
                    SamplesTotal += SamplesInLastFrame;
                    break;
                }
                SamplesTotal += SamplesPerFrame;
                pCkData->SetPos(frameSize, RIFF::stream_curpos);
            }
        }
        else { // Mono
            for (int i = 0 ; ; i++) {
                if (BitDepth != 24 || (i & 7) == 0) frameOffsets.push_back(pCkData->GetPos());

                const int mode = pCkData->ReadUint8();
                if (mode > 5) throw gig::Exception("Unknown compression mode");
                const unsigned long frameSize = bytesPerFrame[mode];

                if (pCkData->RemainingBytes() <= frameSize) {
                    SamplesInLastFrame =
                        ((pCkData->RemainingBytes() - headerSize[mode]) << 3) / bitsPerSample[mode];
                    SamplesTotal += SamplesInLastFrame;
                    break;
                }
                SamplesTotal += SamplesPerFrame;
                pCkData->SetPos(frameSize, RIFF::stream_curpos);
            }
        }
        pCkData->SetPos(0);

        // Build the frames table (which is used for fast resolving of a frame's chunk offset)
        if (FrameTable) delete[] FrameTable;
        FrameTable = new unsigned long[frameOffsets.size()];
        std::list<unsigned long>::iterator end  = frameOffsets.end();
        std::list<unsigned long>::iterator iter = frameOffsets.begin();
        for (int i = 0; iter != end; i++, iter++) {
            FrameTable[i] = *iter;
        }
    }

    /**
     * Loads (and uncompresses if needed) the whole sample wave into RAM. Use
     * ReleaseSampleData() to free the memory if you don't need the cached
     * sample data anymore.
     *
     * @returns  buffer_t structure with start address and size of the buffer
     *           in bytes
     * @see      ReleaseSampleData(), Read(), SetPos()
     */
    buffer_t Sample::LoadSampleData() {
        return LoadSampleDataWithNullSamplesExtension(this->SamplesTotal, 0); // 0 amount of NullSamples
    }

    /**
     * Reads (uncompresses if needed) and caches the first \a SampleCount
     * numbers of SamplePoints in RAM. Use ReleaseSampleData() to free the
     * memory space if you don't need the cached samples anymore. There is no
     * guarantee that exactly \a SampleCount samples will be cached; this is
     * not an error. The size will be eventually truncated e.g. to the
     * beginning of a frame of a compressed sample. This is done for
     * efficiency reasons while streaming the wave by your sampler engine
     * later. Read the <i>Size</i> member of the <i>buffer_t</i> structure
     * that will be returned to determine the actual cached samples, but note
     * that the size is given in bytes! You get the number of actually cached
     * samples by dividing it by the frame size of the sample:
     * @code
     * 	buffer_t buf       = pSample->LoadSampleData(acquired_samples);
     * 	long cachedsamples = buf.Size / pSample->FrameSize;
     * @endcode
     *
     * @param SampleCount - number of sample points to load into RAM
     * @returns             buffer_t structure with start address and size of
     *                      the cached sample data in bytes
     * @see                 ReleaseSampleData(), Read(), SetPos()
     */
    buffer_t Sample::LoadSampleData(unsigned long SampleCount) {
        return LoadSampleDataWithNullSamplesExtension(SampleCount, 0); // 0 amount of NullSamples
    }

    /**
     * Loads (and uncompresses if needed) the whole sample wave into RAM. Use
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
    buffer_t Sample::LoadSampleDataWithNullSamplesExtension(uint NullSamplesCount) {
        return LoadSampleDataWithNullSamplesExtension(this->SamplesTotal, NullSamplesCount);
    }

    /**
     * Reads (uncompresses if needed) and caches the first \a SampleCount
     * numbers of SamplePoints in RAM. Use ReleaseSampleData() to free the
     * memory space if you don't need the cached samples anymore. There is no
     * guarantee that exactly \a SampleCount samples will be cached; this is
     * not an error. The size will be eventually truncated e.g. to the
     * beginning of a frame of a compressed sample. This is done for
     * efficiency reasons while streaming the wave by your sampler engine
     * later. Read the <i>Size</i> member of the <i>buffer_t</i> structure
     * that will be returned to determine the actual cached samples, but note
     * that the size is given in bytes! You get the number of actually cached
     * samples by dividing it by the frame size of the sample:
     * @code
     * 	buffer_t buf       = pSample->LoadSampleDataWithNullSamplesExtension(acquired_samples, null_samples);
     * 	long cachedsamples = buf.Size / pSample->FrameSize;
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
    buffer_t Sample::LoadSampleDataWithNullSamplesExtension(unsigned long SampleCount, uint NullSamplesCount) {
        if (SampleCount > this->SamplesTotal) SampleCount = this->SamplesTotal;
        if (RAMCache.pStart) delete[] (int8_t*) RAMCache.pStart;
        unsigned long allocationsize = (SampleCount + NullSamplesCount) * this->FrameSize;
        SetPos(0); // reset read position to begin of sample
        RAMCache.pStart            = new int8_t[allocationsize];
        RAMCache.Size              = Read(RAMCache.pStart, SampleCount) * this->FrameSize;
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
    buffer_t Sample::GetCache() {
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

    /** @brief Resize sample.
     *
     * Resizes the sample's wave form data, that is the actual size of
     * sample wave data possible to be written for this sample. This call
     * will return immediately and just schedule the resize operation. You
     * should call File::Save() to actually perform the resize operation(s)
     * "physically" to the file. As this can take a while on large files, it
     * is recommended to call Resize() first on all samples which have to be
     * resized and finally to call File::Save() to perform all those resize
     * operations in one rush.
     *
     * The actual size (in bytes) is dependant to the current FrameSize
     * value. You may want to set FrameSize before calling Resize().
     *
     * <b>Caution:</b> You cannot directly write (i.e. with Write()) to
     * enlarged samples before calling File::Save() as this might exceed the
     * current sample's boundary!
     *
     * Also note: only DLS_WAVE_FORMAT_PCM is currently supported, that is
     * FormatTag must be DLS_WAVE_FORMAT_PCM. Trying to resize samples with
     * other formats will fail!
     *
     * @param iNewSize - new sample wave data size in sample points (must be
     *                   greater than zero)
     * @throws DLS::Excecption if FormatTag != DLS_WAVE_FORMAT_PCM
     *                         or if \a iNewSize is less than 1
     * @throws gig::Exception if existing sample is compressed
     * @see DLS::Sample::GetSize(), DLS::Sample::FrameSize,
     *      DLS::Sample::FormatTag, File::Save()
     */
    void Sample::Resize(int iNewSize) {
        if (Compressed) throw gig::Exception("There is no support for modifying compressed samples (yet)");
        DLS::Sample::Resize(iNewSize);
    }

    /**
     * Sets the position within the sample (in sample points, not in
     * bytes). Use this method and <i>Read()</i> if you don't want to load
     * the sample into RAM, thus for disk streaming.
     *
     * Although the original Gigasampler engine doesn't allow positioning
     * within compressed samples, I decided to implement it. Even though
     * the Gigasampler format doesn't allow to define loops for compressed
     * samples at the moment, positioning within compressed samples might be
     * interesting for some sampler engines though. The only drawback about
     * my decision is that it takes longer to load compressed gig Files on
     * startup, because it's neccessary to scan the samples for some
     * mandatory informations. But I think as it doesn't affect the runtime
     * efficiency, nobody will have a problem with that.
     *
     * @param SampleCount  number of sample points to jump
     * @param Whence       optional: to which relation \a SampleCount refers
     *                     to, if omited <i>RIFF::stream_start</i> is assumed
     * @returns            the new sample position
     * @see                Read()
     */
    unsigned long Sample::SetPos(unsigned long SampleCount, RIFF::stream_whence_t Whence) {
        if (Compressed) {
            switch (Whence) {
                case RIFF::stream_curpos:
                    this->SamplePos += SampleCount;
                    break;
                case RIFF::stream_end:
                    this->SamplePos = this->SamplesTotal - 1 - SampleCount;
                    break;
                case RIFF::stream_backward:
                    this->SamplePos -= SampleCount;
                    break;
                case RIFF::stream_start: default:
                    this->SamplePos = SampleCount;
                    break;
            }
            if (this->SamplePos > this->SamplesTotal) this->SamplePos = this->SamplesTotal;

            unsigned long frame = this->SamplePos / 2048; // to which frame to jump
            this->FrameOffset   = this->SamplePos % 2048; // offset (in sample points) within that frame
            pCkData->SetPos(FrameTable[frame]);           // set chunk pointer to the start of sought frame
            return this->SamplePos;
        }
        else { // not compressed
            unsigned long orderedBytes = SampleCount * this->FrameSize;
            unsigned long result = pCkData->SetPos(orderedBytes, Whence);
            return (result == orderedBytes) ? SampleCount
                                            : result / this->FrameSize;
        }
    }

    /**
     * Returns the current position in the sample (in sample points).
     */
    unsigned long Sample::GetPos() {
        if (Compressed) return SamplePos;
        else            return pCkData->GetPos() / FrameSize;
    }

    /**
     * Reads \a SampleCount number of sample points from the position stored
     * in \a pPlaybackState into the buffer pointed by \a pBuffer and moves
     * the position within the sample respectively, this method honors the
     * looping informations of the sample (if any). The sample wave stream
     * will be decompressed on the fly if using a compressed sample. Use this
     * method if you don't want to load the sample into RAM, thus for disk
     * streaming. All this methods needs to know to proceed with streaming
     * for the next time you call this method is stored in \a pPlaybackState.
     * You have to allocate and initialize the playback_state_t structure by
     * yourself before you use it to stream a sample:
     * @code
     * gig::playback_state_t playbackstate;
     * playbackstate.position         = 0;
     * playbackstate.reverse          = false;
     * playbackstate.loop_cycles_left = pSample->LoopPlayCount;
     * @endcode
     * You don't have to take care of things like if there is actually a loop
     * defined or if the current read position is located within a loop area.
     * The method already handles such cases by itself.
     *
     * <b>Caution:</b> If you are using more than one streaming thread, you
     * have to use an external decompression buffer for <b>EACH</b>
     * streaming thread to avoid race conditions and crashes!
     *
     * @param pBuffer          destination buffer
     * @param SampleCount      number of sample points to read
     * @param pPlaybackState   will be used to store and reload the playback
     *                         state for the next ReadAndLoop() call
     * @param pDimRgn          dimension region with looping information
     * @param pExternalDecompressionBuffer  (optional) external buffer to use for decompression
     * @returns                number of successfully read sample points
     * @see                    CreateDecompressionBuffer()
     */
    unsigned long Sample::ReadAndLoop(void* pBuffer, unsigned long SampleCount, playback_state_t* pPlaybackState,
                                      DimensionRegion* pDimRgn, buffer_t* pExternalDecompressionBuffer) {
        unsigned long samplestoread = SampleCount, totalreadsamples = 0, readsamples, samplestoloopend;
        uint8_t* pDst = (uint8_t*) pBuffer;

        SetPos(pPlaybackState->position); // recover position from the last time

        if (pDimRgn->SampleLoops) { // honor looping if there are loop points defined

            const DLS::sample_loop_t& loop = pDimRgn->pSampleLoops[0];
            const uint32_t loopEnd = loop.LoopStart + loop.LoopLength;

            if (GetPos() <= loopEnd) {
                switch (loop.LoopType) {

                    case loop_type_bidirectional: { //TODO: not tested yet!
                        do {
                            // if not endless loop check if max. number of loop cycles have been passed
                            if (this->LoopPlayCount && !pPlaybackState->loop_cycles_left) break;

                            if (!pPlaybackState->reverse) { // forward playback
                                do {
                                    samplestoloopend  = loopEnd - GetPos();
                                    readsamples       = Read(&pDst[totalreadsamples * this->FrameSize], Min(samplestoread, samplestoloopend), pExternalDecompressionBuffer);
                                    samplestoread    -= readsamples;
                                    totalreadsamples += readsamples;
                                    if (readsamples == samplestoloopend) {
                                        pPlaybackState->reverse = true;
                                        break;
                                    }
                                } while (samplestoread && readsamples);
                            }
                            else { // backward playback

                                // as we can only read forward from disk, we have to
                                // determine the end position within the loop first,
                                // read forward from that 'end' and finally after
                                // reading, swap all sample frames so it reflects
                                // backward playback

                                unsigned long swapareastart       = totalreadsamples;
                                unsigned long loopoffset          = GetPos() - loop.LoopStart;
                                unsigned long samplestoreadinloop = Min(samplestoread, loopoffset);
                                unsigned long reverseplaybackend  = GetPos() - samplestoreadinloop;

                                SetPos(reverseplaybackend);

                                // read samples for backward playback
                                do {
                                    readsamples          = Read(&pDst[totalreadsamples * this->FrameSize], samplestoreadinloop, pExternalDecompressionBuffer);
                                    samplestoreadinloop -= readsamples;
                                    samplestoread       -= readsamples;
                                    totalreadsamples    += readsamples;
                                } while (samplestoreadinloop && readsamples);

                                SetPos(reverseplaybackend); // pretend we really read backwards

                                if (reverseplaybackend == loop.LoopStart) {
                                    pPlaybackState->loop_cycles_left--;
                                    pPlaybackState->reverse = false;
                                }

                                // reverse the sample frames for backward playback
                                if (totalreadsamples > swapareastart) //FIXME: this if() is just a crash workaround for now (#102), but totalreadsamples <= swapareastart should never be the case, so there's probably still a bug above!
                                    SwapMemoryArea(&pDst[swapareastart * this->FrameSize], (totalreadsamples - swapareastart) * this->FrameSize, this->FrameSize);
                            }
                        } while (samplestoread && readsamples);
                        break;
                    }

                    case loop_type_backward: { // TODO: not tested yet!
                        // forward playback (not entered the loop yet)
                        if (!pPlaybackState->reverse) do {
                            samplestoloopend  = loopEnd - GetPos();
                            readsamples       = Read(&pDst[totalreadsamples * this->FrameSize], Min(samplestoread, samplestoloopend), pExternalDecompressionBuffer);
                            samplestoread    -= readsamples;
                            totalreadsamples += readsamples;
                            if (readsamples == samplestoloopend) {
                                pPlaybackState->reverse = true;
                                break;
                            }
                        } while (samplestoread && readsamples);

                        if (!samplestoread) break;

                        // as we can only read forward from disk, we have to
                        // determine the end position within the loop first,
                        // read forward from that 'end' and finally after
                        // reading, swap all sample frames so it reflects
                        // backward playback

                        unsigned long swapareastart       = totalreadsamples;
                        unsigned long loopoffset          = GetPos() - loop.LoopStart;
                        unsigned long samplestoreadinloop = (this->LoopPlayCount) ? Min(samplestoread, pPlaybackState->loop_cycles_left * loop.LoopLength - loopoffset)
                                                                                  : samplestoread;
                        unsigned long reverseplaybackend  = loop.LoopStart + Abs((loopoffset - samplestoreadinloop) % loop.LoopLength);

                        SetPos(reverseplaybackend);

                        // read samples for backward playback
                        do {
                            // if not endless loop check if max. number of loop cycles have been passed
                            if (this->LoopPlayCount && !pPlaybackState->loop_cycles_left) break;
                            samplestoloopend     = loopEnd - GetPos();
                            readsamples          = Read(&pDst[totalreadsamples * this->FrameSize], Min(samplestoreadinloop, samplestoloopend), pExternalDecompressionBuffer);
                            samplestoreadinloop -= readsamples;
                            samplestoread       -= readsamples;
                            totalreadsamples    += readsamples;
                            if (readsamples == samplestoloopend) {
                                pPlaybackState->loop_cycles_left--;
                                SetPos(loop.LoopStart);
                            }
                        } while (samplestoreadinloop && readsamples);

                        SetPos(reverseplaybackend); // pretend we really read backwards

                        // reverse the sample frames for backward playback
                        SwapMemoryArea(&pDst[swapareastart * this->FrameSize], (totalreadsamples - swapareastart) * this->FrameSize, this->FrameSize);
                        break;
                    }

                    default: case loop_type_normal: {
                        do {
                            // if not endless loop check if max. number of loop cycles have been passed
                            if (this->LoopPlayCount && !pPlaybackState->loop_cycles_left) break;
                            samplestoloopend  = loopEnd - GetPos();
                            readsamples       = Read(&pDst[totalreadsamples * this->FrameSize], Min(samplestoread, samplestoloopend), pExternalDecompressionBuffer);
                            samplestoread    -= readsamples;
                            totalreadsamples += readsamples;
                            if (readsamples == samplestoloopend) {
                                pPlaybackState->loop_cycles_left--;
                                SetPos(loop.LoopStart);
                            }
                        } while (samplestoread && readsamples);
                        break;
                    }
                }
            }
        }

        // read on without looping
        if (samplestoread) do {
            readsamples = Read(&pDst[totalreadsamples * this->FrameSize], samplestoread, pExternalDecompressionBuffer);
            samplestoread    -= readsamples;
            totalreadsamples += readsamples;
        } while (readsamples && samplestoread);

        // store current position
        pPlaybackState->position = GetPos();

        return totalreadsamples;
    }

    /**
     * Reads \a SampleCount number of sample points from the current
     * position into the buffer pointed by \a pBuffer and increments the
     * position within the sample. The sample wave stream will be
     * decompressed on the fly if using a compressed sample. Use this method
     * and <i>SetPos()</i> if you don't want to load the sample into RAM,
     * thus for disk streaming.
     *
     * <b>Caution:</b> If you are using more than one streaming thread, you
     * have to use an external decompression buffer for <b>EACH</b>
     * streaming thread to avoid race conditions and crashes!
     *
     * For 16 bit samples, the data in the buffer will be int16_t
     * (using native endianness). For 24 bit, the buffer will
     * contain three bytes per sample, little-endian.
     *
     * @param pBuffer      destination buffer
     * @param SampleCount  number of sample points to read
     * @param pExternalDecompressionBuffer  (optional) external buffer to use for decompression
     * @returns            number of successfully read sample points
     * @see                SetPos(), CreateDecompressionBuffer()
     */
    unsigned long Sample::Read(void* pBuffer, unsigned long SampleCount, buffer_t* pExternalDecompressionBuffer) {
        if (SampleCount == 0) return 0;
        if (!Compressed) {
            if (BitDepth == 24) {
                return pCkData->Read(pBuffer, SampleCount * FrameSize, 1) / FrameSize;
            }
            else { // 16 bit
                // (pCkData->Read does endian correction)
                return Channels == 2 ? pCkData->Read(pBuffer, SampleCount << 1, 2) >> 1
                                     : pCkData->Read(pBuffer, SampleCount, 2);
            }
        }
        else {
            if (this->SamplePos >= this->SamplesTotal) return 0;
            //TODO: efficiency: maybe we should test for an average compression rate
            unsigned long assumedsize      = GuessSize(SampleCount),
                          remainingbytes   = 0,           // remaining bytes in the local buffer
                          remainingsamples = SampleCount,
                          copysamples, skipsamples,
                          currentframeoffset = this->FrameOffset;  // offset in current sample frame since last Read()
            this->FrameOffset = 0;

            buffer_t* pDecompressionBuffer = (pExternalDecompressionBuffer) ? pExternalDecompressionBuffer : &InternalDecompressionBuffer;

            // if decompression buffer too small, then reduce amount of samples to read
            if (pDecompressionBuffer->Size < assumedsize) {
                std::cerr << "gig::Read(): WARNING - decompression buffer size too small!" << std::endl;
                SampleCount      = WorstCaseMaxSamples(pDecompressionBuffer);
                remainingsamples = SampleCount;
                assumedsize      = GuessSize(SampleCount);
            }

            unsigned char* pSrc = (unsigned char*) pDecompressionBuffer->pStart;
            int16_t* pDst = static_cast<int16_t*>(pBuffer);
            uint8_t* pDst24 = static_cast<uint8_t*>(pBuffer);
            remainingbytes = pCkData->Read(pSrc, assumedsize, 1);

            while (remainingsamples && remainingbytes) {
                unsigned long framesamples = SamplesPerFrame;
                unsigned long framebytes, rightChannelOffset = 0, nextFrameOffset;

                int mode_l = *pSrc++, mode_r = 0;

                if (Channels == 2) {
                    mode_r = *pSrc++;
                    framebytes = bytesPerFrame[mode_l] + bytesPerFrame[mode_r] + 2;
                    rightChannelOffset = bytesPerFrameNoHdr[mode_l];
                    nextFrameOffset = rightChannelOffset + bytesPerFrameNoHdr[mode_r];
                    if (remainingbytes < framebytes) { // last frame in sample
                        framesamples = SamplesInLastFrame;
                        if (mode_l == 4 && (framesamples & 1)) {
                            rightChannelOffset = ((framesamples + 1) * bitsPerSample[mode_l]) >> 3;
                        }
                        else {
                            rightChannelOffset = (framesamples * bitsPerSample[mode_l]) >> 3;
                        }
                    }
                }
                else {
                    framebytes = bytesPerFrame[mode_l] + 1;
                    nextFrameOffset = bytesPerFrameNoHdr[mode_l];
                    if (remainingbytes < framebytes) {
                        framesamples = SamplesInLastFrame;
                    }
                }

                // determine how many samples in this frame to skip and read
                if (currentframeoffset + remainingsamples >= framesamples) {
                    if (currentframeoffset <= framesamples) {
                        copysamples = framesamples - currentframeoffset;
                        skipsamples = currentframeoffset;
                    }
                    else {
                        copysamples = 0;
                        skipsamples = framesamples;
                    }
                }
                else {
                    // This frame has enough data for pBuffer, but not
                    // all of the frame is needed. Set file position
                    // to start of this frame for next call to Read.
                    copysamples = remainingsamples;
                    skipsamples = currentframeoffset;
                    pCkData->SetPos(remainingbytes, RIFF::stream_backward);
                    this->FrameOffset = currentframeoffset + copysamples;
                }
                remainingsamples -= copysamples;

                if (remainingbytes > framebytes) {
                    remainingbytes -= framebytes;
                    if (remainingsamples == 0 &&
                        currentframeoffset + copysamples == framesamples) {
                        // This frame has enough data for pBuffer, and
                        // all of the frame is needed. Set file
                        // position to start of next frame for next
                        // call to Read. FrameOffset is 0.
                        pCkData->SetPos(remainingbytes, RIFF::stream_backward);
                    }
                }
                else remainingbytes = 0;

                currentframeoffset -= skipsamples;

                if (copysamples == 0) {
                    // skip this frame
                    pSrc += framebytes - Channels;
                }
                else {
                    const unsigned char* const param_l = pSrc;
                    if (BitDepth == 24) {
                        if (mode_l != 2) pSrc += 12;

                        if (Channels == 2) { // Stereo
                            const unsigned char* const param_r = pSrc;
                            if (mode_r != 2) pSrc += 12;

                            Decompress24(mode_l, param_l, 6, pSrc, pDst24,
                                         skipsamples, copysamples, TruncatedBits);
                            Decompress24(mode_r, param_r, 6, pSrc + rightChannelOffset, pDst24 + 3,
                                         skipsamples, copysamples, TruncatedBits);
                            pDst24 += copysamples * 6;
                        }
                        else { // Mono
                            Decompress24(mode_l, param_l, 3, pSrc, pDst24,
                                         skipsamples, copysamples, TruncatedBits);
                            pDst24 += copysamples * 3;
                        }
                    }
                    else { // 16 bit
                        if (mode_l) pSrc += 4;

                        int step;
                        if (Channels == 2) { // Stereo
                            const unsigned char* const param_r = pSrc;
                            if (mode_r) pSrc += 4;

                            step = (2 - mode_l) + (2 - mode_r);
                            Decompress16(mode_l, param_l, step, 2, pSrc, pDst, skipsamples, copysamples);
                            Decompress16(mode_r, param_r, step, 2, pSrc + (2 - mode_l), pDst + 1,
                                         skipsamples, copysamples);
                            pDst += copysamples << 1;
                        }
                        else { // Mono
                            step = 2 - mode_l;
                            Decompress16(mode_l, param_l, step, 1, pSrc, pDst, skipsamples, copysamples);
                            pDst += copysamples;
                        }
                    }
                    pSrc += nextFrameOffset;
                }

                // reload from disk to local buffer if needed
                if (remainingsamples && remainingbytes < WorstCaseFrameSize && pCkData->GetState() == RIFF::stream_ready) {
                    assumedsize    = GuessSize(remainingsamples);
                    pCkData->SetPos(remainingbytes, RIFF::stream_backward);
                    if (pCkData->RemainingBytes() < assumedsize) assumedsize = pCkData->RemainingBytes();
                    remainingbytes = pCkData->Read(pDecompressionBuffer->pStart, assumedsize, 1);
                    pSrc = (unsigned char*) pDecompressionBuffer->pStart;
                }
            } // while

            this->SamplePos += (SampleCount - remainingsamples);
            if (this->SamplePos > this->SamplesTotal) this->SamplePos = this->SamplesTotal;
            return (SampleCount - remainingsamples);
        }
    }

    /** @brief Write sample wave data.
     *
     * Writes \a SampleCount number of sample points from the buffer pointed
     * by \a pBuffer and increments the position within the sample. Use this
     * method to directly write the sample data to disk, i.e. if you don't
     * want or cannot load the whole sample data into RAM.
     *
     * You have to Resize() the sample to the desired size and call
     * File::Save() <b>before</b> using Write().
     *
     * Note: there is currently no support for writing compressed samples.
     *
     * For 16 bit samples, the data in the source buffer should be
     * int16_t (using native endianness). For 24 bit, the buffer
     * should contain three bytes per sample, little-endian.
     *
     * @param pBuffer     - source buffer
     * @param SampleCount - number of sample points to write
     * @throws DLS::Exception if current sample size is too small
     * @throws gig::Exception if sample is compressed
     * @see DLS::LoadSampleData()
     */
    unsigned long Sample::Write(void* pBuffer, unsigned long SampleCount) {
        if (Compressed) throw gig::Exception("There is no support for writing compressed gig samples (yet)");

        // if this is the first write in this sample, reset the
        // checksum calculator
        if (pCkData->GetPos() == 0) {
            __resetCRC(crc);
        }
        if (GetSize() < SampleCount) throw Exception("Could not write sample data, current sample size to small");
        unsigned long res;
        if (BitDepth == 24) {
            res = pCkData->Write(pBuffer, SampleCount * FrameSize, 1) / FrameSize;
        } else { // 16 bit
            res = Channels == 2 ? pCkData->Write(pBuffer, SampleCount << 1, 2) >> 1
                                : pCkData->Write(pBuffer, SampleCount, 2);
        }
        __calculateCRC((unsigned char *)pBuffer, SampleCount * FrameSize, crc);

        // if this is the last write, update the checksum chunk in the
        // file
        if (pCkData->GetPos() == pCkData->GetSize()) {
            File* pFile = static_cast<File*>(GetParent());
            pFile->SetSampleChecksum(this, __encodeCRC(crc));
        }
        return res;
    }

    /**
     * Allocates a decompression buffer for streaming (compressed) samples
     * with Sample::Read(). If you are using more than one streaming thread
     * in your application you <b>HAVE</b> to create a decompression buffer
     * for <b>EACH</b> of your streaming threads and provide it with the
     * Sample::Read() call in order to avoid race conditions and crashes.
     *
     * You should free the memory occupied by the allocated buffer(s) once
     * you don't need one of your streaming threads anymore by calling
     * DestroyDecompressionBuffer().
     *
     * @param MaxReadSize - the maximum size (in sample points) you ever
     *                      expect to read with one Read() call
     * @returns allocated decompression buffer
     * @see DestroyDecompressionBuffer()
     */
    buffer_t Sample::CreateDecompressionBuffer(unsigned long MaxReadSize) {
        buffer_t result;
        const double worstCaseHeaderOverhead =
                (256.0 /*frame size*/ + 12.0 /*header*/ + 2.0 /*compression type flag (stereo)*/) / 256.0;
        result.Size              = (unsigned long) (double(MaxReadSize) * 3.0 /*(24 Bit)*/ * 2.0 /*stereo*/ * worstCaseHeaderOverhead);
        result.pStart            = new int8_t[result.Size];
        result.NullExtensionSize = 0;
        return result;
    }

    /**
     * Free decompression buffer, previously created with
     * CreateDecompressionBuffer().
     *
     * @param DecompressionBuffer - previously allocated decompression
     *                              buffer to free
     */
    void Sample::DestroyDecompressionBuffer(buffer_t& DecompressionBuffer) {
        if (DecompressionBuffer.Size && DecompressionBuffer.pStart) {
            delete[] (int8_t*) DecompressionBuffer.pStart;
            DecompressionBuffer.pStart = NULL;
            DecompressionBuffer.Size   = 0;
            DecompressionBuffer.NullExtensionSize = 0;
        }
    }

    /**
     * Returns pointer to the Group this Sample belongs to. In the .gig
     * format a sample always belongs to one group. If it wasn't explicitly
     * assigned to a certain group, it will be automatically assigned to a
     * default group.
     *
     * @returns Sample's Group (never NULL)
     */
    Group* Sample::GetGroup() const {
        return pGroup;
    }

    Sample::~Sample() {
        Instances--;
        if (!Instances && InternalDecompressionBuffer.Size) {
            delete[] (unsigned char*) InternalDecompressionBuffer.pStart;
            InternalDecompressionBuffer.pStart = NULL;
            InternalDecompressionBuffer.Size   = 0;
        }
        if (FrameTable) delete[] FrameTable;
        if (RAMCache.pStart) delete[] (int8_t*) RAMCache.pStart;
    }



// *************** DimensionRegion ***************
// *

    uint                               DimensionRegion::Instances       = 0;
    DimensionRegion::VelocityTableMap* DimensionRegion::pVelocityTables = NULL;

    DimensionRegion::DimensionRegion(Region* pParent, RIFF::List* _3ewl) : DLS::Sampler(_3ewl) {
        Instances++;

        pSample = NULL;
        pRegion = pParent;

        if (_3ewl->GetSubChunk(CHUNK_ID_WSMP)) memcpy(&Crossfade, &SamplerOptions, 4);
        else memset(&Crossfade, 0, 4);

        if (!pVelocityTables) pVelocityTables = new VelocityTableMap;

        RIFF::Chunk* _3ewa = _3ewl->GetSubChunk(CHUNK_ID_3EWA);
        if (_3ewa) { // if '3ewa' chunk exists
            _3ewa->ReadInt32(); // unknown, always == chunk size ?
            LFO3Frequency = (double) GIG_EXP_DECODE(_3ewa->ReadInt32());
            EG3Attack     = (double) GIG_EXP_DECODE(_3ewa->ReadInt32());
            _3ewa->ReadInt16(); // unknown
            LFO1InternalDepth = _3ewa->ReadUint16();
            _3ewa->ReadInt16(); // unknown
            LFO3InternalDepth = _3ewa->ReadInt16();
            _3ewa->ReadInt16(); // unknown
            LFO1ControlDepth = _3ewa->ReadUint16();
            _3ewa->ReadInt16(); // unknown
            LFO3ControlDepth = _3ewa->ReadInt16();
            EG1Attack           = (double) GIG_EXP_DECODE(_3ewa->ReadInt32());
            EG1Decay1           = (double) GIG_EXP_DECODE(_3ewa->ReadInt32());
            _3ewa->ReadInt16(); // unknown
            EG1Sustain          = _3ewa->ReadUint16();
            EG1Release          = (double) GIG_EXP_DECODE(_3ewa->ReadInt32());
            EG1Controller       = DecodeLeverageController(static_cast<_lev_ctrl_t>(_3ewa->ReadUint8()));
            uint8_t eg1ctrloptions        = _3ewa->ReadUint8();
            EG1ControllerInvert           = eg1ctrloptions & 0x01;
            EG1ControllerAttackInfluence  = GIG_EG_CTR_ATTACK_INFLUENCE_EXTRACT(eg1ctrloptions);
            EG1ControllerDecayInfluence   = GIG_EG_CTR_DECAY_INFLUENCE_EXTRACT(eg1ctrloptions);
            EG1ControllerReleaseInfluence = GIG_EG_CTR_RELEASE_INFLUENCE_EXTRACT(eg1ctrloptions);
            EG2Controller       = DecodeLeverageController(static_cast<_lev_ctrl_t>(_3ewa->ReadUint8()));
            uint8_t eg2ctrloptions        = _3ewa->ReadUint8();
            EG2ControllerInvert           = eg2ctrloptions & 0x01;
            EG2ControllerAttackInfluence  = GIG_EG_CTR_ATTACK_INFLUENCE_EXTRACT(eg2ctrloptions);
            EG2ControllerDecayInfluence   = GIG_EG_CTR_DECAY_INFLUENCE_EXTRACT(eg2ctrloptions);
            EG2ControllerReleaseInfluence = GIG_EG_CTR_RELEASE_INFLUENCE_EXTRACT(eg2ctrloptions);
            LFO1Frequency    = (double) GIG_EXP_DECODE(_3ewa->ReadInt32());
            EG2Attack        = (double) GIG_EXP_DECODE(_3ewa->ReadInt32());
            EG2Decay1        = (double) GIG_EXP_DECODE(_3ewa->ReadInt32());
            _3ewa->ReadInt16(); // unknown
            EG2Sustain       = _3ewa->ReadUint16();
            EG2Release       = (double) GIG_EXP_DECODE(_3ewa->ReadInt32());
            _3ewa->ReadInt16(); // unknown
            LFO2ControlDepth = _3ewa->ReadUint16();
            LFO2Frequency    = (double) GIG_EXP_DECODE(_3ewa->ReadInt32());
            _3ewa->ReadInt16(); // unknown
            LFO2InternalDepth = _3ewa->ReadUint16();
            int32_t eg1decay2 = _3ewa->ReadInt32();
            EG1Decay2          = (double) GIG_EXP_DECODE(eg1decay2);
            EG1InfiniteSustain = (eg1decay2 == 0x7fffffff);
            _3ewa->ReadInt16(); // unknown
            EG1PreAttack      = _3ewa->ReadUint16();
            int32_t eg2decay2 = _3ewa->ReadInt32();
            EG2Decay2         = (double) GIG_EXP_DECODE(eg2decay2);
            EG2InfiniteSustain = (eg2decay2 == 0x7fffffff);
            _3ewa->ReadInt16(); // unknown
            EG2PreAttack      = _3ewa->ReadUint16();
            uint8_t velocityresponse = _3ewa->ReadUint8();
            if (velocityresponse < 5) {
                VelocityResponseCurve = curve_type_nonlinear;
                VelocityResponseDepth = velocityresponse;
            } else if (velocityresponse < 10) {
                VelocityResponseCurve = curve_type_linear;
                VelocityResponseDepth = velocityresponse - 5;
            } else if (velocityresponse < 15) {
                VelocityResponseCurve = curve_type_special;
                VelocityResponseDepth = velocityresponse - 10;
            } else {
                VelocityResponseCurve = curve_type_unknown;
                VelocityResponseDepth = 0;
            }
            uint8_t releasevelocityresponse = _3ewa->ReadUint8();
            if (releasevelocityresponse < 5) {
                ReleaseVelocityResponseCurve = curve_type_nonlinear;
                ReleaseVelocityResponseDepth = releasevelocityresponse;
            } else if (releasevelocityresponse < 10) {
                ReleaseVelocityResponseCurve = curve_type_linear;
                ReleaseVelocityResponseDepth = releasevelocityresponse - 5;
            } else if (releasevelocityresponse < 15) {
                ReleaseVelocityResponseCurve = curve_type_special;
                ReleaseVelocityResponseDepth = releasevelocityresponse - 10;
            } else {
                ReleaseVelocityResponseCurve = curve_type_unknown;
                ReleaseVelocityResponseDepth = 0;
            }
            VelocityResponseCurveScaling = _3ewa->ReadUint8();
            AttenuationControllerThreshold = _3ewa->ReadInt8();
            _3ewa->ReadInt32(); // unknown
            SampleStartOffset = (uint16_t) _3ewa->ReadInt16();
            _3ewa->ReadInt16(); // unknown
            uint8_t pitchTrackDimensionBypass = _3ewa->ReadInt8();
            PitchTrack = GIG_PITCH_TRACK_EXTRACT(pitchTrackDimensionBypass);
            if      (pitchTrackDimensionBypass & 0x10) DimensionBypass = dim_bypass_ctrl_94;
            else if (pitchTrackDimensionBypass & 0x20) DimensionBypass = dim_bypass_ctrl_95;
            else                                       DimensionBypass = dim_bypass_ctrl_none;
            uint8_t pan = _3ewa->ReadUint8();
            Pan         = (pan < 64) ? pan : -((int)pan - 63); // signed 7 bit -> signed 8 bit
            SelfMask = _3ewa->ReadInt8() & 0x01;
            _3ewa->ReadInt8(); // unknown
            uint8_t lfo3ctrl = _3ewa->ReadUint8();
            LFO3Controller           = static_cast<lfo3_ctrl_t>(lfo3ctrl & 0x07); // lower 3 bits
            LFO3Sync                 = lfo3ctrl & 0x20; // bit 5
            InvertAttenuationController = lfo3ctrl & 0x80; // bit 7
            AttenuationController  = DecodeLeverageController(static_cast<_lev_ctrl_t>(_3ewa->ReadUint8()));
            uint8_t lfo2ctrl       = _3ewa->ReadUint8();
            LFO2Controller         = static_cast<lfo2_ctrl_t>(lfo2ctrl & 0x07); // lower 3 bits
            LFO2FlipPhase          = lfo2ctrl & 0x80; // bit 7
            LFO2Sync               = lfo2ctrl & 0x20; // bit 5
            bool extResonanceCtrl  = lfo2ctrl & 0x40; // bit 6
            uint8_t lfo1ctrl       = _3ewa->ReadUint8();
            LFO1Controller         = static_cast<lfo1_ctrl_t>(lfo1ctrl & 0x07); // lower 3 bits
            LFO1FlipPhase          = lfo1ctrl & 0x80; // bit 7
            LFO1Sync               = lfo1ctrl & 0x40; // bit 6
            VCFResonanceController = (extResonanceCtrl) ? static_cast<vcf_res_ctrl_t>(GIG_VCF_RESONANCE_CTRL_EXTRACT(lfo1ctrl))
                                                        : vcf_res_ctrl_none;
            uint16_t eg3depth = _3ewa->ReadUint16();
            EG3Depth = (eg3depth <= 1200) ? eg3depth /* positives */
                                        : (-1) * (int16_t) ((eg3depth ^ 0xffff) + 1); /* binary complementary for negatives */
            _3ewa->ReadInt16(); // unknown
            ChannelOffset = _3ewa->ReadUint8() / 4;
            uint8_t regoptions = _3ewa->ReadUint8();
            MSDecode           = regoptions & 0x01; // bit 0
            SustainDefeat      = regoptions & 0x02; // bit 1
            _3ewa->ReadInt16(); // unknown
            VelocityUpperLimit = _3ewa->ReadInt8();
            _3ewa->ReadInt8(); // unknown
            _3ewa->ReadInt16(); // unknown
            ReleaseTriggerDecay = _3ewa->ReadUint8(); // release trigger decay
            _3ewa->ReadInt8(); // unknown
            _3ewa->ReadInt8(); // unknown
            EG1Hold = _3ewa->ReadUint8() & 0x80; // bit 7
            uint8_t vcfcutoff = _3ewa->ReadUint8();
            VCFEnabled = vcfcutoff & 0x80; // bit 7
            VCFCutoff  = vcfcutoff & 0x7f; // lower 7 bits
            VCFCutoffController = static_cast<vcf_cutoff_ctrl_t>(_3ewa->ReadUint8());
            uint8_t vcfvelscale = _3ewa->ReadUint8();
            VCFCutoffControllerInvert = vcfvelscale & 0x80; // bit 7
            VCFVelocityScale = vcfvelscale & 0x7f; // lower 7 bits
            _3ewa->ReadInt8(); // unknown
            uint8_t vcfresonance = _3ewa->ReadUint8();
            VCFResonance = vcfresonance & 0x7f; // lower 7 bits
            VCFResonanceDynamic = !(vcfresonance & 0x80); // bit 7
            uint8_t vcfbreakpoint         = _3ewa->ReadUint8();
            VCFKeyboardTracking           = vcfbreakpoint & 0x80; // bit 7
            VCFKeyboardTrackingBreakpoint = vcfbreakpoint & 0x7f; // lower 7 bits
            uint8_t vcfvelocity = _3ewa->ReadUint8();
            VCFVelocityDynamicRange = vcfvelocity % 5;
            VCFVelocityCurve        = static_cast<curve_type_t>(vcfvelocity / 5);
            VCFType = static_cast<vcf_type_t>(_3ewa->ReadUint8());
            if (VCFType == vcf_type_lowpass) {
                if (lfo3ctrl & 0x40) // bit 6
                    VCFType = vcf_type_lowpassturbo;
            }
            if (_3ewa->RemainingBytes() >= 8) {
                _3ewa->Read(DimensionUpperLimits, 1, 8);
            } else {
                memset(DimensionUpperLimits, 0, 8);
            }
        } else { // '3ewa' chunk does not exist yet
            // use default values
            LFO3Frequency                   = 1.0;
            EG3Attack                       = 0.0;
            LFO1InternalDepth               = 0;
            LFO3InternalDepth               = 0;
            LFO1ControlDepth                = 0;
            LFO3ControlDepth                = 0;
            EG1Attack                       = 0.0;
            EG1Decay1                       = 0.005;
            EG1Sustain                      = 1000;
            EG1Release                      = 0.3;
            EG1Controller.type              = eg1_ctrl_t::type_none;
            EG1Controller.controller_number = 0;
            EG1ControllerInvert             = false;
            EG1ControllerAttackInfluence    = 0;
            EG1ControllerDecayInfluence     = 0;
            EG1ControllerReleaseInfluence   = 0;
            EG2Controller.type              = eg2_ctrl_t::type_none;
            EG2Controller.controller_number = 0;
            EG2ControllerInvert             = false;
            EG2ControllerAttackInfluence    = 0;
            EG2ControllerDecayInfluence     = 0;
            EG2ControllerReleaseInfluence   = 0;
            LFO1Frequency                   = 1.0;
            EG2Attack                       = 0.0;
            EG2Decay1                       = 0.005;
            EG2Sustain                      = 1000;
            EG2Release                      = 0.3;
            LFO2ControlDepth                = 0;
            LFO2Frequency                   = 1.0;
            LFO2InternalDepth               = 0;
            EG1Decay2                       = 0.0;
            EG1InfiniteSustain              = true;
            EG1PreAttack                    = 0;
            EG2Decay2                       = 0.0;
            EG2InfiniteSustain              = true;
            EG2PreAttack                    = 0;
            VelocityResponseCurve           = curve_type_nonlinear;
            VelocityResponseDepth           = 3;
            ReleaseVelocityResponseCurve    = curve_type_nonlinear;
            ReleaseVelocityResponseDepth    = 3;
            VelocityResponseCurveScaling    = 32;
            AttenuationControllerThreshold  = 0;
            SampleStartOffset               = 0;
            PitchTrack                      = true;
            DimensionBypass                 = dim_bypass_ctrl_none;
            Pan                             = 0;
            SelfMask                        = true;
            LFO3Controller                  = lfo3_ctrl_modwheel;
            LFO3Sync                        = false;
            InvertAttenuationController     = false;
            AttenuationController.type      = attenuation_ctrl_t::type_none;
            AttenuationController.controller_number = 0;
            LFO2Controller                  = lfo2_ctrl_internal;
            LFO2FlipPhase                   = false;
            LFO2Sync                        = false;
            LFO1Controller                  = lfo1_ctrl_internal;
            LFO1FlipPhase                   = false;
            LFO1Sync                        = false;
            VCFResonanceController          = vcf_res_ctrl_none;
            EG3Depth                        = 0;
            ChannelOffset                   = 0;
            MSDecode                        = false;
            SustainDefeat                   = false;
            VelocityUpperLimit              = 0;
            ReleaseTriggerDecay             = 0;
            EG1Hold                         = false;
            VCFEnabled                      = false;
            VCFCutoff                       = 0;
            VCFCutoffController             = vcf_cutoff_ctrl_none;
            VCFCutoffControllerInvert       = false;
            VCFVelocityScale                = 0;
            VCFResonance                    = 0;
            VCFResonanceDynamic             = false;
            VCFKeyboardTracking             = false;
            VCFKeyboardTrackingBreakpoint   = 0;
            VCFVelocityDynamicRange         = 0x04;
            VCFVelocityCurve                = curve_type_linear;
            VCFType                         = vcf_type_lowpass;
            memset(DimensionUpperLimits, 127, 8);
        }

        pVelocityAttenuationTable = GetVelocityTable(VelocityResponseCurve,
                                                     VelocityResponseDepth,
                                                     VelocityResponseCurveScaling);

        pVelocityReleaseTable = GetReleaseVelocityTable(
                                    ReleaseVelocityResponseCurve,
                                    ReleaseVelocityResponseDepth
                                );

        pVelocityCutoffTable = GetCutoffVelocityTable(VCFVelocityCurve,
                                                      VCFVelocityDynamicRange,
                                                      VCFVelocityScale,
                                                      VCFCutoffController);

        SampleAttenuation = pow(10.0, -Gain / (20.0 * 655360));
        VelocityTable = 0;
    }

    /*
     * Constructs a DimensionRegion by copying all parameters from
     * another DimensionRegion
     */
    DimensionRegion::DimensionRegion(RIFF::List* _3ewl, const DimensionRegion& src) : DLS::Sampler(_3ewl) {
        Instances++;
        *this = src; // default memberwise shallow copy of all parameters
        pParentList = _3ewl; // restore the chunk pointer

        // deep copy of owned structures
        if (src.VelocityTable) {
            VelocityTable = new uint8_t[128];
            for (int k = 0 ; k < 128 ; k++)
                VelocityTable[k] = src.VelocityTable[k];
        }
        if (src.pSampleLoops) {
            pSampleLoops = new DLS::sample_loop_t[src.SampleLoops];
            for (int k = 0 ; k < src.SampleLoops ; k++)
                pSampleLoops[k] = src.pSampleLoops[k];
        }
    }

    /**
     * Updates the respective member variable and updates @c SampleAttenuation
     * which depends on this value.
     */
    void DimensionRegion::SetGain(int32_t gain) {
        DLS::Sampler::SetGain(gain);
        SampleAttenuation = pow(10.0, -Gain / (20.0 * 655360));
    }

    /**
     * Apply dimension region settings to the respective RIFF chunks. You
     * have to call File::Save() to make changes persistent.
     *
     * Usually there is absolutely no need to call this method explicitly.
     * It will be called automatically when File::Save() was called.
     */
    void DimensionRegion::UpdateChunks() {
        // first update base class's chunk
        DLS::Sampler::UpdateChunks();

        RIFF::Chunk* wsmp = pParentList->GetSubChunk(CHUNK_ID_WSMP);
        uint8_t* pData = (uint8_t*) wsmp->LoadChunkData();
        pData[12] = Crossfade.in_start;
        pData[13] = Crossfade.in_end;
        pData[14] = Crossfade.out_start;
        pData[15] = Crossfade.out_end;

        // make sure '3ewa' chunk exists
        RIFF::Chunk* _3ewa = pParentList->GetSubChunk(CHUNK_ID_3EWA);
        if (!_3ewa) {
            File* pFile = (File*) GetParent()->GetParent()->GetParent();
            bool version3 = pFile->pVersion && pFile->pVersion->major == 3;
            _3ewa = pParentList->AddSubChunk(CHUNK_ID_3EWA, version3 ? 148 : 140);
        }
        pData = (uint8_t*) _3ewa->LoadChunkData();

        // update '3ewa' chunk with DimensionRegion's current settings

        const uint32_t chunksize = _3ewa->GetNewSize();
        store32(&pData[0], chunksize); // unknown, always chunk size?

        const int32_t lfo3freq = (int32_t) GIG_EXP_ENCODE(LFO3Frequency);
        store32(&pData[4], lfo3freq);

        const int32_t eg3attack = (int32_t) GIG_EXP_ENCODE(EG3Attack);
        store32(&pData[8], eg3attack);

        // next 2 bytes unknown

        store16(&pData[14], LFO1InternalDepth);

        // next 2 bytes unknown

        store16(&pData[18], LFO3InternalDepth);

        // next 2 bytes unknown

        store16(&pData[22], LFO1ControlDepth);

        // next 2 bytes unknown

        store16(&pData[26], LFO3ControlDepth);

        const int32_t eg1attack = (int32_t) GIG_EXP_ENCODE(EG1Attack);
        store32(&pData[28], eg1attack);

        const int32_t eg1decay1 = (int32_t) GIG_EXP_ENCODE(EG1Decay1);
        store32(&pData[32], eg1decay1);

        // next 2 bytes unknown

        store16(&pData[38], EG1Sustain);

        const int32_t eg1release = (int32_t) GIG_EXP_ENCODE(EG1Release);
        store32(&pData[40], eg1release);

        const uint8_t eg1ctl = (uint8_t) EncodeLeverageController(EG1Controller);
        pData[44] = eg1ctl;

        const uint8_t eg1ctrloptions =
            (EG1ControllerInvert ? 0x01 : 0x00) |
            GIG_EG_CTR_ATTACK_INFLUENCE_ENCODE(EG1ControllerAttackInfluence) |
            GIG_EG_CTR_DECAY_INFLUENCE_ENCODE(EG1ControllerDecayInfluence) |
            GIG_EG_CTR_RELEASE_INFLUENCE_ENCODE(EG1ControllerReleaseInfluence);
        pData[45] = eg1ctrloptions;

        const uint8_t eg2ctl = (uint8_t) EncodeLeverageController(EG2Controller);
        pData[46] = eg2ctl;

        const uint8_t eg2ctrloptions =
            (EG2ControllerInvert ? 0x01 : 0x00) |
            GIG_EG_CTR_ATTACK_INFLUENCE_ENCODE(EG2ControllerAttackInfluence) |
            GIG_EG_CTR_DECAY_INFLUENCE_ENCODE(EG2ControllerDecayInfluence) |
            GIG_EG_CTR_RELEASE_INFLUENCE_ENCODE(EG2ControllerReleaseInfluence);
        pData[47] = eg2ctrloptions;

        const int32_t lfo1freq = (int32_t) GIG_EXP_ENCODE(LFO1Frequency);
        store32(&pData[48], lfo1freq);

        const int32_t eg2attack = (int32_t) GIG_EXP_ENCODE(EG2Attack);
        store32(&pData[52], eg2attack);

        const int32_t eg2decay1 = (int32_t) GIG_EXP_ENCODE(EG2Decay1);
        store32(&pData[56], eg2decay1);

        // next 2 bytes unknown

        store16(&pData[62], EG2Sustain);

        const int32_t eg2release = (int32_t) GIG_EXP_ENCODE(EG2Release);
        store32(&pData[64], eg2release);

        // next 2 bytes unknown

        store16(&pData[70], LFO2ControlDepth);

        const int32_t lfo2freq = (int32_t) GIG_EXP_ENCODE(LFO2Frequency);
        store32(&pData[72], lfo2freq);

        // next 2 bytes unknown

        store16(&pData[78], LFO2InternalDepth);

        const int32_t eg1decay2 = (int32_t) (EG1InfiniteSustain) ? 0x7fffffff : (int32_t) GIG_EXP_ENCODE(EG1Decay2);
        store32(&pData[80], eg1decay2);

        // next 2 bytes unknown

        store16(&pData[86], EG1PreAttack);

        const int32_t eg2decay2 = (int32_t) (EG2InfiniteSustain) ? 0x7fffffff : (int32_t) GIG_EXP_ENCODE(EG2Decay2);
        store32(&pData[88], eg2decay2);

        // next 2 bytes unknown

        store16(&pData[94], EG2PreAttack);

        {
            if (VelocityResponseDepth > 4) throw Exception("VelocityResponseDepth must be between 0 and 4");
            uint8_t velocityresponse = VelocityResponseDepth;
            switch (VelocityResponseCurve) {
                case curve_type_nonlinear:
                    break;
                case curve_type_linear:
                    velocityresponse += 5;
                    break;
                case curve_type_special:
                    velocityresponse += 10;
                    break;
                case curve_type_unknown:
                default:
                    throw Exception("Could not update DimensionRegion's chunk, unknown VelocityResponseCurve selected");
            }
            pData[96] = velocityresponse;
        }

        {
            if (ReleaseVelocityResponseDepth > 4) throw Exception("ReleaseVelocityResponseDepth must be between 0 and 4");
            uint8_t releasevelocityresponse = ReleaseVelocityResponseDepth;
            switch (ReleaseVelocityResponseCurve) {
                case curve_type_nonlinear:
                    break;
                case curve_type_linear:
                    releasevelocityresponse += 5;
                    break;
                case curve_type_special:
                    releasevelocityresponse += 10;
                    break;
                case curve_type_unknown:
                default:
                    throw Exception("Could not update DimensionRegion's chunk, unknown ReleaseVelocityResponseCurve selected");
            }
            pData[97] = releasevelocityresponse;
        }

        pData[98] = VelocityResponseCurveScaling;

        pData[99] = AttenuationControllerThreshold;

        // next 4 bytes unknown

        store16(&pData[104], SampleStartOffset);

        // next 2 bytes unknown

        {
            uint8_t pitchTrackDimensionBypass = GIG_PITCH_TRACK_ENCODE(PitchTrack);
            switch (DimensionBypass) {
                case dim_bypass_ctrl_94:
                    pitchTrackDimensionBypass |= 0x10;
                    break;
                case dim_bypass_ctrl_95:
                    pitchTrackDimensionBypass |= 0x20;
                    break;
                case dim_bypass_ctrl_none:
                    //FIXME: should we set anything here?
                    break;
                default:
                    throw Exception("Could not update DimensionRegion's chunk, unknown DimensionBypass selected");
            }
            pData[108] = pitchTrackDimensionBypass;
        }

        const uint8_t pan = (Pan >= 0) ? Pan : ((-Pan) + 63); // signed 8 bit -> signed 7 bit
        pData[109] = pan;

        const uint8_t selfmask = (SelfMask) ? 0x01 : 0x00;
        pData[110] = selfmask;

        // next byte unknown

        {
            uint8_t lfo3ctrl = LFO3Controller & 0x07; // lower 3 bits
            if (LFO3Sync) lfo3ctrl |= 0x20; // bit 5
            if (InvertAttenuationController) lfo3ctrl |= 0x80; // bit 7
            if (VCFType == vcf_type_lowpassturbo) lfo3ctrl |= 0x40; // bit 6
            pData[112] = lfo3ctrl;
        }

        const uint8_t attenctl = EncodeLeverageController(AttenuationController);
        pData[113] = attenctl;

        {
            uint8_t lfo2ctrl = LFO2Controller & 0x07; // lower 3 bits
            if (LFO2FlipPhase) lfo2ctrl |= 0x80; // bit 7
            if (LFO2Sync)      lfo2ctrl |= 0x20; // bit 5
            if (VCFResonanceController != vcf_res_ctrl_none) lfo2ctrl |= 0x40; // bit 6
            pData[114] = lfo2ctrl;
        }

        {
            uint8_t lfo1ctrl = LFO1Controller & 0x07; // lower 3 bits
            if (LFO1FlipPhase) lfo1ctrl |= 0x80; // bit 7
            if (LFO1Sync)      lfo1ctrl |= 0x40; // bit 6
            if (VCFResonanceController != vcf_res_ctrl_none)
                lfo1ctrl |= GIG_VCF_RESONANCE_CTRL_ENCODE(VCFResonanceController);
            pData[115] = lfo1ctrl;
        }

        const uint16_t eg3depth = (EG3Depth >= 0) ? EG3Depth
                                                  : uint16_t(((-EG3Depth) - 1) ^ 0xffff); /* binary complementary for negatives */
        store16(&pData[116], eg3depth);

        // next 2 bytes unknown

        const uint8_t channeloffset = ChannelOffset * 4;
        pData[120] = channeloffset;

        {
            uint8_t regoptions = 0;
            if (MSDecode)      regoptions |= 0x01; // bit 0
            if (SustainDefeat) regoptions |= 0x02; // bit 1
            pData[121] = regoptions;
        }

        // next 2 bytes unknown

        pData[124] = VelocityUpperLimit;

        // next 3 bytes unknown

        pData[128] = ReleaseTriggerDecay;

        // next 2 bytes unknown

        const uint8_t eg1hold = (EG1Hold) ? 0x80 : 0x00; // bit 7
        pData[131] = eg1hold;

        const uint8_t vcfcutoff = (VCFEnabled ? 0x80 : 0x00) |  /* bit 7 */
                                  (VCFCutoff & 0x7f);   /* lower 7 bits */
        pData[132] = vcfcutoff;

        pData[133] = VCFCutoffController;

        const uint8_t vcfvelscale = (VCFCutoffControllerInvert ? 0x80 : 0x00) | /* bit 7 */
                                    (VCFVelocityScale & 0x7f); /* lower 7 bits */
        pData[134] = vcfvelscale;

        // next byte unknown

        const uint8_t vcfresonance = (VCFResonanceDynamic ? 0x00 : 0x80) | /* bit 7 */
                                     (VCFResonance & 0x7f); /* lower 7 bits */
        pData[136] = vcfresonance;

        const uint8_t vcfbreakpoint = (VCFKeyboardTracking ? 0x80 : 0x00) | /* bit 7 */
                                      (VCFKeyboardTrackingBreakpoint & 0x7f); /* lower 7 bits */
        pData[137] = vcfbreakpoint;

        const uint8_t vcfvelocity = VCFVelocityDynamicRange % 5 +
                                    VCFVelocityCurve * 5;
        pData[138] = vcfvelocity;

        const uint8_t vcftype = (VCFType == vcf_type_lowpassturbo) ? vcf_type_lowpass : VCFType;
        pData[139] = vcftype;

        if (chunksize >= 148) {
            memcpy(&pData[140], DimensionUpperLimits, 8);
        }
    }

    double* DimensionRegion::GetReleaseVelocityTable(curve_type_t releaseVelocityResponseCurve, uint8_t releaseVelocityResponseDepth) {
        curve_type_t curveType = releaseVelocityResponseCurve;
        uint8_t depth = releaseVelocityResponseDepth;
        // this models a strange behaviour or bug in GSt: two of the
        // velocity response curves for release time are not used even
        // if specified, instead another curve is chosen.
        if ((curveType == curve_type_nonlinear && depth == 0) ||
            (curveType == curve_type_special   && depth == 4)) {
            curveType = curve_type_nonlinear;
            depth = 3;
        }
        return GetVelocityTable(curveType, depth, 0);
    }

    double* DimensionRegion::GetCutoffVelocityTable(curve_type_t vcfVelocityCurve,
                                                    uint8_t vcfVelocityDynamicRange,
                                                    uint8_t vcfVelocityScale,
                                                    vcf_cutoff_ctrl_t vcfCutoffController)
    {
        curve_type_t curveType = vcfVelocityCurve;
        uint8_t depth = vcfVelocityDynamicRange;
        // even stranger GSt: two of the velocity response curves for
        // filter cutoff are not used, instead another special curve
        // is chosen. This curve is not used anywhere else.
        if ((curveType == curve_type_nonlinear && depth == 0) ||
            (curveType == curve_type_special   && depth == 4)) {
            curveType = curve_type_special;
            depth = 5;
        }
        return GetVelocityTable(curveType, depth,
                                (vcfCutoffController <= vcf_cutoff_ctrl_none2)
                                    ? vcfVelocityScale : 0);
    }

    // get the corresponding velocity table from the table map or create & calculate that table if it doesn't exist yet
    double* DimensionRegion::GetVelocityTable(curve_type_t curveType, uint8_t depth, uint8_t scaling)
    {
        double* table;
        uint32_t tableKey = (curveType<<16) | (depth<<8) | scaling;
        if (pVelocityTables->count(tableKey)) { // if key exists
            table = (*pVelocityTables)[tableKey];
        }
        else {
            table = CreateVelocityTable(curveType, depth, scaling);
            (*pVelocityTables)[tableKey] = table; // put the new table into the tables map
        }
        return table;
    }

    Region* DimensionRegion::GetParent() const {
        return pRegion;
    }

    leverage_ctrl_t DimensionRegion::DecodeLeverageController(_lev_ctrl_t EncodedController) {
        leverage_ctrl_t decodedcontroller;
        switch (EncodedController) {
            // special controller
            case _lev_ctrl_none:
                decodedcontroller.type = leverage_ctrl_t::type_none;
                decodedcontroller.controller_number = 0;
                break;
            case _lev_ctrl_velocity:
                decodedcontroller.type = leverage_ctrl_t::type_velocity;
                decodedcontroller.controller_number = 0;
                break;
            case _lev_ctrl_channelaftertouch:
                decodedcontroller.type = leverage_ctrl_t::type_channelaftertouch;
                decodedcontroller.controller_number = 0;
                break;

            // ordinary MIDI control change controller
            case _lev_ctrl_modwheel:
                decodedcontroller.type = leverage_ctrl_t::type_controlchange;
                decodedcontroller.controller_number = 1;
                break;
            case _lev_ctrl_breath:
                decodedcontroller.type = leverage_ctrl_t::type_controlchange;
                decodedcontroller.controller_number = 2;
                break;
            case _lev_ctrl_foot:
                decodedcontroller.type = leverage_ctrl_t::type_controlchange;
                decodedcontroller.controller_number = 4;
                break;
            case _lev_ctrl_effect1:
                decodedcontroller.type = leverage_ctrl_t::type_controlchange;
                decodedcontroller.controller_number = 12;
                break;
            case _lev_ctrl_effect2:
                decodedcontroller.type = leverage_ctrl_t::type_controlchange;
                decodedcontroller.controller_number = 13;
                break;
            case _lev_ctrl_genpurpose1:
                decodedcontroller.type = leverage_ctrl_t::type_controlchange;
                decodedcontroller.controller_number = 16;
                break;
            case _lev_ctrl_genpurpose2:
                decodedcontroller.type = leverage_ctrl_t::type_controlchange;
                decodedcontroller.controller_number = 17;
                break;
            case _lev_ctrl_genpurpose3:
                decodedcontroller.type = leverage_ctrl_t::type_controlchange;
                decodedcontroller.controller_number = 18;
                break;
            case _lev_ctrl_genpurpose4:
                decodedcontroller.type = leverage_ctrl_t::type_controlchange;
                decodedcontroller.controller_number = 19;
                break;
            case _lev_ctrl_portamentotime:
                decodedcontroller.type = leverage_ctrl_t::type_controlchange;
                decodedcontroller.controller_number = 5;
                break;
            case _lev_ctrl_sustainpedal:
                decodedcontroller.type = leverage_ctrl_t::type_controlchange;
                decodedcontroller.controller_number = 64;
                break;
            case _lev_ctrl_portamento:
                decodedcontroller.type = leverage_ctrl_t::type_controlchange;
                decodedcontroller.controller_number = 65;
                break;
            case _lev_ctrl_sostenutopedal:
                decodedcontroller.type = leverage_ctrl_t::type_controlchange;
                decodedcontroller.controller_number = 66;
                break;
            case _lev_ctrl_softpedal:
                decodedcontroller.type = leverage_ctrl_t::type_controlchange;
                decodedcontroller.controller_number = 67;
                break;
            case _lev_ctrl_genpurpose5:
                decodedcontroller.type = leverage_ctrl_t::type_controlchange;
                decodedcontroller.controller_number = 80;
                break;
            case _lev_ctrl_genpurpose6:
                decodedcontroller.type = leverage_ctrl_t::type_controlchange;
                decodedcontroller.controller_number = 81;
                break;
            case _lev_ctrl_genpurpose7:
                decodedcontroller.type = leverage_ctrl_t::type_controlchange;
                decodedcontroller.controller_number = 82;
                break;
            case _lev_ctrl_genpurpose8:
                decodedcontroller.type = leverage_ctrl_t::type_controlchange;
                decodedcontroller.controller_number = 83;
                break;
            case _lev_ctrl_effect1depth:
                decodedcontroller.type = leverage_ctrl_t::type_controlchange;
                decodedcontroller.controller_number = 91;
                break;
            case _lev_ctrl_effect2depth:
                decodedcontroller.type = leverage_ctrl_t::type_controlchange;
                decodedcontroller.controller_number = 92;
                break;
            case _lev_ctrl_effect3depth:
                decodedcontroller.type = leverage_ctrl_t::type_controlchange;
                decodedcontroller.controller_number = 93;
                break;
            case _lev_ctrl_effect4depth:
                decodedcontroller.type = leverage_ctrl_t::type_controlchange;
                decodedcontroller.controller_number = 94;
                break;
            case _lev_ctrl_effect5depth:
                decodedcontroller.type = leverage_ctrl_t::type_controlchange;
                decodedcontroller.controller_number = 95;
                break;

            // unknown controller type
            default:
                throw gig::Exception("Unknown leverage controller type.");
        }
        return decodedcontroller;
    }

    DimensionRegion::_lev_ctrl_t DimensionRegion::EncodeLeverageController(leverage_ctrl_t DecodedController) {
        _lev_ctrl_t encodedcontroller;
        switch (DecodedController.type) {
            // special controller
            case leverage_ctrl_t::type_none:
                encodedcontroller = _lev_ctrl_none;
                break;
            case leverage_ctrl_t::type_velocity:
                encodedcontroller = _lev_ctrl_velocity;
                break;
            case leverage_ctrl_t::type_channelaftertouch:
                encodedcontroller = _lev_ctrl_channelaftertouch;
                break;

            // ordinary MIDI control change controller
            case leverage_ctrl_t::type_controlchange:
                switch (DecodedController.controller_number) {
                    case 1:
                        encodedcontroller = _lev_ctrl_modwheel;
                        break;
                    case 2:
                        encodedcontroller = _lev_ctrl_breath;
                        break;
                    case 4:
                        encodedcontroller = _lev_ctrl_foot;
                        break;
                    case 12:
                        encodedcontroller = _lev_ctrl_effect1;
                        break;
                    case 13:
                        encodedcontroller = _lev_ctrl_effect2;
                        break;
                    case 16:
                        encodedcontroller = _lev_ctrl_genpurpose1;
                        break;
                    case 17:
                        encodedcontroller = _lev_ctrl_genpurpose2;
                        break;
                    case 18:
                        encodedcontroller = _lev_ctrl_genpurpose3;
                        break;
                    case 19:
                        encodedcontroller = _lev_ctrl_genpurpose4;
                        break;
                    case 5:
                        encodedcontroller = _lev_ctrl_portamentotime;
                        break;
                    case 64:
                        encodedcontroller = _lev_ctrl_sustainpedal;
                        break;
                    case 65:
                        encodedcontroller = _lev_ctrl_portamento;
                        break;
                    case 66:
                        encodedcontroller = _lev_ctrl_sostenutopedal;
                        break;
                    case 67:
                        encodedcontroller = _lev_ctrl_softpedal;
                        break;
                    case 80:
                        encodedcontroller = _lev_ctrl_genpurpose5;
                        break;
                    case 81:
                        encodedcontroller = _lev_ctrl_genpurpose6;
                        break;
                    case 82:
                        encodedcontroller = _lev_ctrl_genpurpose7;
                        break;
                    case 83:
                        encodedcontroller = _lev_ctrl_genpurpose8;
                        break;
                    case 91:
                        encodedcontroller = _lev_ctrl_effect1depth;
                        break;
                    case 92:
                        encodedcontroller = _lev_ctrl_effect2depth;
                        break;
                    case 93:
                        encodedcontroller = _lev_ctrl_effect3depth;
                        break;
                    case 94:
                        encodedcontroller = _lev_ctrl_effect4depth;
                        break;
                    case 95:
                        encodedcontroller = _lev_ctrl_effect5depth;
                        break;
                    default:
                        throw gig::Exception("leverage controller number is not supported by the gig format");
                }
                break;
            default:
                throw gig::Exception("Unknown leverage controller type.");
        }
        return encodedcontroller;
    }

    DimensionRegion::~DimensionRegion() {
        Instances--;
        if (!Instances) {
            // delete the velocity->volume tables
            VelocityTableMap::iterator iter;
            for (iter = pVelocityTables->begin(); iter != pVelocityTables->end(); iter++) {
                double* pTable = iter->second;
                if (pTable) delete[] pTable;
            }
            pVelocityTables->clear();
            delete pVelocityTables;
            pVelocityTables = NULL;
        }
        if (VelocityTable) delete[] VelocityTable;
    }

    /**
     * Returns the correct amplitude factor for the given \a MIDIKeyVelocity.
     * All involved parameters (VelocityResponseCurve, VelocityResponseDepth
     * and VelocityResponseCurveScaling) involved are taken into account to
     * calculate the amplitude factor. Use this method when a key was
     * triggered to get the volume with which the sample should be played
     * back.
     *
     * @param MIDIKeyVelocity  MIDI velocity value of the triggered key (between 0 and 127)
     * @returns                amplitude factor (between 0.0 and 1.0)
     */
    double DimensionRegion::GetVelocityAttenuation(uint8_t MIDIKeyVelocity) {
        return pVelocityAttenuationTable[MIDIKeyVelocity];
    }

    double DimensionRegion::GetVelocityRelease(uint8_t MIDIKeyVelocity) {
        return pVelocityReleaseTable[MIDIKeyVelocity];
    }

    double DimensionRegion::GetVelocityCutoff(uint8_t MIDIKeyVelocity) {
        return pVelocityCutoffTable[MIDIKeyVelocity];
    }

    /**
     * Updates the respective member variable and the lookup table / cache
     * that depends on this value.
     */
    void DimensionRegion::SetVelocityResponseCurve(curve_type_t curve) {
        pVelocityAttenuationTable =
            GetVelocityTable(
                curve, VelocityResponseDepth, VelocityResponseCurveScaling
            );
        VelocityResponseCurve = curve;
    }

    /**
     * Updates the respective member variable and the lookup table / cache
     * that depends on this value.
     */
    void DimensionRegion::SetVelocityResponseDepth(uint8_t depth) {
        pVelocityAttenuationTable =
            GetVelocityTable(
                VelocityResponseCurve, depth, VelocityResponseCurveScaling
            );
        VelocityResponseDepth = depth;
    }

    /**
     * Updates the respective member variable and the lookup table / cache
     * that depends on this value.
     */
    void DimensionRegion::SetVelocityResponseCurveScaling(uint8_t scaling) {
        pVelocityAttenuationTable =
            GetVelocityTable(
                VelocityResponseCurve, VelocityResponseDepth, scaling
            );
        VelocityResponseCurveScaling = scaling;
    }

    /**
     * Updates the respective member variable and the lookup table / cache
     * that depends on this value.
     */
    void DimensionRegion::SetReleaseVelocityResponseCurve(curve_type_t curve) {
        pVelocityReleaseTable = GetReleaseVelocityTable(curve, ReleaseVelocityResponseDepth);
        ReleaseVelocityResponseCurve = curve;
    }

    /**
     * Updates the respective member variable and the lookup table / cache
     * that depends on this value.
     */
    void DimensionRegion::SetReleaseVelocityResponseDepth(uint8_t depth) {
        pVelocityReleaseTable = GetReleaseVelocityTable(ReleaseVelocityResponseCurve, depth);
        ReleaseVelocityResponseDepth = depth;
    }

    /**
     * Updates the respective member variable and the lookup table / cache
     * that depends on this value.
     */
    void DimensionRegion::SetVCFCutoffController(vcf_cutoff_ctrl_t controller) {
        pVelocityCutoffTable = GetCutoffVelocityTable(VCFVelocityCurve, VCFVelocityDynamicRange, VCFVelocityScale, controller);
        VCFCutoffController = controller;
    }

    /**
     * Updates the respective member variable and the lookup table / cache
     * that depends on this value.
     */
    void DimensionRegion::SetVCFVelocityCurve(curve_type_t curve) {
        pVelocityCutoffTable = GetCutoffVelocityTable(curve, VCFVelocityDynamicRange, VCFVelocityScale, VCFCutoffController);
        VCFVelocityCurve = curve;
    }

    /**
     * Updates the respective member variable and the lookup table / cache
     * that depends on this value.
     */
    void DimensionRegion::SetVCFVelocityDynamicRange(uint8_t range) {
        pVelocityCutoffTable = GetCutoffVelocityTable(VCFVelocityCurve, range, VCFVelocityScale, VCFCutoffController);
        VCFVelocityDynamicRange = range;
    }

    /**
     * Updates the respective member variable and the lookup table / cache
     * that depends on this value.
     */
    void DimensionRegion::SetVCFVelocityScale(uint8_t scaling) {
        pVelocityCutoffTable = GetCutoffVelocityTable(VCFVelocityCurve, VCFVelocityDynamicRange, scaling, VCFCutoffController);
        VCFVelocityScale = scaling;
    }

    double* DimensionRegion::CreateVelocityTable(curve_type_t curveType, uint8_t depth, uint8_t scaling) {

        // line-segment approximations of the 15 velocity curves

        // linear
        const int lin0[] = { 1, 1, 127, 127 };
        const int lin1[] = { 1, 21, 127, 127 };
        const int lin2[] = { 1, 45, 127, 127 };
        const int lin3[] = { 1, 74, 127, 127 };
        const int lin4[] = { 1, 127, 127, 127 };

        // non-linear
        const int non0[] = { 1, 4, 24, 5, 57, 17, 92, 57, 122, 127, 127, 127 };
        const int non1[] = { 1, 4, 46, 9, 93, 56, 118, 106, 123, 127,
                             127, 127 };
        const int non2[] = { 1, 4, 46, 9, 57, 20, 102, 107, 107, 127,
                             127, 127 };
        const int non3[] = { 1, 15, 10, 19, 67, 73, 80, 80, 90, 98, 98, 127,
                             127, 127 };
        const int non4[] = { 1, 25, 33, 57, 82, 81, 92, 127, 127, 127 };

        // special
        const int spe0[] = { 1, 2, 76, 10, 90, 15, 95, 20, 99, 28, 103, 44,
                             113, 127, 127, 127 };
        const int spe1[] = { 1, 2, 27, 5, 67, 18, 89, 29, 95, 35, 107, 67,
                             118, 127, 127, 127 };
        const int spe2[] = { 1, 1, 33, 1, 53, 5, 61, 13, 69, 32, 79, 74,
                             85, 90, 91, 127, 127, 127 };
        const int spe3[] = { 1, 32, 28, 35, 66, 48, 89, 59, 95, 65, 99, 73,
                             117, 127, 127, 127 };
        const int spe4[] = { 1, 4, 23, 5, 49, 13, 57, 17, 92, 57, 122, 127,
                             127, 127 };

        // this is only used by the VCF velocity curve
        const int spe5[] = { 1, 2, 30, 5, 60, 19, 77, 70, 83, 85, 88, 106,
                             91, 127, 127, 127 };

        const int* const curves[] = { non0, non1, non2, non3, non4,
                                      lin0, lin1, lin2, lin3, lin4,
                                      spe0, spe1, spe2, spe3, spe4, spe5 };

        double* const table = new double[128];

        const int* curve = curves[curveType * 5 + depth];
        const int s = scaling == 0 ? 20 : scaling; // 0 or 20 means no scaling

        table[0] = 0;
        for (int x = 1 ; x < 128 ; x++) {

            if (x > curve[2]) curve += 2;
            double y = curve[1] + (x - curve[0]) *
                (double(curve[3] - curve[1]) / (curve[2] - curve[0]));
            y = y / 127;

            // Scale up for s > 20, down for s < 20. When
            // down-scaling, the curve still ends at 1.0.
            if (s < 20 && y >= 0.5)
                y = y / ((2 - 40.0 / s) * y + 40.0 / s - 1);
            else
                y = y * (s / 20.0);
            if (y > 1) y = 1;

            table[x] = y;
        }
        return table;
    }


// *************** Region ***************
// *

    Region::Region(Instrument* pInstrument, RIFF::List* rgnList) : DLS::Region((DLS::Instrument*) pInstrument, rgnList) {
        // Initialization
        Dimensions = 0;
        for (int i = 0; i < 256; i++) {
            pDimensionRegions[i] = NULL;
        }
        Layers = 1;
        File* file = (File*) GetParent()->GetParent();
        int dimensionBits = (file->pVersion && file->pVersion->major == 3) ? 8 : 5;

        // Actual Loading

        if (!file->GetAutoLoad()) return;

        LoadDimensionRegions(rgnList);

        RIFF::Chunk* _3lnk = rgnList->GetSubChunk(CHUNK_ID_3LNK);
        if (_3lnk) {
            DimensionRegions = _3lnk->ReadUint32();
            for (int i = 0; i < dimensionBits; i++) {
                dimension_t dimension = static_cast<dimension_t>(_3lnk->ReadUint8());
                uint8_t     bits      = _3lnk->ReadUint8();
                _3lnk->ReadUint8(); // bit position of the dimension (bits[0] + bits[1] + ... + bits[i-1])
                _3lnk->ReadUint8(); // (1 << bit position of next dimension) - (1 << bit position of this dimension)
                uint8_t     zones     = _3lnk->ReadUint8(); // new for v3: number of zones doesn't have to be == pow(2,bits)
                if (dimension == dimension_none) { // inactive dimension
                    pDimensionDefinitions[i].dimension  = dimension_none;
                    pDimensionDefinitions[i].bits       = 0;
                    pDimensionDefinitions[i].zones      = 0;
                    pDimensionDefinitions[i].split_type = split_type_bit;
                    pDimensionDefinitions[i].zone_size  = 0;
                }
                else { // active dimension
                    pDimensionDefinitions[i].dimension = dimension;
                    pDimensionDefinitions[i].bits      = bits;
                    pDimensionDefinitions[i].zones     = zones ? zones : 0x01 << bits; // = pow(2,bits)
                    pDimensionDefinitions[i].split_type = __resolveSplitType(dimension);
                    pDimensionDefinitions[i].zone_size  = __resolveZoneSize(pDimensionDefinitions[i]);
                    Dimensions++;

                    // if this is a layer dimension, remember the amount of layers
                    if (dimension == dimension_layer) Layers = pDimensionDefinitions[i].zones;
                }
                _3lnk->SetPos(3, RIFF::stream_curpos); // jump forward to next dimension definition
            }
            for (int i = dimensionBits ; i < 8 ; i++) pDimensionDefinitions[i].bits = 0;

            // if there's a velocity dimension and custom velocity zone splits are used,
            // update the VelocityTables in the dimension regions
            UpdateVelocityTable();

            // jump to start of the wave pool indices (if not already there)
            if (file->pVersion && file->pVersion->major == 3)
                _3lnk->SetPos(68); // version 3 has a different 3lnk structure
            else
                _3lnk->SetPos(44);

            // load sample references (if auto loading is enabled)
            if (file->GetAutoLoad()) {
                for (uint i = 0; i < DimensionRegions; i++) {
                    uint32_t wavepoolindex = _3lnk->ReadUint32();
                    if (file->pWavePoolTable) pDimensionRegions[i]->pSample = GetSampleFromWavePool(wavepoolindex);
                }
                GetSample(); // load global region sample reference
            }
        } else {
            DimensionRegions = 0;
            for (int i = 0 ; i < 8 ; i++) {
                pDimensionDefinitions[i].dimension  = dimension_none;
                pDimensionDefinitions[i].bits       = 0;
                pDimensionDefinitions[i].zones      = 0;
            }
        }

        // make sure there is at least one dimension region
        if (!DimensionRegions) {
            RIFF::List* _3prg = rgnList->GetSubList(LIST_TYPE_3PRG);
            if (!_3prg) _3prg = rgnList->AddSubList(LIST_TYPE_3PRG);
            RIFF::List* _3ewl = _3prg->AddSubList(LIST_TYPE_3EWL);
            pDimensionRegions[0] = new DimensionRegion(this, _3ewl);
            DimensionRegions = 1;
        }
    }

    /**
     * Apply Region settings and all its DimensionRegions to the respective
     * RIFF chunks. You have to call File::Save() to make changes persistent.
     *
     * Usually there is absolutely no need to call this method explicitly.
     * It will be called automatically when File::Save() was called.
     *
     * @throws gig::Exception if samples cannot be dereferenced
     */
    void Region::UpdateChunks() {
        // in the gig format we don't care about the Region's sample reference
        // but we still have to provide some existing one to not corrupt the
        // file, so to avoid the latter we simply always assign the sample of
        // the first dimension region of this region
        pSample = pDimensionRegions[0]->pSample;

        // first update base class's chunks
        DLS::Region::UpdateChunks();

        // update dimension region's chunks
        for (int i = 0; i < DimensionRegions; i++) {
            pDimensionRegions[i]->UpdateChunks();
        }

        File* pFile = (File*) GetParent()->GetParent();
        bool version3 = pFile->pVersion && pFile->pVersion->major == 3;
        const int iMaxDimensions =  version3 ? 8 : 5;
        const int iMaxDimensionRegions = version3 ? 256 : 32;

        // make sure '3lnk' chunk exists
        RIFF::Chunk* _3lnk = pCkRegion->GetSubChunk(CHUNK_ID_3LNK);
        if (!_3lnk) {
            const int _3lnkChunkSize = version3 ? 1092 : 172;
            _3lnk = pCkRegion->AddSubChunk(CHUNK_ID_3LNK, _3lnkChunkSize);
            memset(_3lnk->LoadChunkData(), 0, _3lnkChunkSize);

            // move 3prg to last position
            pCkRegion->MoveSubChunk(pCkRegion->GetSubList(LIST_TYPE_3PRG), 0);
        }

        // update dimension definitions in '3lnk' chunk
        uint8_t* pData = (uint8_t*) _3lnk->LoadChunkData();
        store32(&pData[0], DimensionRegions);
        int shift = 0;
        for (int i = 0; i < iMaxDimensions; i++) {
            pData[4 + i * 8] = (uint8_t) pDimensionDefinitions[i].dimension;
            pData[5 + i * 8] = pDimensionDefinitions[i].bits;
            pData[6 + i * 8] = pDimensionDefinitions[i].dimension == dimension_none ? 0 : shift;
            pData[7 + i * 8] = (1 << (shift + pDimensionDefinitions[i].bits)) - (1 << shift);
            pData[8 + i * 8] = pDimensionDefinitions[i].zones;
            // next 3 bytes unknown, always zero?

            shift += pDimensionDefinitions[i].bits;
        }

        // update wave pool table in '3lnk' chunk
        const int iWavePoolOffset = version3 ? 68 : 44;
        for (uint i = 0; i < iMaxDimensionRegions; i++) {
            int iWaveIndex = -1;
            if (i < DimensionRegions) {
                if (!pFile->pSamples || !pFile->pSamples->size()) throw gig::Exception("Could not update gig::Region, there are no samples");
                File::SampleList::iterator iter = pFile->pSamples->begin();
                File::SampleList::iterator end  = pFile->pSamples->end();
                for (int index = 0; iter != end; ++iter, ++index) {
                    if (*iter == pDimensionRegions[i]->pSample) {
                        iWaveIndex = index;
                        break;
                    }
                }
            }
            store32(&pData[iWavePoolOffset + i * 4], iWaveIndex);
        }
    }

    void Region::LoadDimensionRegions(RIFF::List* rgn) {
        RIFF::List* _3prg = rgn->GetSubList(LIST_TYPE_3PRG);
        if (_3prg) {
            int dimensionRegionNr = 0;
            RIFF::List* _3ewl = _3prg->GetFirstSubList();
            while (_3ewl) {
                if (_3ewl->GetListType() == LIST_TYPE_3EWL) {
                    pDimensionRegions[dimensionRegionNr] = new DimensionRegion(this, _3ewl);
                    dimensionRegionNr++;
                }
                _3ewl = _3prg->GetNextSubList();
            }
            if (dimensionRegionNr == 0) throw gig::Exception("No dimension region found.");
        }
    }

    void Region::SetKeyRange(uint16_t Low, uint16_t High) {
        // update KeyRange struct and make sure regions are in correct order
        DLS::Region::SetKeyRange(Low, High);
        // update Region key table for fast lookup
        ((gig::Instrument*)GetParent())->UpdateRegionKeyTable();
    }

    void Region::UpdateVelocityTable() {
        // get velocity dimension's index
        int veldim = -1;
        for (int i = 0 ; i < Dimensions ; i++) {
            if (pDimensionDefinitions[i].dimension == gig::dimension_velocity) {
                veldim = i;
                break;
            }
        }
        if (veldim == -1) return;

        int step = 1;
        for (int i = 0 ; i < veldim ; i++) step <<= pDimensionDefinitions[i].bits;
        int skipveldim = (step << pDimensionDefinitions[veldim].bits) - step;
        int end = step * pDimensionDefinitions[veldim].zones;

        // loop through all dimension regions for all dimensions except the velocity dimension
        int dim[8] = { 0 };
        for (int i = 0 ; i < DimensionRegions ; i++) {

            if (pDimensionRegions[i]->DimensionUpperLimits[veldim] ||
                pDimensionRegions[i]->VelocityUpperLimit) {
                // create the velocity table
                uint8_t* table = pDimensionRegions[i]->VelocityTable;
                if (!table) {
                    table = new uint8_t[128];
                    pDimensionRegions[i]->VelocityTable = table;
                }
                int tableidx = 0;
                int velocityZone = 0;
                if (pDimensionRegions[i]->DimensionUpperLimits[veldim]) { // gig3
                    for (int k = i ; k < end ; k += step) {
                        DimensionRegion *d = pDimensionRegions[k];
                        for (; tableidx <= d->DimensionUpperLimits[veldim] ; tableidx++) table[tableidx] = velocityZone;
                        velocityZone++;
                    }
                } else { // gig2
                    for (int k = i ; k < end ; k += step) {
                        DimensionRegion *d = pDimensionRegions[k];
                        for (; tableidx <= d->VelocityUpperLimit ; tableidx++) table[tableidx] = velocityZone;
                        velocityZone++;
                    }
                }
            } else {
                if (pDimensionRegions[i]->VelocityTable) {
                    delete[] pDimensionRegions[i]->VelocityTable;
                    pDimensionRegions[i]->VelocityTable = 0;
                }
            }

            int j;
            int shift = 0;
            for (j = 0 ; j < Dimensions ; j++) {
                if (j == veldim) i += skipveldim; // skip velocity dimension
                else {
                    dim[j]++;
                    if (dim[j] < pDimensionDefinitions[j].zones) break;
                    else {
                        // skip unused dimension regions
                        dim[j] = 0;
                        i += ((1 << pDimensionDefinitions[j].bits) -
                              pDimensionDefinitions[j].zones) << shift;
                    }
                }
                shift += pDimensionDefinitions[j].bits;
            }
            if (j == Dimensions) break;
        }
    }

    /** @brief Einstein would have dreamed of it - create a new dimension.
     *
     * Creates a new dimension with the dimension definition given by
     * \a pDimDef. The appropriate amount of DimensionRegions will be created.
     * There is a hard limit of dimensions and total amount of "bits" all
     * dimensions can have. This limit is dependant to what gig file format
     * version this file refers to. The gig v2 (and lower) format has a
     * dimension limit and total amount of bits limit of 5, whereas the gig v3
     * format has a limit of 8.
     *
     * @param pDimDef - defintion of the new dimension
     * @throws gig::Exception if dimension of the same type exists already
     * @throws gig::Exception if amount of dimensions or total amount of
     *                        dimension bits limit is violated
     */
    void Region::AddDimension(dimension_def_t* pDimDef) {
        // check if max. amount of dimensions reached
        File* file = (File*) GetParent()->GetParent();
        const int iMaxDimensions = (file->pVersion && file->pVersion->major == 3) ? 8 : 5;
        if (Dimensions >= iMaxDimensions)
            throw gig::Exception("Could not add new dimension, max. amount of " + ToString(iMaxDimensions) + " dimensions already reached");
        // check if max. amount of dimension bits reached
        int iCurrentBits = 0;
        for (int i = 0; i < Dimensions; i++)
            iCurrentBits += pDimensionDefinitions[i].bits;
        if (iCurrentBits >= iMaxDimensions)
            throw gig::Exception("Could not add new dimension, max. amount of " + ToString(iMaxDimensions) + " dimension bits already reached");
        const int iNewBits = iCurrentBits + pDimDef->bits;
        if (iNewBits > iMaxDimensions)
            throw gig::Exception("Could not add new dimension, new dimension would exceed max. amount of " + ToString(iMaxDimensions) + " dimension bits");
        // check if there's already a dimensions of the same type
        for (int i = 0; i < Dimensions; i++)
            if (pDimensionDefinitions[i].dimension == pDimDef->dimension)
                throw gig::Exception("Could not add new dimension, there is already a dimension of the same type");

        // pos is where the new dimension should be placed, normally
        // last in list, except for the samplechannel dimension which
        // has to be first in list
        int pos = pDimDef->dimension == dimension_samplechannel ? 0 : Dimensions;
        int bitpos = 0;
        for (int i = 0 ; i < pos ; i++)
            bitpos += pDimensionDefinitions[i].bits;

        // make room for the new dimension
        for (int i = Dimensions ; i > pos ; i--) pDimensionDefinitions[i] = pDimensionDefinitions[i - 1];
        for (int i = 0 ; i < (1 << iCurrentBits) ; i++) {
            for (int j = Dimensions ; j > pos ; j--) {
                pDimensionRegions[i]->DimensionUpperLimits[j] =
                    pDimensionRegions[i]->DimensionUpperLimits[j - 1];
            }
        }

        // assign definition of new dimension
        pDimensionDefinitions[pos] = *pDimDef;

        // auto correct certain dimension definition fields (where possible)
        pDimensionDefinitions[pos].split_type  =
            __resolveSplitType(pDimensionDefinitions[pos].dimension);
        pDimensionDefinitions[pos].zone_size =
            __resolveZoneSize(pDimensionDefinitions[pos]);

        // create new dimension region(s) for this new dimension, and make
        // sure that the dimension regions are placed correctly in both the
        // RIFF list and the pDimensionRegions array
        RIFF::Chunk* moveTo = NULL;
        RIFF::List* _3prg = pCkRegion->GetSubList(LIST_TYPE_3PRG);
        for (int i = (1 << iCurrentBits) - (1 << bitpos) ; i >= 0 ; i -= (1 << bitpos)) {
            for (int k = 0 ; k < (1 << bitpos) ; k++) {
                pDimensionRegions[(i << pDimDef->bits) + k] = pDimensionRegions[i + k];
            }
            for (int j = 1 ; j < (1 << pDimDef->bits) ; j++) {
                for (int k = 0 ; k < (1 << bitpos) ; k++) {
                    RIFF::List* pNewDimRgnListChunk = _3prg->AddSubList(LIST_TYPE_3EWL);
                    if (moveTo) _3prg->MoveSubChunk(pNewDimRgnListChunk, moveTo);
                    // create a new dimension region and copy all parameter values from
                    // an existing dimension region
                    pDimensionRegions[(i << pDimDef->bits) + (j << bitpos) + k] =
                        new DimensionRegion(pNewDimRgnListChunk, *pDimensionRegions[i + k]);

                    DimensionRegions++;
                }
            }
            moveTo = pDimensionRegions[i]->pParentList;
        }

        // initialize the upper limits for this dimension
        int mask = (1 << bitpos) - 1;
        for (int z = 0 ; z < pDimDef->zones ; z++) {
            uint8_t upperLimit = uint8_t((z + 1) * 128.0 / pDimDef->zones - 1);
            for (int i = 0 ; i < 1 << iCurrentBits ; i++) {
                pDimensionRegions[((i & ~mask) << pDimDef->bits) |
                                  (z << bitpos) |
                                  (i & mask)]->DimensionUpperLimits[pos] = upperLimit;
            }
        }

        Dimensions++;

        // if this is a layer dimension, update 'Layers' attribute
        if (pDimDef->dimension == dimension_layer) Layers = pDimDef->zones;

        UpdateVelocityTable();
    }

    /** @brief Delete an existing dimension.
     *
     * Deletes the dimension given by \a pDimDef and deletes all respective
     * dimension regions, that is all dimension regions where the dimension's
     * bit(s) part is greater than 0. In case of a 'sustain pedal' dimension
     * for example this would delete all dimension regions for the case(s)
     * where the sustain pedal is pressed down.
     *
     * @param pDimDef - dimension to delete
     * @throws gig::Exception if given dimension cannot be found
     */
    void Region::DeleteDimension(dimension_def_t* pDimDef) {
        // get dimension's index
        int iDimensionNr = -1;
        for (int i = 0; i < Dimensions; i++) {
            if (&pDimensionDefinitions[i] == pDimDef) {
                iDimensionNr = i;
                break;
            }
        }
        if (iDimensionNr < 0) throw gig::Exception("Invalid dimension_def_t pointer");

        // get amount of bits below the dimension to delete
        int iLowerBits = 0;
        for (int i = 0; i < iDimensionNr; i++)
            iLowerBits += pDimensionDefinitions[i].bits;

        // get amount ot bits above the dimension to delete
        int iUpperBits = 0;
        for (int i = iDimensionNr + 1; i < Dimensions; i++)
            iUpperBits += pDimensionDefinitions[i].bits;

        RIFF::List* _3prg = pCkRegion->GetSubList(LIST_TYPE_3PRG);

        // delete dimension regions which belong to the given dimension
        // (that is where the dimension's bit > 0)
        for (int iUpperBit = 0; iUpperBit < 1 << iUpperBits; iUpperBit++) {
            for (int iObsoleteBit = 1; iObsoleteBit < 1 << pDimensionDefinitions[iDimensionNr].bits; iObsoleteBit++) {
                for (int iLowerBit = 0; iLowerBit < 1 << iLowerBits; iLowerBit++) {
                    int iToDelete = iUpperBit    << (pDimensionDefinitions[iDimensionNr].bits + iLowerBits) |
                                    iObsoleteBit << iLowerBits |
                                    iLowerBit;

                    _3prg->DeleteSubChunk(pDimensionRegions[iToDelete]->pParentList);
                    delete pDimensionRegions[iToDelete];
                    pDimensionRegions[iToDelete] = NULL;
                    DimensionRegions--;
                }
            }
        }

        // defrag pDimensionRegions array
        // (that is remove the NULL spaces within the pDimensionRegions array)
        for (int iFrom = 2, iTo = 1; iFrom < 256 && iTo < 256 - 1; iTo++) {
            if (!pDimensionRegions[iTo]) {
                if (iFrom <= iTo) iFrom = iTo + 1;
                while (!pDimensionRegions[iFrom] && iFrom < 256) iFrom++;
                if (iFrom < 256 && pDimensionRegions[iFrom]) {
                    pDimensionRegions[iTo]   = pDimensionRegions[iFrom];
                    pDimensionRegions[iFrom] = NULL;
                }
            }
        }

        // remove the this dimension from the upper limits arrays
        for (int j = 0 ; j < 256 && pDimensionRegions[j] ; j++) {
            DimensionRegion* d = pDimensionRegions[j];
            for (int i = iDimensionNr + 1; i < Dimensions; i++) {
                d->DimensionUpperLimits[i - 1] = d->DimensionUpperLimits[i];
            }
            d->DimensionUpperLimits[Dimensions - 1] = 127;
        }

        // 'remove' dimension definition
        for (int i = iDimensionNr + 1; i < Dimensions; i++) {
            pDimensionDefinitions[i - 1] = pDimensionDefinitions[i];
        }
        pDimensionDefinitions[Dimensions - 1].dimension = dimension_none;
        pDimensionDefinitions[Dimensions - 1].bits      = 0;
        pDimensionDefinitions[Dimensions - 1].zones     = 0;

        Dimensions--;

        // if this was a layer dimension, update 'Layers' attribute
        if (pDimDef->dimension == dimension_layer) Layers = 1;
    }

    Region::~Region() {
        for (int i = 0; i < 256; i++) {
            if (pDimensionRegions[i]) delete pDimensionRegions[i];
        }
    }

    /**
     * Use this method in your audio engine to get the appropriate dimension
     * region with it's articulation data for the current situation. Just
     * call the method with the current MIDI controller values and you'll get
     * the DimensionRegion with the appropriate articulation data for the
     * current situation (for this Region of course only). To do that you'll
     * first have to look which dimensions with which controllers and in
     * which order are defined for this Region when you load the .gig file.
     * Special cases are e.g. layer or channel dimensions where you just put
     * in the index numbers instead of a MIDI controller value (means 0 for
     * left channel, 1 for right channel or 0 for layer 0, 1 for layer 1,
     * etc.).
     *
     * @param  DimValues  MIDI controller values (0-127) for dimension 0 to 7
     * @returns         adress to the DimensionRegion for the given situation
     * @see             pDimensionDefinitions
     * @see             Dimensions
     */
    DimensionRegion* Region::GetDimensionRegionByValue(const uint DimValues[8]) {
        uint8_t bits;
        int veldim = -1;
        int velbitpos;
        int bitpos = 0;
        int dimregidx = 0;
        for (uint i = 0; i < Dimensions; i++) {
            if (pDimensionDefinitions[i].dimension == dimension_velocity) {
                // the velocity dimension must be handled after the other dimensions
                veldim = i;
                velbitpos = bitpos;
            } else {
                switch (pDimensionDefinitions[i].split_type) {
                    case split_type_normal:
                        if (pDimensionRegions[0]->DimensionUpperLimits[i]) {
                            // gig3: all normal dimensions (not just the velocity dimension) have custom zone ranges
                            for (bits = 0 ; bits < pDimensionDefinitions[i].zones ; bits++) {
                                if (DimValues[i] <= pDimensionRegions[bits << bitpos]->DimensionUpperLimits[i]) break;
                            }
                        } else {
                            // gig2: evenly sized zones
                            bits = uint8_t(DimValues[i] / pDimensionDefinitions[i].zone_size);
                        }
                        break;
                    case split_type_bit: // the value is already the sought dimension bit number
                        const uint8_t limiter_mask = (0xff << pDimensionDefinitions[i].bits) ^ 0xff;
                        bits = DimValues[i] & limiter_mask; // just make sure the value doesn't use more bits than allowed
                        break;
                }
                dimregidx |= bits << bitpos;
            }
            bitpos += pDimensionDefinitions[i].bits;
        }
        DimensionRegion* dimreg = pDimensionRegions[dimregidx];
        if (veldim != -1) {
            // (dimreg is now the dimension region for the lowest velocity)
            if (dimreg->VelocityTable) // custom defined zone ranges
                bits = dimreg->VelocityTable[DimValues[veldim]];
            else // normal split type
                bits = uint8_t(DimValues[veldim] / pDimensionDefinitions[veldim].zone_size);

            dimregidx |= bits << velbitpos;
            dimreg = pDimensionRegions[dimregidx];
        }
        return dimreg;
    }

    /**
     * Returns the appropriate DimensionRegion for the given dimension bit
     * numbers (zone index). You usually use <i>GetDimensionRegionByValue</i>
     * instead of calling this method directly!
     *
     * @param DimBits  Bit numbers for dimension 0 to 7
     * @returns        adress to the DimensionRegion for the given dimension
     *                 bit numbers
     * @see            GetDimensionRegionByValue()
     */
    DimensionRegion* Region::GetDimensionRegionByBit(const uint8_t DimBits[8]) {
        return pDimensionRegions[((((((DimBits[7] << pDimensionDefinitions[6].bits | DimBits[6])
                                                  << pDimensionDefinitions[5].bits | DimBits[5])
                                                  << pDimensionDefinitions[4].bits | DimBits[4])
                                                  << pDimensionDefinitions[3].bits | DimBits[3])
                                                  << pDimensionDefinitions[2].bits | DimBits[2])
                                                  << pDimensionDefinitions[1].bits | DimBits[1])
                                                  << pDimensionDefinitions[0].bits | DimBits[0]];
    }

    /**
     * Returns pointer address to the Sample referenced with this region.
     * This is the global Sample for the entire Region (not sure if this is
     * actually used by the Gigasampler engine - I would only use the Sample
     * referenced by the appropriate DimensionRegion instead of this sample).
     *
     * @returns  address to Sample or NULL if there is no reference to a
     *           sample saved in the .gig file
     */
    Sample* Region::GetSample() {
        if (pSample) return static_cast<gig::Sample*>(pSample);
        else         return static_cast<gig::Sample*>(pSample = GetSampleFromWavePool(WavePoolTableIndex));
    }

    Sample* Region::GetSampleFromWavePool(unsigned int WavePoolTableIndex, progress_t* pProgress) {
        if ((int32_t)WavePoolTableIndex == -1) return NULL;
        File* file = (File*) GetParent()->GetParent();
        if (!file->pWavePoolTable) return NULL;
        unsigned long soughtoffset = file->pWavePoolTable[WavePoolTableIndex];
        unsigned long soughtfileno = file->pWavePoolTableHi[WavePoolTableIndex];
        Sample* sample = file->GetFirstSample(pProgress);
        while (sample) {
            if (sample->ulWavePoolOffset == soughtoffset &&
                sample->FileNo == soughtfileno) return static_cast<gig::Sample*>(sample);
            sample = file->GetNextSample();
        }
        return NULL;
    }


// *************** MidiRule ***************
// *

MidiRuleCtrlTrigger::MidiRuleCtrlTrigger(RIFF::Chunk* _3ewg) {
    _3ewg->SetPos(36);
    Triggers = _3ewg->ReadUint8();
    _3ewg->SetPos(40);
    ControllerNumber = _3ewg->ReadUint8();
    _3ewg->SetPos(46);
    for (int i = 0 ; i < Triggers ; i++) {
        pTriggers[i].TriggerPoint = _3ewg->ReadUint8();
        pTriggers[i].Descending = _3ewg->ReadUint8();
        pTriggers[i].VelSensitivity = _3ewg->ReadUint8();
        pTriggers[i].Key = _3ewg->ReadUint8();
        pTriggers[i].NoteOff = _3ewg->ReadUint8();
        pTriggers[i].Velocity = _3ewg->ReadUint8();
        pTriggers[i].OverridePedal = _3ewg->ReadUint8();
        _3ewg->ReadUint8();
    }
}


// *************** Instrument ***************
// *

    Instrument::Instrument(File* pFile, RIFF::List* insList, progress_t* pProgress) : DLS::Instrument((DLS::File*)pFile, insList) {
        static const DLS::Info::string_length_t fixedStringLengths[] = {
            { CHUNK_ID_INAM, 64 },
            { CHUNK_ID_ISFT, 12 },
            { 0, 0 }
        };
        pInfo->SetFixedStringLengths(fixedStringLengths);

        // Initialization
        for (int i = 0; i < 128; i++) RegionKeyTable[i] = NULL;
        EffectSend = 0;
        Attenuation = 0;
        FineTune = 0;
        PitchbendRange = 0;
        PianoReleaseMode = false;
        DimensionKeyRange.low = 0;
        DimensionKeyRange.high = 0;
        pMidiRules = new MidiRule*[3];
        pMidiRules[0] = NULL;

        // Loading
        RIFF::List* lart = insList->GetSubList(LIST_TYPE_LART);
        if (lart) {
            RIFF::Chunk* _3ewg = lart->GetSubChunk(CHUNK_ID_3EWG);
            if (_3ewg) {
                EffectSend             = _3ewg->ReadUint16();
                Attenuation            = _3ewg->ReadInt32();
                FineTune               = _3ewg->ReadInt16();
                PitchbendRange         = _3ewg->ReadInt16();
                uint8_t dimkeystart    = _3ewg->ReadUint8();
                PianoReleaseMode       = dimkeystart & 0x01;
                DimensionKeyRange.low  = dimkeystart >> 1;
                DimensionKeyRange.high = _3ewg->ReadUint8();

                if (_3ewg->GetSize() > 32) {
                    // read MIDI rules
                    int i = 0;
                    _3ewg->SetPos(32);
                    uint8_t id1 = _3ewg->ReadUint8();
                    uint8_t id2 = _3ewg->ReadUint8();

                    if (id1 == 4 && id2 == 16) {
                        pMidiRules[i++] = new MidiRuleCtrlTrigger(_3ewg);
                    }
                    //TODO: all the other types of rules

                    pMidiRules[i] = NULL;
                }
            }
        }

        if (pFile->GetAutoLoad()) {
            if (!pRegions) pRegions = new RegionList;
            RIFF::List* lrgn = insList->GetSubList(LIST_TYPE_LRGN);
            if (lrgn) {
                RIFF::List* rgn = lrgn->GetFirstSubList();
                while (rgn) {
                    if (rgn->GetListType() == LIST_TYPE_RGN) {
                        __notify_progress(pProgress, (float) pRegions->size() / (float) Regions);
                        pRegions->push_back(new Region(this, rgn));
                    }
                    rgn = lrgn->GetNextSubList();
                }
                // Creating Region Key Table for fast lookup
                UpdateRegionKeyTable();
            }
        }

        __notify_progress(pProgress, 1.0f); // notify done
    }

    void Instrument::UpdateRegionKeyTable() {
        for (int i = 0; i < 128; i++) RegionKeyTable[i] = NULL;
        RegionList::iterator iter = pRegions->begin();
        RegionList::iterator end  = pRegions->end();
        for (; iter != end; ++iter) {
            gig::Region* pRegion = static_cast<gig::Region*>(*iter);
            for (int iKey = pRegion->KeyRange.low; iKey <= pRegion->KeyRange.high; iKey++) {
                RegionKeyTable[iKey] = pRegion;
            }
        }
    }

    Instrument::~Instrument() {
        for (int i = 0 ; pMidiRules[i] ; i++) {
            delete pMidiRules[i];
        }
        delete[] pMidiRules;
    }

    /**
     * Apply Instrument with all its Regions to the respective RIFF chunks.
     * You have to call File::Save() to make changes persistent.
     *
     * Usually there is absolutely no need to call this method explicitly.
     * It will be called automatically when File::Save() was called.
     *
     * @throws gig::Exception if samples cannot be dereferenced
     */
    void Instrument::UpdateChunks() {
        // first update base classes' chunks
        DLS::Instrument::UpdateChunks();

        // update Regions' chunks
        {
            RegionList::iterator iter = pRegions->begin();
            RegionList::iterator end  = pRegions->end();
            for (; iter != end; ++iter)
                (*iter)->UpdateChunks();
        }

        // make sure 'lart' RIFF list chunk exists
        RIFF::List* lart = pCkInstrument->GetSubList(LIST_TYPE_LART);
        if (!lart)  lart = pCkInstrument->AddSubList(LIST_TYPE_LART);
        // make sure '3ewg' RIFF chunk exists
        RIFF::Chunk* _3ewg = lart->GetSubChunk(CHUNK_ID_3EWG);
        if (!_3ewg)  {
            File* pFile = (File*) GetParent();

            // 3ewg is bigger in gig3, as it includes the iMIDI rules
            int size = (pFile->pVersion && pFile->pVersion->major == 3) ? 16416 : 12;
            _3ewg = lart->AddSubChunk(CHUNK_ID_3EWG, size);
            memset(_3ewg->LoadChunkData(), 0, size);
        }
        // update '3ewg' RIFF chunk
        uint8_t* pData = (uint8_t*) _3ewg->LoadChunkData();
        store16(&pData[0], EffectSend);
        store32(&pData[2], Attenuation);
        store16(&pData[6], FineTune);
        store16(&pData[8], PitchbendRange);
        const uint8_t dimkeystart = (PianoReleaseMode ? 0x01 : 0x00) |
                                    DimensionKeyRange.low << 1;
        pData[10] = dimkeystart;
        pData[11] = DimensionKeyRange.high;
    }

    /**
     * Returns the appropriate Region for a triggered note.
     *
     * @param Key  MIDI Key number of triggered note / key (0 - 127)
     * @returns    pointer adress to the appropriate Region or NULL if there
     *             there is no Region defined for the given \a Key
     */
    Region* Instrument::GetRegion(unsigned int Key) {
        if (!pRegions || pRegions->empty() || Key > 127) return NULL;
        return RegionKeyTable[Key];

        /*for (int i = 0; i < Regions; i++) {
            if (Key <= pRegions[i]->KeyRange.high &&
                Key >= pRegions[i]->KeyRange.low) return pRegions[i];
        }
        return NULL;*/
    }

    /**
     * Returns the first Region of the instrument. You have to call this
     * method once before you use GetNextRegion().
     *
     * @returns  pointer address to first region or NULL if there is none
     * @see      GetNextRegion()
     */
    Region* Instrument::GetFirstRegion() {
        if (!pRegions) return NULL;
        RegionsIterator = pRegions->begin();
        return static_cast<gig::Region*>( (RegionsIterator != pRegions->end()) ? *RegionsIterator : NULL );
    }

    /**
     * Returns the next Region of the instrument. You have to call
     * GetFirstRegion() once before you can use this method. By calling this
     * method multiple times it iterates through the available Regions.
     *
     * @returns  pointer address to the next region or NULL if end reached
     * @see      GetFirstRegion()
     */
    Region* Instrument::GetNextRegion() {
        if (!pRegions) return NULL;
        RegionsIterator++;
        return static_cast<gig::Region*>( (RegionsIterator != pRegions->end()) ? *RegionsIterator : NULL );
    }

    Region* Instrument::AddRegion() {
        // create new Region object (and its RIFF chunks)
        RIFF::List* lrgn = pCkInstrument->GetSubList(LIST_TYPE_LRGN);
        if (!lrgn)  lrgn = pCkInstrument->AddSubList(LIST_TYPE_LRGN);
        RIFF::List* rgn = lrgn->AddSubList(LIST_TYPE_RGN);
        Region* pNewRegion = new Region(this, rgn);
        pRegions->push_back(pNewRegion);
        Regions = pRegions->size();
        // update Region key table for fast lookup
        UpdateRegionKeyTable();
        // done
        return pNewRegion;
    }

    void Instrument::DeleteRegion(Region* pRegion) {
        if (!pRegions) return;
        DLS::Instrument::DeleteRegion((DLS::Region*) pRegion);
        // update Region key table for fast lookup
        UpdateRegionKeyTable();
    }

    /**
     * Returns a MIDI rule of the instrument.
     *
     * The list of MIDI rules, at least in gig v3, always contains at
     * most two rules. The second rule can only be the DEF filter
     * (which currently isn't supported by libgig).
     *
     * @param i - MIDI rule number
     * @returns   pointer address to MIDI rule number i or NULL if there is none
     */
    MidiRule* Instrument::GetMidiRule(int i) {
        return pMidiRules[i];
    }


// *************** Group ***************
// *

    /** @brief Constructor.
     *
     * @param file   - pointer to the gig::File object
     * @param ck3gnm - pointer to 3gnm chunk associated with this group or
     *                 NULL if this is a new Group
     */
    Group::Group(File* file, RIFF::Chunk* ck3gnm) {
        pFile      = file;
        pNameChunk = ck3gnm;
        ::LoadString(pNameChunk, Name);
    }

    Group::~Group() {
        // remove the chunk associated with this group (if any)
        if (pNameChunk) pNameChunk->GetParent()->DeleteSubChunk(pNameChunk);
    }

    /** @brief Update chunks with current group settings.
     *
     * Apply current Group field values to the respective chunks. You have
     * to call File::Save() to make changes persistent.
     *
     * Usually there is absolutely no need to call this method explicitly.
     * It will be called automatically when File::Save() was called.
     */
    void Group::UpdateChunks() {
        // make sure <3gri> and <3gnl> list chunks exist
        RIFF::List* _3gri = pFile->pRIFF->GetSubList(LIST_TYPE_3GRI);
        if (!_3gri) {
            _3gri = pFile->pRIFF->AddSubList(LIST_TYPE_3GRI);
            pFile->pRIFF->MoveSubChunk(_3gri, pFile->pRIFF->GetSubChunk(CHUNK_ID_PTBL));
        }
        RIFF::List* _3gnl = _3gri->GetSubList(LIST_TYPE_3GNL);
        if (!_3gnl) _3gnl = _3gri->AddSubList(LIST_TYPE_3GNL);

        if (!pNameChunk && pFile->pVersion && pFile->pVersion->major == 3) {
            // v3 has a fixed list of 128 strings, find a free one
            for (RIFF::Chunk* ck = _3gnl->GetFirstSubChunk() ; ck ; ck = _3gnl->GetNextSubChunk()) {
                if (strcmp(static_cast<char*>(ck->LoadChunkData()), "") == 0) {
                    pNameChunk = ck;
                    break;
                }
            }
        }

        // now store the name of this group as <3gnm> chunk as subchunk of the <3gnl> list chunk
        ::SaveString(CHUNK_ID_3GNM, pNameChunk, _3gnl, Name, String("Unnamed Group"), true, 64);
    }

    /**
     * Returns the first Sample of this Group. You have to call this method
     * once before you use GetNextSample().
     *
     * <b>Notice:</b> this method might block for a long time, in case the
     * samples of this .gig file were not scanned yet
     *
     * @returns  pointer address to first Sample or NULL if there is none
     *           applied to this Group
     * @see      GetNextSample()
     */
    Sample* Group::GetFirstSample() {
        // FIXME: lazy und unsafe implementation, should be an autonomous iterator
        for (Sample* pSample = pFile->GetFirstSample(); pSample; pSample = pFile->GetNextSample()) {
            if (pSample->GetGroup() == this) return pSample;
        }
        return NULL;
    }

    /**
     * Returns the next Sample of the Group. You have to call
     * GetFirstSample() once before you can use this method. By calling this
     * method multiple times it iterates through the Samples assigned to
     * this Group.
     *
     * @returns  pointer address to the next Sample of this Group or NULL if
     *           end reached
     * @see      GetFirstSample()
     */
    Sample* Group::GetNextSample() {
        // FIXME: lazy und unsafe implementation, should be an autonomous iterator
        for (Sample* pSample = pFile->GetNextSample(); pSample; pSample = pFile->GetNextSample()) {
            if (pSample->GetGroup() == this) return pSample;
        }
        return NULL;
    }

    /**
     * Move Sample given by \a pSample from another Group to this Group.
     */
    void Group::AddSample(Sample* pSample) {
        pSample->pGroup = this;
    }

    /**
     * Move all members of this group to another group (preferably the 1st
     * one except this). This method is called explicitly by
     * File::DeleteGroup() thus when a Group was deleted. This code was
     * intentionally not placed in the destructor!
     */
    void Group::MoveAll() {
        // get "that" other group first
        Group* pOtherGroup = NULL;
        for (pOtherGroup = pFile->GetFirstGroup(); pOtherGroup; pOtherGroup = pFile->GetNextGroup()) {
            if (pOtherGroup != this) break;
        }
        if (!pOtherGroup) throw Exception(
            "Could not move samples to another group, since there is no "
            "other Group. This is a bug, report it!"
        );
        // now move all samples of this group to the other group
        for (Sample* pSample = GetFirstSample(); pSample; pSample = GetNextSample()) {
            pOtherGroup->AddSample(pSample);
        }
    }



// *************** File ***************
// *

    /// Reflects Gigasampler file format version 2.0 (1998-06-28).
    const DLS::version_t File::VERSION_2 = {
        0, 2, 19980628 & 0xffff, 19980628 >> 16
    };

    /// Reflects Gigasampler file format version 3.0 (2003-03-31).
    const DLS::version_t File::VERSION_3 = {
        0, 3, 20030331 & 0xffff, 20030331 >> 16
    };

    static const DLS::Info::string_length_t _FileFixedStringLengths[] = {
        { CHUNK_ID_IARL, 256 },
        { CHUNK_ID_IART, 128 },
        { CHUNK_ID_ICMS, 128 },
        { CHUNK_ID_ICMT, 1024 },
        { CHUNK_ID_ICOP, 128 },
        { CHUNK_ID_ICRD, 128 },
        { CHUNK_ID_IENG, 128 },
        { CHUNK_ID_IGNR, 128 },
        { CHUNK_ID_IKEY, 128 },
        { CHUNK_ID_IMED, 128 },
        { CHUNK_ID_INAM, 128 },
        { CHUNK_ID_IPRD, 128 },
        { CHUNK_ID_ISBJ, 128 },
        { CHUNK_ID_ISFT, 128 },
        { CHUNK_ID_ISRC, 128 },
        { CHUNK_ID_ISRF, 128 },
        { CHUNK_ID_ITCH, 128 },
        { 0, 0 }
    };

    File::File() : DLS::File() {
        bAutoLoad = true;
        *pVersion = VERSION_3;
        pGroups = NULL;
        pInfo->SetFixedStringLengths(_FileFixedStringLengths);
        pInfo->ArchivalLocation = String(256, ' ');

        // add some mandatory chunks to get the file chunks in right
        // order (INFO chunk will be moved to first position later)
        pRIFF->AddSubChunk(CHUNK_ID_VERS, 8);
        pRIFF->AddSubChunk(CHUNK_ID_COLH, 4);
        pRIFF->AddSubChunk(CHUNK_ID_DLID, 16);

        GenerateDLSID();
    }

    File::File(RIFF::File* pRIFF) : DLS::File(pRIFF) {
        bAutoLoad = true;
        pGroups = NULL;
        pInfo->SetFixedStringLengths(_FileFixedStringLengths);
    }

    File::~File() {
        if (pGroups) {
            std::list<Group*>::iterator iter = pGroups->begin();
            std::list<Group*>::iterator end  = pGroups->end();
            while (iter != end) {
                delete *iter;
                ++iter;
            }
            delete pGroups;
        }
    }

    Sample* File::GetFirstSample(progress_t* pProgress) {
        if (!pSamples) LoadSamples(pProgress);
        if (!pSamples) return NULL;
        SamplesIterator = pSamples->begin();
        return static_cast<gig::Sample*>( (SamplesIterator != pSamples->end()) ? *SamplesIterator : NULL );
    }

    Sample* File::GetNextSample() {
        if (!pSamples) return NULL;
        SamplesIterator++;
        return static_cast<gig::Sample*>( (SamplesIterator != pSamples->end()) ? *SamplesIterator : NULL );
    }

    /** @brief Add a new sample.
     *
     * This will create a new Sample object for the gig file. You have to
     * call Save() to make this persistent to the file.
     *
     * @returns pointer to new Sample object
     */
    Sample* File::AddSample() {
       if (!pSamples) LoadSamples();
       __ensureMandatoryChunksExist();
       RIFF::List* wvpl = pRIFF->GetSubList(LIST_TYPE_WVPL);
       // create new Sample object and its respective 'wave' list chunk
       RIFF::List* wave = wvpl->AddSubList(LIST_TYPE_WAVE);
       Sample* pSample = new Sample(this, wave, 0 /*arbitrary value, we update offsets when we save*/);

       // add mandatory chunks to get the chunks in right order
       wave->AddSubChunk(CHUNK_ID_FMT, 16);
       wave->AddSubList(LIST_TYPE_INFO);

       pSamples->push_back(pSample);
       return pSample;
    }

    /** @brief Delete a sample.
     *
     * This will delete the given Sample object from the gig file. Any
     * references to this sample from Regions and DimensionRegions will be
     * removed. You have to call Save() to make this persistent to the file.
     *
     * @param pSample - sample to delete
     * @throws gig::Exception if given sample could not be found
     */
    void File::DeleteSample(Sample* pSample) {
        if (!pSamples || !pSamples->size()) throw gig::Exception("Could not delete sample as there are no samples");
        SampleList::iterator iter = find(pSamples->begin(), pSamples->end(), (DLS::Sample*) pSample);
        if (iter == pSamples->end()) throw gig::Exception("Could not delete sample, could not find given sample");
        if (SamplesIterator != pSamples->end() && *SamplesIterator == pSample) ++SamplesIterator; // avoid iterator invalidation
        pSamples->erase(iter);
        delete pSample;

        SampleList::iterator tmp = SamplesIterator;
        // remove all references to the sample
        for (Instrument* instrument = GetFirstInstrument() ; instrument ;
             instrument = GetNextInstrument()) {
            for (Region* region = instrument->GetFirstRegion() ; region ;
                 region = instrument->GetNextRegion()) {

                if (region->GetSample() == pSample) region->SetSample(NULL);

                for (int i = 0 ; i < region->DimensionRegions ; i++) {
                    gig::DimensionRegion *d = region->pDimensionRegions[i];
                    if (d->pSample == pSample) d->pSample = NULL;
                }
            }
        }
        SamplesIterator = tmp; // restore iterator
    }

    void File::LoadSamples() {
        LoadSamples(NULL);
    }

    void File::LoadSamples(progress_t* pProgress) {
        // Groups must be loaded before samples, because samples will try
        // to resolve the group they belong to
        if (!pGroups) LoadGroups();

        if (!pSamples) pSamples = new SampleList;

        RIFF::File* file = pRIFF;

        // just for progress calculation
        int iSampleIndex  = 0;
        int iTotalSamples = WavePoolCount;

        // check if samples should be loaded from extension files
        int lastFileNo = 0;
        for (int i = 0 ; i < WavePoolCount ; i++) {
            if (pWavePoolTableHi[i] > lastFileNo) lastFileNo = pWavePoolTableHi[i];
        }
        String name(pRIFF->GetFileName());
        int nameLen = name.length();
        char suffix[6];
        if (nameLen > 4 && name.substr(nameLen - 4) == ".gig") nameLen -= 4;

        for (int fileNo = 0 ; ; ) {
            RIFF::List* wvpl = file->GetSubList(LIST_TYPE_WVPL);
            if (wvpl) {
                unsigned long wvplFileOffset = wvpl->GetFilePos();
                RIFF::List* wave = wvpl->GetFirstSubList();
                while (wave) {
                    if (wave->GetListType() == LIST_TYPE_WAVE) {
                        // notify current progress
                        const float subprogress = (float) iSampleIndex / (float) iTotalSamples;
                        __notify_progress(pProgress, subprogress);

                        unsigned long waveFileOffset = wave->GetFilePos();
                        pSamples->push_back(new Sample(this, wave, waveFileOffset - wvplFileOffset, fileNo));

                        iSampleIndex++;
                    }
                    wave = wvpl->GetNextSubList();
                }

                if (fileNo == lastFileNo) break;

                // open extension file (*.gx01, *.gx02, ...)
                fileNo++;
                sprintf(suffix, ".gx%02d", fileNo);
                name.replace(nameLen, 5, suffix);
                file = new RIFF::File(name);
                ExtensionFiles.push_back(file);
            } else break;
        }

        __notify_progress(pProgress, 1.0); // notify done
    }

    Instrument* File::GetFirstInstrument() {
        if (!pInstruments) LoadInstruments();
        if (!pInstruments) return NULL;
        InstrumentsIterator = pInstruments->begin();
        return static_cast<gig::Instrument*>( (InstrumentsIterator != pInstruments->end()) ? *InstrumentsIterator : NULL );
    }

    Instrument* File::GetNextInstrument() {
        if (!pInstruments) return NULL;
        InstrumentsIterator++;
        return static_cast<gig::Instrument*>( (InstrumentsIterator != pInstruments->end()) ? *InstrumentsIterator : NULL );
    }

    /**
     * Returns the instrument with the given index.
     *
     * @param index     - number of the sought instrument (0..n)
     * @param pProgress - optional: callback function for progress notification
     * @returns  sought instrument or NULL if there's no such instrument
     */
    Instrument* File::GetInstrument(uint index, progress_t* pProgress) {
        if (!pInstruments) {
            // TODO: hack - we simply load ALL samples here, it would have been done in the Region constructor anyway (ATM)

            // sample loading subtask
            progress_t subprogress;
            __divide_progress(pProgress, &subprogress, 3.0f, 0.0f); // randomly schedule 33% for this subtask
            __notify_progress(&subprogress, 0.0f);
            if (GetAutoLoad())
                GetFirstSample(&subprogress); // now force all samples to be loaded
            __notify_progress(&subprogress, 1.0f);

            // instrument loading subtask
            if (pProgress && pProgress->callback) {
                subprogress.__range_min = subprogress.__range_max;
                subprogress.__range_max = pProgress->__range_max; // schedule remaining percentage for this subtask
            }
            __notify_progress(&subprogress, 0.0f);
            LoadInstruments(&subprogress);
            __notify_progress(&subprogress, 1.0f);
        }
        if (!pInstruments) return NULL;
        InstrumentsIterator = pInstruments->begin();
        for (uint i = 0; InstrumentsIterator != pInstruments->end(); i++) {
            if (i == index) return static_cast<gig::Instrument*>( *InstrumentsIterator );
            InstrumentsIterator++;
        }
        return NULL;
    }

    /** @brief Add a new instrument definition.
     *
     * This will create a new Instrument object for the gig file. You have
     * to call Save() to make this persistent to the file.
     *
     * @returns pointer to new Instrument object
     */
    Instrument* File::AddInstrument() {
       if (!pInstruments) LoadInstruments();
       __ensureMandatoryChunksExist();
       RIFF::List* lstInstruments = pRIFF->GetSubList(LIST_TYPE_LINS);
       RIFF::List* lstInstr = lstInstruments->AddSubList(LIST_TYPE_INS);

       // add mandatory chunks to get the chunks in right order
       lstInstr->AddSubList(LIST_TYPE_INFO);
       lstInstr->AddSubChunk(CHUNK_ID_DLID, 16);

       Instrument* pInstrument = new Instrument(this, lstInstr);
       pInstrument->GenerateDLSID();

       lstInstr->AddSubChunk(CHUNK_ID_INSH, 12);

       // this string is needed for the gig to be loadable in GSt:
       pInstrument->pInfo->Software = "Endless Wave";

       pInstruments->push_back(pInstrument);
       return pInstrument;
    }

    /** @brief Delete an instrument.
     *
     * This will delete the given Instrument object from the gig file. You
     * have to call Save() to make this persistent to the file.
     *
     * @param pInstrument - instrument to delete
     * @throws gig::Exception if given instrument could not be found
     */
    void File::DeleteInstrument(Instrument* pInstrument) {
        if (!pInstruments) throw gig::Exception("Could not delete instrument as there are no instruments");
        InstrumentList::iterator iter = find(pInstruments->begin(), pInstruments->end(), (DLS::Instrument*) pInstrument);
        if (iter == pInstruments->end()) throw gig::Exception("Could not delete instrument, could not find given instrument");
        pInstruments->erase(iter);
        delete pInstrument;
    }

    void File::LoadInstruments() {
        LoadInstruments(NULL);
    }

    void File::LoadInstruments(progress_t* pProgress) {
        if (!pInstruments) pInstruments = new InstrumentList;
        RIFF::List* lstInstruments = pRIFF->GetSubList(LIST_TYPE_LINS);
        if (lstInstruments) {
            int iInstrumentIndex = 0;
            RIFF::List* lstInstr = lstInstruments->GetFirstSubList();
            while (lstInstr) {
                if (lstInstr->GetListType() == LIST_TYPE_INS) {
                    // notify current progress
                    const float localProgress = (float) iInstrumentIndex / (float) Instruments;
                    __notify_progress(pProgress, localProgress);

                    // divide local progress into subprogress for loading current Instrument
                    progress_t subprogress;
                    __divide_progress(pProgress, &subprogress, Instruments, iInstrumentIndex);

                    pInstruments->push_back(new Instrument(this, lstInstr, &subprogress));

                    iInstrumentIndex++;
                }
                lstInstr = lstInstruments->GetNextSubList();
            }
            __notify_progress(pProgress, 1.0); // notify done
        }
    }

    /// Updates the 3crc chunk with the checksum of a sample. The
    /// update is done directly to disk, as this method is called
    /// after File::Save()
    void File::SetSampleChecksum(Sample* pSample, uint32_t crc) {
        RIFF::Chunk* _3crc = pRIFF->GetSubChunk(CHUNK_ID_3CRC);
        if (!_3crc) return;

        // get the index of the sample
        int iWaveIndex = -1;
        File::SampleList::iterator iter = pSamples->begin();
        File::SampleList::iterator end  = pSamples->end();
        for (int index = 0; iter != end; ++iter, ++index) {
            if (*iter == pSample) {
                iWaveIndex = index;
                break;
            }
        }
        if (iWaveIndex < 0) throw gig::Exception("Could not update crc, could not find sample");

        // write the CRC-32 checksum to disk
        _3crc->SetPos(iWaveIndex * 8);
        uint32_t tmp = 1;
        _3crc->WriteUint32(&tmp); // unknown, always 1?
        _3crc->WriteUint32(&crc);
    }

    Group* File::GetFirstGroup() {
        if (!pGroups) LoadGroups();
        // there must always be at least one group
        GroupsIterator = pGroups->begin();
        return *GroupsIterator;
    }

    Group* File::GetNextGroup() {
        if (!pGroups) return NULL;
        ++GroupsIterator;
        return (GroupsIterator == pGroups->end()) ? NULL : *GroupsIterator;
    }

    /**
     * Returns the group with the given index.
     *
     * @param index - number of the sought group (0..n)
     * @returns sought group or NULL if there's no such group
     */
    Group* File::GetGroup(uint index) {
        if (!pGroups) LoadGroups();
        GroupsIterator = pGroups->begin();
        for (uint i = 0; GroupsIterator != pGroups->end(); i++) {
            if (i == index) return *GroupsIterator;
            ++GroupsIterator;
        }
        return NULL;
    }

    Group* File::AddGroup() {
        if (!pGroups) LoadGroups();
        // there must always be at least one group
        __ensureMandatoryChunksExist();
        Group* pGroup = new Group(this, NULL);
        pGroups->push_back(pGroup);
        return pGroup;
    }

    /** @brief Delete a group and its samples.
     *
     * This will delete the given Group object and all the samples that
     * belong to this group from the gig file. You have to call Save() to
     * make this persistent to the file.
     *
     * @param pGroup - group to delete
     * @throws gig::Exception if given group could not be found
     */
    void File::DeleteGroup(Group* pGroup) {
        if (!pGroups) LoadGroups();
        std::list<Group*>::iterator iter = find(pGroups->begin(), pGroups->end(), pGroup);
        if (iter == pGroups->end()) throw gig::Exception("Could not delete group, could not find given group");
        if (pGroups->size() == 1) throw gig::Exception("Cannot delete group, there must be at least one default group!");
        // delete all members of this group
        for (Sample* pSample = pGroup->GetFirstSample(); pSample; pSample = pGroup->GetNextSample()) {
            DeleteSample(pSample);
        }
        // now delete this group object
        pGroups->erase(iter);
        delete pGroup;
    }

    /** @brief Delete a group.
     *
     * This will delete the given Group object from the gig file. All the
     * samples that belong to this group will not be deleted, but instead
     * be moved to another group. You have to call Save() to make this
     * persistent to the file.
     *
     * @param pGroup - group to delete
     * @throws gig::Exception if given group could not be found
     */
    void File::DeleteGroupOnly(Group* pGroup) {
        if (!pGroups) LoadGroups();
        std::list<Group*>::iterator iter = find(pGroups->begin(), pGroups->end(), pGroup);
        if (iter == pGroups->end()) throw gig::Exception("Could not delete group, could not find given group");
        if (pGroups->size() == 1) throw gig::Exception("Cannot delete group, there must be at least one default group!");
        // move all members of this group to another group
        pGroup->MoveAll();
        pGroups->erase(iter);
        delete pGroup;
    }

    void File::LoadGroups() {
        if (!pGroups) pGroups = new std::list<Group*>;
        // try to read defined groups from file
        RIFF::List* lst3gri = pRIFF->GetSubList(LIST_TYPE_3GRI);
        if (lst3gri) {
            RIFF::List* lst3gnl = lst3gri->GetSubList(LIST_TYPE_3GNL);
            if (lst3gnl) {
                RIFF::Chunk* ck = lst3gnl->GetFirstSubChunk();
                while (ck) {
                    if (ck->GetChunkID() == CHUNK_ID_3GNM) {
                        if (pVersion && pVersion->major == 3 &&
                            strcmp(static_cast<char*>(ck->LoadChunkData()), "") == 0) break;

                        pGroups->push_back(new Group(this, ck));
                    }
                    ck = lst3gnl->GetNextSubChunk();
                }
            }
        }
        // if there were no group(s), create at least the mandatory default group
        if (!pGroups->size()) {
            Group* pGroup = new Group(this, NULL);
            pGroup->Name = "Default Group";
            pGroups->push_back(pGroup);
        }
    }

    /**
     * Apply all the gig file's current instruments, samples, groups and settings
     * to the respective RIFF chunks. You have to call Save() to make changes
     * persistent.
     *
     * Usually there is absolutely no need to call this method explicitly.
     * It will be called automatically when File::Save() was called.
     *
     * @throws Exception - on errors
     */
    void File::UpdateChunks() {
        bool newFile = pRIFF->GetSubList(LIST_TYPE_INFO) == NULL;

        b64BitWavePoolOffsets = pVersion && pVersion->major == 3;

        // first update base class's chunks
        DLS::File::UpdateChunks();

        if (newFile) {
            // INFO was added by Resource::UpdateChunks - make sure it
            // is placed first in file
            RIFF::Chunk* info = pRIFF->GetSubList(LIST_TYPE_INFO);
            RIFF::Chunk* first = pRIFF->GetFirstSubChunk();
            if (first != info) {
                pRIFF->MoveSubChunk(info, first);
            }
        }

        // update group's chunks
        if (pGroups) {
            std::list<Group*>::iterator iter = pGroups->begin();
            std::list<Group*>::iterator end  = pGroups->end();
            for (; iter != end; ++iter) {
                (*iter)->UpdateChunks();
            }

            // v3: make sure the file has 128 3gnm chunks
            if (pVersion && pVersion->major == 3) {
                RIFF::List* _3gnl = pRIFF->GetSubList(LIST_TYPE_3GRI)->GetSubList(LIST_TYPE_3GNL);
                RIFF::Chunk* _3gnm = _3gnl->GetFirstSubChunk();
                for (int i = 0 ; i < 128 ; i++) {
                    if (i >= pGroups->size()) ::SaveString(CHUNK_ID_3GNM, _3gnm, _3gnl, "", "", true, 64);
                    if (_3gnm) _3gnm = _3gnl->GetNextSubChunk();
                }
            }
        }

        // update einf chunk

        // The einf chunk contains statistics about the gig file, such
        // as the number of regions and samples used by each
        // instrument. It is divided in equally sized parts, where the
        // first part contains information about the whole gig file,
        // and the rest of the parts map to each instrument in the
        // file.
        //
        // At the end of each part there is a bit map of each sample
        // in the file, where a set bit means that the sample is used
        // by the file/instrument.
        //
        // Note that there are several fields with unknown use. These
        // are set to zero.

        int sublen = pSamples->size() / 8 + 49;
        int einfSize = (Instruments + 1) * sublen;

        RIFF::Chunk* einf = pRIFF->GetSubChunk(CHUNK_ID_EINF);
        if (einf) {
            if (einf->GetSize() != einfSize) {
                einf->Resize(einfSize);
                memset(einf->LoadChunkData(), 0, einfSize);
            }
        } else if (newFile) {
            einf = pRIFF->AddSubChunk(CHUNK_ID_EINF, einfSize);
        }
        if (einf) {
            uint8_t* pData = (uint8_t*) einf->LoadChunkData();

            std::map<gig::Sample*,int> sampleMap;
            int sampleIdx = 0;
            for (Sample* pSample = GetFirstSample(); pSample; pSample = GetNextSample()) {
                sampleMap[pSample] = sampleIdx++;
            }

            int totnbusedsamples = 0;
            int totnbusedchannels = 0;
            int totnbregions = 0;
            int totnbdimregions = 0;
            int totnbloops = 0;
            int instrumentIdx = 0;

            memset(&pData[48], 0, sublen - 48);

            for (Instrument* instrument = GetFirstInstrument() ; instrument ;
                 instrument = GetNextInstrument()) {
                int nbusedsamples = 0;
                int nbusedchannels = 0;
                int nbdimregions = 0;
                int nbloops = 0;

                memset(&pData[(instrumentIdx + 1) * sublen + 48], 0, sublen - 48);

                for (Region* region = instrument->GetFirstRegion() ; region ;
                     region = instrument->GetNextRegion()) {
                    for (int i = 0 ; i < region->DimensionRegions ; i++) {
                        gig::DimensionRegion *d = region->pDimensionRegions[i];
                        if (d->pSample) {
                            int sampleIdx = sampleMap[d->pSample];
                            int byte = 48 + sampleIdx / 8;
                            int bit = 1 << (sampleIdx & 7);
                            if ((pData[(instrumentIdx + 1) * sublen + byte] & bit) == 0) {
                                pData[(instrumentIdx + 1) * sublen + byte] |= bit;
                                nbusedsamples++;
                                nbusedchannels += d->pSample->Channels;

                                if ((pData[byte] & bit) == 0) {
                                    pData[byte] |= bit;
                                    totnbusedsamples++;
                                    totnbusedchannels += d->pSample->Channels;
                                }
                            }
                        }
                        if (d->SampleLoops) nbloops++;
                    }
                    nbdimregions += region->DimensionRegions;
                }
                // first 4 bytes unknown - sometimes 0, sometimes length of einf part
                // store32(&pData[(instrumentIdx + 1) * sublen], sublen);
                store32(&pData[(instrumentIdx + 1) * sublen + 4], nbusedchannels);
                store32(&pData[(instrumentIdx + 1) * sublen + 8], nbusedsamples);
                store32(&pData[(instrumentIdx + 1) * sublen + 12], 1);
                store32(&pData[(instrumentIdx + 1) * sublen + 16], instrument->Regions);
                store32(&pData[(instrumentIdx + 1) * sublen + 20], nbdimregions);
                store32(&pData[(instrumentIdx + 1) * sublen + 24], nbloops);
                // next 8 bytes unknown
                store32(&pData[(instrumentIdx + 1) * sublen + 36], instrumentIdx);
                store32(&pData[(instrumentIdx + 1) * sublen + 40], pSamples->size());
                // next 4 bytes unknown

                totnbregions += instrument->Regions;
                totnbdimregions += nbdimregions;
                totnbloops += nbloops;
                instrumentIdx++;
            }
            // first 4 bytes unknown - sometimes 0, sometimes length of einf part
            // store32(&pData[0], sublen);
            store32(&pData[4], totnbusedchannels);
            store32(&pData[8], totnbusedsamples);
            store32(&pData[12], Instruments);
            store32(&pData[16], totnbregions);
            store32(&pData[20], totnbdimregions);
            store32(&pData[24], totnbloops);
            // next 8 bytes unknown
            // next 4 bytes unknown, not always 0
            store32(&pData[40], pSamples->size());
            // next 4 bytes unknown
        }

        // update 3crc chunk

        // The 3crc chunk contains CRC-32 checksums for the
        // samples. The actual checksum values will be filled in
        // later, by Sample::Write.

        RIFF::Chunk* _3crc = pRIFF->GetSubChunk(CHUNK_ID_3CRC);
        if (_3crc) {
            _3crc->Resize(pSamples->size() * 8);
        } else if (newFile) {
            _3crc = pRIFF->AddSubChunk(CHUNK_ID_3CRC, pSamples->size() * 8);
            _3crc->LoadChunkData();

            // the order of einf and 3crc is not the same in v2 and v3
            if (einf && pVersion && pVersion->major == 3) pRIFF->MoveSubChunk(_3crc, einf);
        }
    }

    /**
     * Enable / disable automatic loading. By default this properyt is
     * enabled and all informations are loaded automatically. However
     * loading all Regions, DimensionRegions and especially samples might
     * take a long time for large .gig files, and sometimes one might only
     * be interested in retrieving very superficial informations like the
     * amount of instruments and their names. In this case one might disable
     * automatic loading to avoid very slow response times.
     *
     * @e CAUTION: by disabling this property many pointers (i.e. sample
     * references) and informations will have invalid or even undefined
     * data! This feature is currently only intended for retrieving very
     * superficial informations in a very fast way. Don't use it to retrieve
     * details like synthesis informations or even to modify .gig files!
     */
    void File::SetAutoLoad(bool b) {
        bAutoLoad = b;
    }

    /**
     * Returns whether automatic loading is enabled.
     * @see SetAutoLoad()
     */
    bool File::GetAutoLoad() {
        return bAutoLoad;
    }



// *************** Exception ***************
// *

    Exception::Exception(String Message) : DLS::Exception(Message) {
    }

    void Exception::PrintMessage() {
        std::cout << "gig::Exception: " << Message << std::endl;
    }


// *************** functions ***************
// *

    /**
     * Returns the name of this C++ library. This is usually "libgig" of
     * course. This call is equivalent to RIFF::libraryName() and
     * DLS::libraryName().
     */
    String libraryName() {
        return PACKAGE;
    }

    /**
     * Returns version of this C++ library. This call is equivalent to
     * RIFF::libraryVersion() and DLS::libraryVersion().
     */
    String libraryVersion() {
        return VERSION;
    }

} // namespace gig
