/***************************************************************************
 *                                                                         *
 *   libgig - C++ cross-platform Gigasampler format file access library    *
 *                                                                         *
 *   Copyright (C) 2003-2012 by Christian Schoenebeck                      *
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

#ifndef __GIG_H__
#define __GIG_H__

#include "DLS.h"

#if WORDS_BIGENDIAN
# define LIST_TYPE_3PRG	0x33707267
# define LIST_TYPE_3EWL	0x3365776C
# define LIST_TYPE_3GRI	0x33677269
# define LIST_TYPE_3GNL	0x33676E6C
# define CHUNK_ID_3GIX	0x33676978
# define CHUNK_ID_3EWA	0x33657761
# define CHUNK_ID_3LNK	0x336C6E6B
# define CHUNK_ID_3EWG	0x33657767
# define CHUNK_ID_EWAV	0x65776176
# define CHUNK_ID_3GNM	0x33676E6D
# define CHUNK_ID_EINF	0x65696E66
# define CHUNK_ID_3CRC	0x33637263
#else  // little endian
# define LIST_TYPE_3PRG	0x67727033
# define LIST_TYPE_3EWL	0x6C776533
# define LIST_TYPE_3GRI	0x69726733
# define LIST_TYPE_3GNL	0x6C6E6733
# define CHUNK_ID_3GIX	0x78696733
# define CHUNK_ID_3EWA	0x61776533
# define CHUNK_ID_3LNK	0x6B6E6C33
# define CHUNK_ID_3EWG	0x67776533
# define CHUNK_ID_EWAV	0x76617765
# define CHUNK_ID_3GNM	0x6D6E6733
# define CHUNK_ID_EINF	0x666E6965
# define CHUNK_ID_3CRC	0x63726333
#endif // WORDS_BIGENDIAN

/** Gigasampler specific classes and definitions */
namespace gig {

    typedef std::string String;

    /** Lower and upper limit of a range. */
    struct range_t {
        uint8_t low;  ///< Low value of range.
        uint8_t high; ///< High value of range.
    };

    /** Pointer address and size of a buffer. */
    struct buffer_t {
        void*         pStart;            ///< Points to the beginning of the buffer.
        unsigned long Size;              ///< Size of the actual data in the buffer in bytes.
        unsigned long NullExtensionSize; ///< The buffer might be bigger than the actual data, if that's the case that unused space at the end of the buffer is filled with NULLs and NullExtensionSize reflects that unused buffer space in bytes. Those NULL extensions are mandatory for differential algorithms that have to take the following data words into account, thus have to access past the buffer's boundary. If you don't know what I'm talking about, just forget this variable. :)
        buffer_t() {
            pStart            = NULL;
            Size              = 0;
            NullExtensionSize = 0;
        }
    };

    /** Standard types of sample loops. */
    typedef enum {
        loop_type_normal        = 0x00000000,  ///< Loop forward (normal)
        loop_type_bidirectional = 0x00000001,  ///< Alternating loop (forward/backward, also known as Ping Pong)
        loop_type_backward      = 0x00000002   ///< Loop backward (reverse)
    } loop_type_t;

    /** Society of Motion Pictures and Television E time format. */
    typedef enum {
        smpte_format_no_offset          = 0x00000000,  ///< no SMPTE offset
        smpte_format_24_frames          = 0x00000018,  ///< 24 frames per second
        smpte_format_25_frames          = 0x00000019,  ///< 25 frames per second
        smpte_format_30_frames_dropping = 0x0000001D,  ///< 30 frames per second with frame dropping (30 drop)
        smpte_format_30_frames          = 0x0000001E   ///< 30 frames per second
    } smpte_format_t;

    /** Defines the shape of a function graph. */
    typedef enum {
        curve_type_nonlinear = 0,
        curve_type_linear    = 1,
        curve_type_special   = 2,
        curve_type_unknown   = 0xffffffff
    } curve_type_t;

    /** Dimensions allow to bypass one of the following controllers. */
    typedef enum {
        dim_bypass_ctrl_none,
        dim_bypass_ctrl_94,   ///< Effect 4 Depth (MIDI Controller 94)
        dim_bypass_ctrl_95    ///< Effect 5 Depth (MIDI Controller 95)
    } dim_bypass_ctrl_t;

    /** Defines how LFO3 is controlled by. */
    typedef enum {
        lfo3_ctrl_internal            = 0x00, ///< Only internally controlled.
        lfo3_ctrl_modwheel            = 0x01, ///< Only controlled by external modulation wheel.
        lfo3_ctrl_aftertouch          = 0x02, ///< Only controlled by aftertouch controller.
        lfo3_ctrl_internal_modwheel   = 0x03, ///< Controlled internally and by external modulation wheel.
        lfo3_ctrl_internal_aftertouch = 0x04  ///< Controlled internally and by aftertouch controller.
    } lfo3_ctrl_t;

    /** Defines how LFO2 is controlled by. */
    typedef enum {
        lfo2_ctrl_internal            = 0x00, ///< Only internally controlled.
        lfo2_ctrl_modwheel            = 0x01, ///< Only controlled by external modulation wheel.
        lfo2_ctrl_foot                = 0x02, ///< Only controlled by external foot controller.
        lfo2_ctrl_internal_modwheel   = 0x03, ///< Controlled internally and by external modulation wheel.
        lfo2_ctrl_internal_foot       = 0x04  ///< Controlled internally and by external foot controller.
    } lfo2_ctrl_t;

    /** Defines how LFO1 is controlled by. */
    typedef enum {
        lfo1_ctrl_internal            = 0x00, ///< Only internally controlled.
        lfo1_ctrl_modwheel            = 0x01, ///< Only controlled by external modulation wheel.
        lfo1_ctrl_breath              = 0x02, ///< Only controlled by external breath controller.
        lfo1_ctrl_internal_modwheel   = 0x03, ///< Controlled internally and by external modulation wheel.
        lfo1_ctrl_internal_breath     = 0x04  ///< Controlled internally and by external breath controller.
    } lfo1_ctrl_t;

    /** Defines how the filter cutoff frequency is controlled by. */
    typedef enum {
        vcf_cutoff_ctrl_none         = 0x00,
        vcf_cutoff_ctrl_none2        = 0x01,  ///< The difference between none and none2 is unknown
        vcf_cutoff_ctrl_modwheel     = 0x81,  ///< Modulation Wheel (MIDI Controller 1)
        vcf_cutoff_ctrl_effect1      = 0x8c,  ///< Effect Controller 1 (Coarse, MIDI Controller 12)
        vcf_cutoff_ctrl_effect2      = 0x8d,  ///< Effect Controller 2 (Coarse, MIDI Controller 13)
        vcf_cutoff_ctrl_breath       = 0x82,  ///< Breath Controller (Coarse, MIDI Controller 2)
        vcf_cutoff_ctrl_foot         = 0x84,  ///< Foot Pedal (Coarse, MIDI Controller 4)
        vcf_cutoff_ctrl_sustainpedal = 0xc0,  ///< Sustain Pedal (MIDI Controller 64)
        vcf_cutoff_ctrl_softpedal    = 0xc3,  ///< Soft Pedal (MIDI Controller 67)
        vcf_cutoff_ctrl_genpurpose7  = 0xd2,  ///< General Purpose Controller 7 (Button, MIDI Controller 82)
        vcf_cutoff_ctrl_genpurpose8  = 0xd3,  ///< General Purpose Controller 8 (Button, MIDI Controller 83)
        vcf_cutoff_ctrl_aftertouch   = 0x80   ///< Key Pressure
    } vcf_cutoff_ctrl_t;

    /** Defines how the filter resonance is controlled by. */
    typedef enum {
        vcf_res_ctrl_none        = 0xffffffff,
        vcf_res_ctrl_genpurpose3 = 0,           ///< General Purpose Controller 3 (Slider, MIDI Controller 18)
        vcf_res_ctrl_genpurpose4 = 1,           ///< General Purpose Controller 4 (Slider, MIDI Controller 19)
        vcf_res_ctrl_genpurpose5 = 2,           ///< General Purpose Controller 5 (Button, MIDI Controller 80)
        vcf_res_ctrl_genpurpose6 = 3            ///< General Purpose Controller 6 (Button, MIDI Controller 81)
    } vcf_res_ctrl_t;

    /**
     * Defines a controller that has a certain contrained influence on a
     * particular synthesis parameter (used to define attenuation controller,
     * EG1 controller and EG2 controller).
     *
     * You should use the respective <i>typedef</i> (means either
     * attenuation_ctrl_t, eg1_ctrl_t or eg2_ctrl_t) in your code!
     */
    struct leverage_ctrl_t {
        typedef enum {
            type_none              = 0x00, ///< No controller defined
            type_channelaftertouch = 0x2f, ///< Channel Key Pressure
            type_velocity          = 0xff, ///< Key Velocity
            type_controlchange     = 0xfe  ///< Ordinary MIDI control change controller, see field 'controller_number'
        } type_t;

        type_t type;              ///< Controller type
        uint   controller_number; ///< MIDI controller number if this controller is a control change controller, 0 otherwise
    };

    /**
     * Defines controller influencing attenuation.
     *
     * @see leverage_ctrl_t
     */
    typedef leverage_ctrl_t attenuation_ctrl_t;

    /**
     * Defines controller influencing envelope generator 1.
     *
     * @see leverage_ctrl_t
     */
    typedef leverage_ctrl_t eg1_ctrl_t;

    /**
     * Defines controller influencing envelope generator 2.
     *
     * @see leverage_ctrl_t
     */
    typedef leverage_ctrl_t eg2_ctrl_t;

    /**
     * Defines the type of dimension, that is how the dimension zones (and
     * thus how the dimension regions are selected by. The number of
     * dimension zones is always a power of two. All dimensions can have up
     * to 32 zones (except the layer dimension with only up to 8 zones and
     * the samplechannel dimension which currently allows only 2 zones).
     */
    typedef enum {
        dimension_none              = 0x00, ///< Dimension not in use.
        dimension_samplechannel     = 0x80, ///< If used sample has more than one channel (thus is not mono).
        dimension_layer             = 0x81, ///< For layering of up to 8 instruments (and eventually crossfading of 2 or 4 layers).
        dimension_velocity          = 0x82, ///< Key Velocity (this is the only dimension in gig2 where the ranges can exactly be defined).
        dimension_channelaftertouch = 0x83, ///< Channel Key Pressure
        dimension_releasetrigger    = 0x84, ///< Special dimension for triggering samples on releasing a key.
        dimension_keyboard          = 0x85, ///< Dimension for keyswitching
        dimension_roundrobin        = 0x86, ///< Different samples triggered each time a note is played, dimension regions selected in sequence
        dimension_random            = 0x87, ///< Different samples triggered each time a note is played, random order
        dimension_smartmidi         = 0x88, ///< For MIDI tools like legato and repetition mode
        dimension_roundrobinkeyboard = 0x89, ///< Different samples triggered each time a note is played, any key advances the counter
        dimension_modwheel          = 0x01, ///< Modulation Wheel (MIDI Controller 1)
        dimension_breath            = 0x02, ///< Breath Controller (Coarse, MIDI Controller 2)
        dimension_foot              = 0x04, ///< Foot Pedal (Coarse, MIDI Controller 4)
        dimension_portamentotime    = 0x05, ///< Portamento Time (Coarse, MIDI Controller 5)
        dimension_effect1           = 0x0c, ///< Effect Controller 1 (Coarse, MIDI Controller 12)
        dimension_effect2           = 0x0d, ///< Effect Controller 2 (Coarse, MIDI Controller 13)
        dimension_genpurpose1       = 0x10, ///< General Purpose Controller 1 (Slider, MIDI Controller 16)
        dimension_genpurpose2       = 0x11, ///< General Purpose Controller 2 (Slider, MIDI Controller 17)
        dimension_genpurpose3       = 0x12, ///< General Purpose Controller 3 (Slider, MIDI Controller 18)
        dimension_genpurpose4       = 0x13, ///< General Purpose Controller 4 (Slider, MIDI Controller 19)
        dimension_sustainpedal      = 0x40, ///< Sustain Pedal (MIDI Controller 64)
        dimension_portamento        = 0x41, ///< Portamento (MIDI Controller 65)
        dimension_sostenutopedal    = 0x42, ///< Sostenuto Pedal (MIDI Controller 66)
        dimension_softpedal         = 0x43, ///< Soft Pedal (MIDI Controller 67)
        dimension_genpurpose5       = 0x30, ///< General Purpose Controller 5 (Button, MIDI Controller 80)
        dimension_genpurpose6       = 0x31, ///< General Purpose Controller 6 (Button, MIDI Controller 81)
        dimension_genpurpose7       = 0x32, ///< General Purpose Controller 7 (Button, MIDI Controller 82)
        dimension_genpurpose8       = 0x33, ///< General Purpose Controller 8 (Button, MIDI Controller 83)
        dimension_effect1depth      = 0x5b, ///< Effect 1 Depth (MIDI Controller 91)
        dimension_effect2depth      = 0x5c, ///< Effect 2 Depth (MIDI Controller 92)
        dimension_effect3depth      = 0x5d, ///< Effect 3 Depth (MIDI Controller 93)
        dimension_effect4depth      = 0x5e, ///< Effect 4 Depth (MIDI Controller 94)
        dimension_effect5depth      = 0x5f  ///< Effect 5 Depth (MIDI Controller 95)
    } dimension_t;

    /**
     * Intended for internal usage: will be used to convert a dimension value
     * into the corresponding dimension bit number.
     */
    typedef enum {
        split_type_normal,         ///< dimension value between 0-127
        split_type_bit             ///< dimension values are already the sought bit number
    } split_type_t;

    /** General dimension definition. */
    struct dimension_def_t {
        dimension_t  dimension;  ///< Specifies which source (usually a MIDI controller) is associated with the dimension.
        uint8_t      bits;       ///< Number of "bits" (1 bit = 2 splits/zones, 2 bit = 4 splits/zones, 3 bit = 8 splits/zones,...).
        uint8_t      zones;      ///< Number of zones the dimension has.
        split_type_t split_type; ///< Intended for internal usage: will be used to convert a dimension value into the corresponding dimension bit number.
        float        zone_size;  ///< Intended for internal usage: reflects the size of each zone (128/zones) for normal split types only, 0 otherwise.
    };

    /** Defines which frequencies are filtered by the VCF. */
    typedef enum {
        vcf_type_lowpass      = 0x00,
        vcf_type_lowpassturbo = 0xff, ///< More poles than normal lowpass
        vcf_type_bandpass     = 0x01,
        vcf_type_highpass     = 0x02,
        vcf_type_bandreject   = 0x03
    } vcf_type_t;

    /**
     * Defines the envelope of a crossfade.
     *
     * Note: The default value for crossfade points is 0,0,0,0. Layers with
     * such a default value should be treated as if they would not have a
     * crossfade.
     */
    struct crossfade_t {
        #if WORDS_BIGENDIAN
        uint8_t out_end;    ///< End postition of fade out.
        uint8_t out_start;  ///< Start position of fade out.
        uint8_t in_end;     ///< End position of fade in.
        uint8_t in_start;   ///< Start position of fade in.
        #else // little endian
        uint8_t in_start;   ///< Start position of fade in.
        uint8_t in_end;     ///< End position of fade in.
        uint8_t out_start;  ///< Start position of fade out.
        uint8_t out_end;    ///< End postition of fade out.
        #endif // WORDS_BIGENDIAN
    };

    /** Reflects the current playback state for a sample. */
    struct playback_state_t {
        unsigned long position;          ///< Current position within the sample.
        bool          reverse;           ///< If playback direction is currently backwards (in case there is a pingpong or reverse loop defined).
        unsigned long loop_cycles_left;  ///< How many times the loop has still to be passed, this value will be decremented with each loop cycle.
    };

    /**
     * @brief Used for indicating the progress of a certain task.
     *
     * The function pointer argument has to be supplied with a valid
     * function of the given signature which will then be called on
     * progress changes. An equivalent progress_t structure will be passed
     * back as argument to the callback function on each progress change.
     * The factor field of the supplied progress_t structure will then
     * reflect the current progress as value between 0.0 and 1.0. You might
     * want to use the custom field for data needed in your callback
     * function.
     */
    struct progress_t {
        void (*callback)(progress_t*); ///< Callback function pointer which has to be assigned to a function for progress notification.
        float factor;                  ///< Reflects current progress as value between 0.0 and 1.0.
        void* custom;                  ///< This pointer can be used for arbitrary data.
        float __range_min;             ///< Only for internal usage, do not modify!
        float __range_max;             ///< Only for internal usage, do not modify!
        progress_t();
    };

    // just symbol prototyping
    class File;
    class Instrument;
    class Sample;
    class Region;
    class Group;

    /** @brief Encapsulates articulation information of a dimension region.
     *
     *  Every Gigasampler Instrument has at least one dimension region
     *  (exactly then when it has no dimension defined).
     *
     *  Gigasampler provides three Envelope Generators and Low Frequency
     *  Oscillators:
     *
     *  - EG1 and LFO1, both controlling sample amplitude
     *  - EG2 and LFO2, both controlling filter cutoff frequency
     *  - EG3 and LFO3, both controlling sample pitch
     */
    class DimensionRegion : protected DLS::Sampler {
        public:
            uint8_t            VelocityUpperLimit;            ///< Defines the upper velocity value limit of a velocity split (only if an user defined limit was set, thus a value not equal to 128/NumberOfSplits, else this value is 0). Only for gig2, otherwise the DimensionUpperLimts are used instead.
            Sample*            pSample;                       ///< Points to the Sample which is assigned to the dimension region.
            // Sample Amplitude EG/LFO
            uint16_t           EG1PreAttack;                  ///< Preattack value of the sample amplitude EG (0 - 1000 permille).
            double             EG1Attack;                     ///< Attack time of the sample amplitude EG (0.000 - 60.000s).
            double             EG1Decay1;                     ///< Decay time of the sample amplitude EG (0.000 - 60.000s).
            double             EG1Decay2;                     ///< Only if <i>EG1InfiniteSustain == false</i>: 2nd decay stage time of the sample amplitude EG (0.000 - 60.000s).
            bool               EG1InfiniteSustain;            ///< If <i>true</i>, instead of going into Decay2 phase, Decay1 level will be hold until note will be released.
            uint16_t           EG1Sustain;                    ///< Sustain value of the sample amplitude EG (0 - 1000 permille).
            double             EG1Release;                    ///< Release time of the sample amplitude EG (0.000 - 60.000s).
            bool               EG1Hold;                       ///< If <i>true</i>, Decay1 stage should be postponed until the sample reached the sample loop start.
            eg1_ctrl_t         EG1Controller;                 ///< MIDI Controller which has influence on sample amplitude EG parameters (attack, decay, release).
            bool               EG1ControllerInvert;           ///< Invert values coming from defined EG1 controller.
            uint8_t            EG1ControllerAttackInfluence;  ///< Amount EG1 Controller has influence on the EG1 Attack time (0 - 3, where 0 means off).
            uint8_t            EG1ControllerDecayInfluence;   ///< Amount EG1 Controller has influence on the EG1 Decay time (0 - 3, where 0 means off).
            uint8_t            EG1ControllerReleaseInfluence; ///< Amount EG1 Controller has influence on the EG1 Release time (0 - 3, where 0 means off).
            double             LFO1Frequency;                 ///< Frequency of the sample amplitude LFO (0.10 - 10.00 Hz).
            uint16_t           LFO1InternalDepth;             ///< Firm pitch of the sample amplitude LFO (0 - 1200 cents).
            uint16_t           LFO1ControlDepth;              ///< Controller depth influencing sample amplitude LFO pitch (0 - 1200 cents).
            lfo1_ctrl_t        LFO1Controller;                ///< MIDI Controller which controls sample amplitude LFO.
            bool               LFO1FlipPhase;                 ///< Inverts phase of the sample amplitude LFO wave.
            bool               LFO1Sync;                      ///< If set to <i>true</i> only one LFO should be used for all voices.
            // Filter Cutoff Frequency EG/LFO
            uint16_t           EG2PreAttack;                  ///< Preattack value of the filter cutoff EG (0 - 1000 permille).
            double             EG2Attack;                     ///< Attack time of the filter cutoff EG (0.000 - 60.000s).
            double             EG2Decay1;                     ///< Decay time of the filter cutoff EG (0.000 - 60.000s).
            double             EG2Decay2;                     ///< Only if <i>EG2InfiniteSustain == false</i>: 2nd stage decay time of the filter cutoff EG (0.000 - 60.000s).
            bool               EG2InfiniteSustain;            ///< If <i>true</i>, instead of going into Decay2 phase, Decay1 level will be hold until note will be released.
            uint16_t           EG2Sustain;                    ///< Sustain value of the filter cutoff EG (0 - 1000 permille).
            double             EG2Release;                    ///< Release time of the filter cutoff EG (0.000 - 60.000s).
            eg2_ctrl_t         EG2Controller;                 ///< MIDI Controller which has influence on filter cutoff EG parameters (attack, decay, release).
            bool               EG2ControllerInvert;           ///< Invert values coming from defined EG2 controller.
            uint8_t            EG2ControllerAttackInfluence;  ///< Amount EG2 Controller has influence on the EG2 Attack time (0 - 3, where 0 means off).
            uint8_t            EG2ControllerDecayInfluence;   ///< Amount EG2 Controller has influence on the EG2 Decay time (0 - 3, where 0 means off).
            uint8_t            EG2ControllerReleaseInfluence; ///< Amount EG2 Controller has influence on the EG2 Release time (0 - 3, where 0 means off).
            double             LFO2Frequency;                 ///< Frequency of the filter cutoff LFO (0.10 - 10.00 Hz).
            uint16_t           LFO2InternalDepth;             ///< Firm pitch of the filter cutoff LFO (0 - 1200 cents).
            uint16_t           LFO2ControlDepth;              ///< Controller depth influencing filter cutoff LFO pitch (0 - 1200).
            lfo2_ctrl_t        LFO2Controller;                ///< MIDI Controlle which controls the filter cutoff LFO.
            bool               LFO2FlipPhase;                 ///< Inverts phase of the filter cutoff LFO wave.
            bool               LFO2Sync;                      ///< If set to <i>true</i> only one LFO should be used for all voices.
            // Sample Pitch EG/LFO
            double             EG3Attack;                     ///< Attack time of the sample pitch EG (0.000 - 10.000s).
            int16_t            EG3Depth;                      ///< Depth of the sample pitch EG (-1200 - +1200).
            double             LFO3Frequency;                 ///< Frequency of the sample pitch LFO (0.10 - 10.00 Hz).
            int16_t            LFO3InternalDepth;             ///< Firm depth of the sample pitch LFO (-1200 - +1200 cents).
            int16_t            LFO3ControlDepth;              ///< Controller depth of the sample pitch LFO (-1200 - +1200 cents).
            lfo3_ctrl_t        LFO3Controller;                ///< MIDI Controller which controls the sample pitch LFO.
            bool               LFO3Sync;                      ///< If set to <i>true</i> only one LFO should be used for all voices.
            // Filter
            bool               VCFEnabled;                    ///< If filter should be used.
            vcf_type_t         VCFType;                       ///< Defines the general filter characteristic (lowpass, highpass, bandpass, etc.).
            vcf_cutoff_ctrl_t  VCFCutoffController;           ///< Specifies which external controller has influence on the filter cutoff frequency. @deprecated Don't alter directly, use SetVCFCutoffController() instead!
            bool               VCFCutoffControllerInvert;     ///< Inverts values coming from the defined cutoff controller
            uint8_t            VCFCutoff;                     ///< Max. cutoff frequency.
            curve_type_t       VCFVelocityCurve;              ///< Defines a transformation curve for the incoming velocity values, affecting the VCF. @deprecated Don't alter directly, use SetVCFVelocityCurve() instead!
            uint8_t            VCFVelocityScale;              ///< (0-127) Amount velocity controls VCF cutoff frequency (only if no other VCF cutoff controller is defined, otherwise this is the minimum cutoff). @deprecated Don't alter directly, use SetVCFVelocityScale() instead!
            uint8_t            VCFVelocityDynamicRange;       ///< 0x04 = lowest, 0x00 = highest . @deprecated Don't alter directly, use SetVCFVelocityDynamicRange() instead!
            uint8_t            VCFResonance;                  ///< Firm internal filter resonance weight.
            bool               VCFResonanceDynamic;           ///< If <i>true</i>: Increases the resonance Q according to changes of controllers that actually control the VCF cutoff frequency (EG2, ext. VCF MIDI controller).
            vcf_res_ctrl_t     VCFResonanceController;        ///< Specifies which external controller has influence on the filter resonance Q.
            bool               VCFKeyboardTracking;           ///< If <i>true</i>: VCF cutoff frequence will be dependend to the note key position relative to the defined breakpoint value.
            uint8_t            VCFKeyboardTrackingBreakpoint; ///< See VCFKeyboardTracking (0 - 127).
            // Key Velocity Transformations
            curve_type_t       VelocityResponseCurve;         ///< Defines a transformation curve to the incoming velocity values affecting amplitude (usually you don't have to interpret this parameter, use GetVelocityAttenuation() instead). @deprecated Don't alter directly, use SetVelocityResponseCurve() instead!
            uint8_t            VelocityResponseDepth;         ///< Dynamic range of velocity affecting amplitude (0 - 4) (usually you don't have to interpret this parameter, use GetVelocityAttenuation() instead). @deprecated Don't alter directly, use SetVelocityResponseDepth() instead!
            uint8_t            VelocityResponseCurveScaling;  ///< 0 - 127 (usually you don't have to interpret this parameter, use GetVelocityAttenuation() instead). @deprecated Don't alter directly, use SetVelocityResponseCurveScaling() instead!
            curve_type_t       ReleaseVelocityResponseCurve;  ///< Defines a transformation curve to the incoming release veloctiy values affecting envelope times. @deprecated Don't alter directly, use SetReleaseVelocityResponseCurve() instead!
            uint8_t            ReleaseVelocityResponseDepth;  ///< Dynamic range of release velocity affecting envelope time (0 - 4). @deprecated Don't alter directly, use SetReleaseVelocityResponseDepth() instead!
            uint8_t            ReleaseTriggerDecay;           ///< 0 - 8
            // Mix / Layer
            crossfade_t        Crossfade;
            bool               PitchTrack;                    ///< If <i>true</i>: sample will be pitched according to the key position (this will be disabled for drums for example).
            dim_bypass_ctrl_t  DimensionBypass;               ///< If defined, the MIDI controller can switch on/off the dimension in realtime.
            int8_t             Pan;                           ///< Panorama / Balance (-64..0..63 <-> left..middle..right)
            bool               SelfMask;                      ///< If <i>true</i>: high velocity notes will stop low velocity notes at the same note, with that you can save voices that wouldn't be audible anyway.
            attenuation_ctrl_t AttenuationController;         ///< MIDI Controller which has influence on the volume level of the sample (or entire sample group).
            bool               InvertAttenuationController;   ///< Inverts the values coming from the defined Attenuation Controller.
            uint8_t            AttenuationControllerThreshold;///< 0-127
            uint8_t            ChannelOffset;                 ///< Audio output where the audio signal of the dimension region should be routed to (0 - 9).
            bool               SustainDefeat;                 ///< If <i>true</i>: Sustain pedal will not hold a note.
            bool               MSDecode;                      ///< Gigastudio flag: defines if Mid Side Recordings should be decoded.
            uint16_t           SampleStartOffset;             ///< Number of samples the sample start should be moved (0 - 2000).
            double             SampleAttenuation;             ///< Sample volume (calculated from DLS::Sampler::Gain)
            uint8_t            DimensionUpperLimits[8];       ///< gig3: defines the upper limit of the dimension values for this dimension region

            // derived attributes from DLS::Sampler
            using DLS::Sampler::UnityNote;
            using DLS::Sampler::FineTune;
            using DLS::Sampler::Gain;
            using DLS::Sampler::SampleLoops;
            using DLS::Sampler::pSampleLoops;

            // own methods
            double GetVelocityAttenuation(uint8_t MIDIKeyVelocity);
            double GetVelocityRelease(uint8_t MIDIKeyVelocity);
            double GetVelocityCutoff(uint8_t MIDIKeyVelocity);
            void SetVelocityResponseCurve(curve_type_t curve);
            void SetVelocityResponseDepth(uint8_t depth);
            void SetVelocityResponseCurveScaling(uint8_t scaling);
            void SetReleaseVelocityResponseCurve(curve_type_t curve);
            void SetReleaseVelocityResponseDepth(uint8_t depth);
            void SetVCFCutoffController(vcf_cutoff_ctrl_t controller);
            void SetVCFVelocityCurve(curve_type_t curve);
            void SetVCFVelocityDynamicRange(uint8_t range);
            void SetVCFVelocityScale(uint8_t scaling);
            Region* GetParent() const;
            // derived methods
            using DLS::Sampler::AddSampleLoop;
            using DLS::Sampler::DeleteSampleLoop;
            // overridden methods
            virtual void SetGain(int32_t gain);
            virtual void UpdateChunks();
        protected:
            uint8_t* VelocityTable; ///< For velocity dimensions with custom defined zone ranges only: used for fast converting from velocity MIDI value to dimension bit number.
            DimensionRegion(Region* pParent, RIFF::List* _3ewl);
            DimensionRegion(RIFF::List* _3ewl, const DimensionRegion& src);
           ~DimensionRegion();
            friend class Region;
        private:
            typedef enum { ///< Used to decode attenuation, EG1 and EG2 controller
                _lev_ctrl_none              = 0x00,
                _lev_ctrl_modwheel          = 0x03, ///< Modulation Wheel (MIDI Controller 1)
                _lev_ctrl_breath            = 0x05, ///< Breath Controller (Coarse, MIDI Controller 2)
                _lev_ctrl_foot              = 0x07, ///< Foot Pedal (Coarse, MIDI Controller 4)
                _lev_ctrl_effect1           = 0x0d, ///< Effect Controller 1 (Coarse, MIDI Controller 12)
                _lev_ctrl_effect2           = 0x0f, ///< Effect Controller 2 (Coarse, MIDI Controller 13)
                _lev_ctrl_genpurpose1       = 0x11, ///< General Purpose Controller 1 (Slider, MIDI Controller 16)
                _lev_ctrl_genpurpose2       = 0x13, ///< General Purpose Controller 2 (Slider, MIDI Controller 17)
                _lev_ctrl_genpurpose3       = 0x15, ///< General Purpose Controller 3 (Slider, MIDI Controller 18)
                _lev_ctrl_genpurpose4       = 0x17, ///< General Purpose Controller 4 (Slider, MIDI Controller 19)
                _lev_ctrl_portamentotime    = 0x0b, ///< Portamento Time (Coarse, MIDI Controller 5)
                _lev_ctrl_sustainpedal      = 0x01, ///< Sustain Pedal (MIDI Controller 64)
                _lev_ctrl_portamento        = 0x19, ///< Portamento (MIDI Controller 65)
                _lev_ctrl_sostenutopedal    = 0x1b, ///< Sostenuto Pedal (MIDI Controller 66)
                _lev_ctrl_softpedal         = 0x09, ///< Soft Pedal (MIDI Controller 67)
                _lev_ctrl_genpurpose5       = 0x1d, ///< General Purpose Controller 5 (Button, MIDI Controller 80)
                _lev_ctrl_genpurpose6       = 0x1f, ///< General Purpose Controller 6 (Button, MIDI Controller 81)
                _lev_ctrl_genpurpose7       = 0x21, ///< General Purpose Controller 7 (Button, MIDI Controller 82)
                _lev_ctrl_genpurpose8       = 0x23, ///< General Purpose Controller 8 (Button, MIDI Controller 83)
                _lev_ctrl_effect1depth      = 0x25, ///< Effect 1 Depth (MIDI Controller 91)
                _lev_ctrl_effect2depth      = 0x27, ///< Effect 2 Depth (MIDI Controller 92)
                _lev_ctrl_effect3depth      = 0x29, ///< Effect 3 Depth (MIDI Controller 93)
                _lev_ctrl_effect4depth      = 0x2b, ///< Effect 4 Depth (MIDI Controller 94)
                _lev_ctrl_effect5depth      = 0x2d, ///< Effect 5 Depth (MIDI Controller 95)
                _lev_ctrl_channelaftertouch = 0x2f, ///< Channel Key Pressure
                _lev_ctrl_velocity          = 0xff  ///< Key Velocity
            } _lev_ctrl_t;
            typedef std::map<uint32_t, double*> VelocityTableMap;

            static uint              Instances;                  ///< Number of DimensionRegion instances.
            static VelocityTableMap* pVelocityTables;            ///< Contains the tables corresponding to the various velocity parameters (VelocityResponseCurve and VelocityResponseDepth).
            double*                  pVelocityAttenuationTable;  ///< Points to the velocity table corresponding to the velocity parameters of this DimensionRegion.
            double*                  pVelocityReleaseTable;      ///< Points to the velocity table corresponding to the release velocity parameters of this DimensionRegion
            double*                  pVelocityCutoffTable;       ///< Points to the velocity table corresponding to the filter velocity parameters of this DimensionRegion
            Region*                  pRegion;

            leverage_ctrl_t DecodeLeverageController(_lev_ctrl_t EncodedController);
            _lev_ctrl_t     EncodeLeverageController(leverage_ctrl_t DecodedController);
            double* GetReleaseVelocityTable(curve_type_t releaseVelocityResponseCurve, uint8_t releaseVelocityResponseDepth);
            double* GetCutoffVelocityTable(curve_type_t vcfVelocityCurve, uint8_t vcfVelocityDynamicRange, uint8_t vcfVelocityScale, vcf_cutoff_ctrl_t vcfCutoffController);
            double* GetVelocityTable(curve_type_t curveType, uint8_t depth, uint8_t scaling);
            double* CreateVelocityTable(curve_type_t curveType, uint8_t depth, uint8_t scaling);
    };

    /** @brief Encapsulates sample waves used for playback.
     *
     * In case you created a new sample with File::AddSample(), you should
     * first update all attributes with the desired meta informations
     * (amount of channels, bit depth, sample rate, etc.), then call
     * Resize() with the desired sample size, followed by File::Save(), this
     * will create the mandatory RIFF chunk which will hold the sample wave
     * data and / or resize the file so you will be able to Write() the
     * sample data directly to disk.
     *
     * @e Caution: for gig synthesis, most looping relevant information are
     * retrieved from the respective DimensionRegon instead from the Sample
     * itself. This was made for allowing different loop definitions for the
     * same sample under different conditions.
     */
    class Sample : public DLS::Sample {
        public:
            uint32_t       Manufacturer;      ///< Specifies the MIDI Manufacturer's Association (MMA) Manufacturer code for the sampler intended to receive this file's waveform. If no particular manufacturer is to be specified, a value of 0 should be used.
            uint32_t       Product;           ///< Specifies the MIDI model ID defined by the manufacturer corresponding to the Manufacturer field. If no particular manufacturer's product is to be specified, a value of 0 should be used.
            uint32_t       SamplePeriod;      ///< Specifies the duration of time that passes during the playback of one sample in nanoseconds (normally equal to 1 / Samples Per Second, where Samples Per Second is the value found in the format chunk), don't bother to update this attribute, it won't be saved.
            uint32_t       MIDIUnityNote;     ///< Specifies the musical note at which the sample will be played at it's original sample rate.
            uint32_t       FineTune;          ///< Specifies the fraction of a semitone up from the specified MIDI unity note field. A value of 0x80000000 means 1/2 semitone (50 cents) and a value of 0x00000000 means no fine tuning between semitones.
            smpte_format_t SMPTEFormat;       ///< Specifies the Society of Motion Pictures and Television E time format used in the following <i>SMPTEOffset</i> field. If a value of 0 is set, <i>SMPTEOffset</i> should also be set to 0.
            uint32_t       SMPTEOffset;       ///< The SMPTE Offset value specifies the time offset to be used for the synchronization / calibration to the first sample in the waveform. This value uses a format of 0xhhmmssff where hh is a signed value that specifies the number of hours (-23 to 23), mm is an unsigned value that specifies the number of minutes (0 to 59), ss is an unsigned value that specifies the number of seconds (0 to 59) and ff is an unsigned value that specifies the number of frames (0 to -1).
            uint32_t       Loops;             ///< @e Caution: Use the respective field in the DimensionRegion instead of this one! (Intended purpose: Number of defined sample loops. So far only seen single loops in gig files - please report if you encounter more!)
            uint32_t       LoopID;            ///< Specifies the unique ID that corresponds to one of the defined cue points in the cue point list (only if Loops > 0), as the Gigasampler format only allows one loop definition at the moment, this attribute isn't really useful for anything.
            loop_type_t    LoopType;          ///< @e Caution: Use the respective field in the DimensionRegion instead of this one! (Intended purpose: The type field defines how the waveform samples will be looped.)
            uint32_t       LoopStart;         ///< @e Caution: Use the respective field in the DimensionRegion instead of this one! (Intended purpose: The start value specifies the offset [in sample points] in the waveform data of the first sample to be played in the loop [only if Loops > 0].)
            uint32_t       LoopEnd;           ///< @e Caution: Use the respective field in the DimensionRegion instead of this one! (Intended purpose: The end value specifies the offset [in sample points] in the waveform data which represents the end of the loop [only if Loops > 0].)
            uint32_t       LoopSize;          ///< @e Caution: Use the respective fields in the DimensionRegion instead of this one! (Intended purpose: Length of the looping area [in sample points] which is equivalent to @code LoopEnd - LoopStart @endcode.)
            uint32_t       LoopFraction;      ///< The fractional value specifies a fraction of a sample at which to loop. This allows a loop to be fine tuned at a resolution greater than one sample. A value of 0 means no fraction, a value of 0x80000000 means 1/2 of a sample length. 0xFFFFFFFF is the smallest fraction of a sample that can be represented.
            uint32_t       LoopPlayCount;     ///< Number of times the loop should be played (a value of 0 = infinite).
            bool           Compressed;        ///< If the sample wave is compressed (probably just interesting for instrument and sample editors, as this library already handles the decompression in it's sample access methods anyway).
            uint32_t       TruncatedBits;     ///< For 24-bit compressed samples only: number of bits truncated during compression (0, 4 or 6)
            bool           Dithered;          ///< For 24-bit compressed samples only: if dithering was used during compression with bit reduction

            // own methods
            buffer_t      LoadSampleData();
            buffer_t      LoadSampleData(unsigned long SampleCount);
            buffer_t      LoadSampleDataWithNullSamplesExtension(uint NullSamplesCount);
            buffer_t      LoadSampleDataWithNullSamplesExtension(unsigned long SampleCount, uint NullSamplesCount);
            buffer_t      GetCache();
            // own static methods
            static buffer_t CreateDecompressionBuffer(unsigned long MaxReadSize);
            static void     DestroyDecompressionBuffer(buffer_t& DecompressionBuffer);
            // overridden methods
            void          ReleaseSampleData();
            void          Resize(int iNewSize);
            unsigned long SetPos(unsigned long SampleCount, RIFF::stream_whence_t Whence = RIFF::stream_start);
            unsigned long GetPos();
            unsigned long Read(void* pBuffer, unsigned long SampleCount, buffer_t* pExternalDecompressionBuffer = NULL);
            unsigned long ReadAndLoop(void* pBuffer, unsigned long SampleCount, playback_state_t* pPlaybackState, DimensionRegion* pDimRgn, buffer_t* pExternalDecompressionBuffer = NULL);
            unsigned long Write(void* pBuffer, unsigned long SampleCount);
            Group*        GetGroup() const;
            virtual void  UpdateChunks();
        protected:
            static unsigned int  Instances;               ///< Number of instances of class Sample.
            static buffer_t      InternalDecompressionBuffer; ///< Buffer used for decompression as well as for truncation of 24 Bit -> 16 Bit samples.
            Group*               pGroup;                  ///< pointer to the Group this sample belongs to (always not-NULL)
            unsigned long        FrameOffset;             ///< Current offset (sample points) in current sample frame (for decompression only).
            unsigned long*       FrameTable;              ///< For positioning within compressed samples only: stores the offset values for each frame.
            unsigned long        SamplePos;               ///< For compressed samples only: stores the current position (in sample points).
            unsigned long        SamplesInLastFrame;      ///< For compressed samples only: length of the last sample frame.
            unsigned long        WorstCaseFrameSize;      ///< For compressed samples only: size (in bytes) of the largest possible sample frame.
            unsigned long        SamplesPerFrame;         ///< For compressed samples only: number of samples in a full sample frame.
            buffer_t             RAMCache;                ///< Buffers samples (already uncompressed) in RAM.
            unsigned long        FileNo;                  ///< File number (> 0 when sample is stored in an extension file, 0 when it's in the gig)
            RIFF::Chunk*         pCk3gix;
            RIFF::Chunk*         pCkSmpl;
            uint32_t             crc;                     ///< CRC-32 checksum of the raw sample data

            Sample(File* pFile, RIFF::List* waveList, unsigned long WavePoolOffset, unsigned long fileNo = 0);
           ~Sample();

            // Guess size (in bytes) of a compressed sample
            inline unsigned long GuessSize(unsigned long samples) {
                // 16 bit: assume all frames are compressed - 1 byte
                // per sample and 5 bytes header per 2048 samples

                // 24 bit: assume next best compression rate - 1.5
                // bytes per sample and 13 bytes header per 256
                // samples
                const unsigned long size =
                    BitDepth == 24 ? samples + (samples >> 1) + (samples >> 8) * 13
                                   : samples + (samples >> 10) * 5;
                // Double for stereo and add one worst case sample
                // frame
                return (Channels == 2 ? size << 1 : size) + WorstCaseFrameSize;
            }

            // Worst case amount of sample points that can be read with the
            // given decompression buffer.
            inline unsigned long WorstCaseMaxSamples(buffer_t* pDecompressionBuffer) {
                return (unsigned long) ((float)pDecompressionBuffer->Size / (float)WorstCaseFrameSize * (float)SamplesPerFrame);
            }
        private:
            void ScanCompressedSample();
            friend class File;
            friend class Region;
            friend class Group; // allow to modify protected member pGroup
    };

    // TODO: <3dnl> list not used yet - not important though (just contains optional descriptions for the dimensions)
    /** Defines <i>Region</i> information of an <i>Instrument</i>. */
    class Region : public DLS::Region {
        public:
            unsigned int            Dimensions;               ///< Number of defined dimensions, do not alter!
            dimension_def_t         pDimensionDefinitions[8]; ///< Defines the five (gig2) or eight (gig3) possible dimensions (the dimension's controller and number of bits/splits). Use AddDimension() and DeleteDimension() to create a new dimension or delete an existing one.
            uint32_t                DimensionRegions;         ///< Total number of DimensionRegions this Region contains, do not alter!
            DimensionRegion*        pDimensionRegions[256];   ///< Pointer array to the 32 (gig2) or 256 (gig3) possible dimension regions (reflects NULL for dimension regions not in use). Avoid to access the array directly and better use GetDimensionRegionByValue() instead, but of course in some cases it makes sense to use the array (e.g. iterating through all DimensionRegions). Use AddDimension() and DeleteDimension() to create a new dimension or delete an existing one (which will create or delete the respective dimension region(s) automatically).
            unsigned int            Layers;                   ///< Amount of defined layers (1 - 32). A value of 1 actually means no layering, a value > 1 means there is Layer dimension. The same information can of course also be obtained by accessing pDimensionDefinitions. Do not alter this value!

            // own methods
            DimensionRegion* GetDimensionRegionByValue(const uint DimValues[8]);
            DimensionRegion* GetDimensionRegionByBit(const uint8_t DimBits[8]);
            Sample*          GetSample();
            void             AddDimension(dimension_def_t* pDimDef);
            void             DeleteDimension(dimension_def_t* pDimDef);
            // overridden methods
            virtual void     SetKeyRange(uint16_t Low, uint16_t High);
            virtual void     UpdateChunks();
        protected:
            Region(Instrument* pInstrument, RIFF::List* rgnList);
            void LoadDimensionRegions(RIFF::List* rgn);
            void UpdateVelocityTable();
            Sample* GetSampleFromWavePool(unsigned int WavePoolTableIndex, progress_t* pProgress = NULL);
           ~Region();
            friend class Instrument;
    };

    /** Abstract base class for all MIDI rules. */
    class MidiRule {
        public:
            virtual ~MidiRule() { }
    };

    /** MIDI rule for triggering notes by control change events. */
    class MidiRuleCtrlTrigger : public MidiRule {
        public:
            uint8_t ControllerNumber;   ///< MIDI controller number.
            uint8_t Triggers;           ///< Number of triggers.
            struct trigger_t {
                uint8_t TriggerPoint;   ///< The CC value to pass for the note to be triggered.
                bool    Descending;     ///< If the change in CC value should be downwards.
                uint8_t VelSensitivity; ///< How sensitive the velocity should be to the speed of the controller change.
                uint8_t Key;            ///< Key to trigger.
                bool    NoteOff;        ///< If a note off should be triggered instead of a note on.
                uint8_t Velocity;       ///< Velocity of the note to trigger. 255 means that velocity should depend on the speed of the controller change.
                bool    OverridePedal;  ///< If a note off should be triggered even if the sustain pedal is down.
            } pTriggers[32];

        protected:
            MidiRuleCtrlTrigger(RIFF::Chunk* _3ewg);
            friend class Instrument;
    };

    /** Provides all neccessary information for the synthesis of an <i>Instrument</i>. */
    class Instrument : protected DLS::Instrument {
        public:
            // derived attributes from DLS::Resource
            using DLS::Resource::pInfo;
            using DLS::Resource::pDLSID;
            // derived attributes from DLS::Instrument
            using DLS::Instrument::IsDrum;
            using DLS::Instrument::MIDIBank;
            using DLS::Instrument::MIDIBankCoarse;
            using DLS::Instrument::MIDIBankFine;
            using DLS::Instrument::MIDIProgram;
            using DLS::Instrument::Regions;
            // own attributes
            int32_t   Attenuation;       ///< in dB
            uint16_t  EffectSend;
            int16_t   FineTune;          ///< in cents
            uint16_t  PitchbendRange;    ///< Number of semitones pitchbend controller can pitch (default is 2).
            bool      PianoReleaseMode;
            range_t   DimensionKeyRange; ///< 0-127 (where 0 means C1 and 127 means G9)


            // derived methods from DLS::Resource
            using DLS::Resource::GetParent;
            // overridden methods
            Region*   GetFirstRegion();
            Region*   GetNextRegion();
            Region*   AddRegion();
            void      DeleteRegion(Region* pRegion);
            virtual void UpdateChunks();
            // own methods
            Region*   GetRegion(unsigned int Key);
            MidiRule* GetMidiRule(int i);
        protected:
            Region*   RegionKeyTable[128]; ///< fast lookup for the corresponding Region of a MIDI key

            Instrument(File* pFile, RIFF::List* insList, progress_t* pProgress = NULL);
           ~Instrument();
            void UpdateRegionKeyTable();
            friend class File;
            friend class Region; // so Region can call UpdateRegionKeyTable()
        private:
            MidiRule** pMidiRules;
    };

    /** @brief Group of Gigasampler objects
     *
     * Groups help to organize a huge collection of Gigasampler objects.
     * Groups are not concerned at all for the synthesis, but they help
     * sound library developers when working on complex instruments with an
     * instrument editor (as long as that instrument editor supports it ;-).
     *
     * At the moment, it seems as only samples can be grouped together in
     * the Gigasampler format yet. If this is false in the meantime, please
     * tell us !
     *
     * A sample is always assigned to exactly one Group. This also means
     * there is always at least one Group in a .gig file, no matter if you
     * created one yet or not.
     */
    class Group {
        public:
            String Name; ///< Stores the name of this Group.

            Sample* GetFirstSample();
            Sample* GetNextSample();
            void AddSample(Sample* pSample);
        protected:
            Group(File* file, RIFF::Chunk* ck3gnm);
            virtual ~Group();
            virtual void UpdateChunks();
            void MoveAll();
            friend class File;
        private:
            File*        pFile;
            RIFF::Chunk* pNameChunk;
    };

    /** Parses Gigasampler files and provides abstract access to the data. */
    class File : protected DLS::File {
        public:
            static const DLS::version_t VERSION_2;
            static const DLS::version_t VERSION_3;

            // derived attributes from DLS::Resource
            using DLS::Resource::pInfo;
            using DLS::Resource::pDLSID;
            // derived attributes from DLS::File
            using DLS::File::pVersion;
            using DLS::File::Instruments;

            // derived methods from DLS::Resource
            using DLS::Resource::GetParent;
            // derived methods from DLS::File
            using DLS::File::Save;
            using DLS::File::GetFileName;
            // overridden  methods
            File();
            File(RIFF::File* pRIFF);
            Sample*     GetFirstSample(progress_t* pProgress = NULL); ///< Returns a pointer to the first <i>Sample</i> object of the file, <i>NULL</i> otherwise.
            Sample*     GetNextSample();      ///< Returns a pointer to the next <i>Sample</i> object of the file, <i>NULL</i> otherwise.
            Sample*     AddSample();
            void        DeleteSample(Sample* pSample);
            Instrument* GetFirstInstrument(); ///< Returns a pointer to the first <i>Instrument</i> object of the file, <i>NULL</i> otherwise.
            Instrument* GetNextInstrument();  ///< Returns a pointer to the next <i>Instrument</i> object of the file, <i>NULL</i> otherwise.
            Instrument* GetInstrument(uint index, progress_t* pProgress = NULL);
            Instrument* AddInstrument();
            void        DeleteInstrument(Instrument* pInstrument);
            Group*      GetFirstGroup(); ///< Returns a pointer to the first <i>Group</i> object of the file, <i>NULL</i> otherwise.
            Group*      GetNextGroup();  ///< Returns a pointer to the next <i>Group</i> object of the file, <i>NULL</i> otherwise.
            Group*      GetGroup(uint index);
            Group*      AddGroup();
            void        DeleteGroup(Group* pGroup);
            void        DeleteGroupOnly(Group* pGroup);
            void        SetAutoLoad(bool b);
            bool        GetAutoLoad();
            virtual    ~File();
            virtual void UpdateChunks();
        protected:
            // overridden protected methods from DLS::File
            virtual void LoadSamples();
            virtual void LoadInstruments();
            virtual void LoadGroups();
            // own protected methods
            virtual void LoadSamples(progress_t* pProgress);
            virtual void LoadInstruments(progress_t* pProgress);
            void SetSampleChecksum(Sample* pSample, uint32_t crc);
            friend class Region;
            friend class Sample;
            friend class Group; // so Group can access protected member pRIFF
        private:
            std::list<Group*>*          pGroups;
            std::list<Group*>::iterator GroupsIterator;
            bool                        bAutoLoad;
    };

    /**
     * Will be thrown whenever a gig specific error occurs while trying to
     * access a Gigasampler File. Note: In your application you should
     * better catch for RIFF::Exception rather than this one, except you
     * explicitly want to catch and handle gig::Exception, DLS::Exception
     * and RIFF::Exception independently, which usually shouldn't be
     * necessary though.
     */
    class Exception : public DLS::Exception {
        public:
            Exception(String Message);
            void PrintMessage();
    };

    String libraryName();
    String libraryVersion();

} // namespace gig

#endif // __GIG_H__
