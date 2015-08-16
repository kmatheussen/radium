/*
  ==============================================================================

   This file is part of the JUCE library.
   Copyright (c) 2013 - Raw Material Software Ltd.

   Permission is granted to use this software under the terms of either:
   a) the GPL v2 (or any later version)
   b) the Affero GPL v3

   Details of these licenses can be found at: www.gnu.org/licenses

   JUCE is distributed in the hope that it will be useful, but WITHOUT ANY
   WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR
   A PARTICULAR PURPOSE.  See the GNU General Public License for more details.

   ------------------------------------------------------------------------------

   To release a closed-source product which uses JUCE, commercial licenses are
   available: visit www.juce.com for more information.

  ==============================================================================
*/

#ifndef JUCE_FLOATVECTOROPERATIONS_H_INCLUDED
#define JUCE_FLOATVECTOROPERATIONS_H_INCLUDED


//==============================================================================
/**
    A collection of simple vector operations on arrays of floats, accelerated with
    SIMD instructions where possible.
*/
class JUCE_API  FloatVectorOperations
{
public:
    //==============================================================================
    /** Clears a vector of floats. */
    static void JUCE_CALLTYPE clear (float* dest, int numValues) noexcept;

    /** Clears a vector of doubles. */
    static void JUCE_CALLTYPE clear (double* dest, int numValues) noexcept;

    /** Copies a repeated value into a vector of floats. */
    static void JUCE_CALLTYPE fill (float* dest, float valueToFill, int numValues) noexcept;

    /** Copies a repeated value into a vector of doubles. */
    static void JUCE_CALLTYPE fill (double* dest, double valueToFill, int numValues) noexcept;

    /** Copies a vector of floats. */
    static void JUCE_CALLTYPE copy (float* dest, const float* src, int numValues) noexcept;

    /** Copies a vector of doubles. */
    static void JUCE_CALLTYPE copy (double* dest, const double* src, int numValues) noexcept;

    /** Copies a vector of floats, multiplying each value by a given multiplier */
    static void JUCE_CALLTYPE copyWithMultiply (float* dest, const float* src, float multiplier, int numValues) noexcept;

    /** Copies a vector of doubles, multiplying each value by a given multiplier */
    static void JUCE_CALLTYPE copyWithMultiply (double* dest, const double* src, double multiplier, int numValues) noexcept;

    /** Adds a fixed value to the destination values. */
    static void JUCE_CALLTYPE add (float* dest, float amountToAdd, int numValues) noexcept;

    /** Adds a fixed value to the destination values. */
    static void JUCE_CALLTYPE add (double* dest, double amountToAdd, int numValues) noexcept;

    /** Adds a fixed value to each source value and stores it in the destination array. */
    static void JUCE_CALLTYPE add (float* dest, float* src, float amount, int numValues) noexcept;

    /** Adds a fixed value to each source value and stores it in the destination array. */
    static void JUCE_CALLTYPE add (double* dest, double* src, double amount, int numValues) noexcept;

    /** Adds the source values to the destination values. */
    static void JUCE_CALLTYPE add (float* dest, const float* src, int numValues) noexcept;

    /** Adds the source values to the destination values. */
    static void JUCE_CALLTYPE add (double* dest, const double* src, int numValues) noexcept;

    /** Adds each source1 value to the corresponding source2 value and stores the result in the destination array. */
    static void JUCE_CALLTYPE add (float* dest, const float* src1, const float* src2, int num) noexcept;

    /** Adds each source1 value to the corresponding source2 value and stores the result in the destination array. */
    static void JUCE_CALLTYPE add (double* dest, const double* src1, const double* src2, int num) noexcept;

    /** Subtracts the source values from the destination values. */
    static void JUCE_CALLTYPE subtract (float* dest, const float* src, int numValues) noexcept;

    /** Subtracts the source values from the destination values. */
    static void JUCE_CALLTYPE subtract (double* dest, const double* src, int numValues) noexcept;

    /** Subtracts each source2 value from the corresponding source1 value and stores the result in the destination array. */
    static void JUCE_CALLTYPE subtract (float* dest, const float* src1, const float* src2, int num) noexcept;

    /** Subtracts each source2 value from the corresponding source1 value and stores the result in the destination array. */
    static void JUCE_CALLTYPE subtract (double* dest, const double* src1, const double* src2, int num) noexcept;

    /** Multiplies each source value by the given multiplier, then adds it to the destination value. */
    static void JUCE_CALLTYPE addWithMultiply (float* dest, const float* src, float multiplier, int numValues) noexcept;

    /** Multiplies each source value by the given multiplier, then adds it to the destination value. */
    static void JUCE_CALLTYPE addWithMultiply (double* dest, const double* src, double multiplier, int numValues) noexcept;

    /** Multiplies the destination values by the source values. */
    static void JUCE_CALLTYPE multiply (float* dest, const float* src, int numValues) noexcept;

    /** Multiplies the destination values by the source values. */
    static void JUCE_CALLTYPE multiply (double* dest, const double* src, int numValues) noexcept;

    /** Multiplies each source1 value by the correspinding source2 value, then stores it in the destination array. */
    static void JUCE_CALLTYPE multiply (float* dest, const float* src1, const float* src2, int numValues) noexcept;

    /** Multiplies each source1 value by the correspinding source2 value, then stores it in the destination array. */
    static void JUCE_CALLTYPE multiply (double* dest, const double* src1, const double* src2, int numValues) noexcept;

    /** Multiplies each of the destination values by a fixed multiplier. */
    static void JUCE_CALLTYPE multiply (float* dest, float multiplier, int numValues) noexcept;

    /** Multiplies each of the destination values by a fixed multiplier. */
    static void JUCE_CALLTYPE multiply (double* dest, double multiplier, int numValues) noexcept;

    /** Multiplies each of the source values by a fixed multiplier and stores the result in the destination array. */
    static void JUCE_CALLTYPE multiply (float* dest, const float* src, float multiplier, int num) noexcept;

    /** Multiplies each of the source values by a fixed multiplier and stores the result in the destination array. */
    static void JUCE_CALLTYPE multiply (double* dest, const double* src, double multiplier, int num) noexcept;

    /** Copies a source vector to a destination, negating each value. */
    static void JUCE_CALLTYPE negate (float* dest, const float* src, int numValues) noexcept;

    /** Copies a source vector to a destination, negating each value. */
    static void JUCE_CALLTYPE negate (double* dest, const double* src, int numValues) noexcept;

    /** Converts a stream of integers to floats, multiplying each one by the given multiplier. */
    static void JUCE_CALLTYPE convertFixedToFloat (float* dest, const int* src, float multiplier, int numValues) noexcept;

    /** Finds the miniumum and maximum values in the given array. */
    static Range<float> JUCE_CALLTYPE findMinAndMax (const float* src, int numValues) noexcept;

    /** Finds the miniumum and maximum values in the given array. */
    static Range<double> JUCE_CALLTYPE findMinAndMax (const double* src, int numValues) noexcept;

    /** Finds the miniumum value in the given array. */
    static float JUCE_CALLTYPE findMinimum (const float* src, int numValues) noexcept;

    /** Finds the miniumum value in the given array. */
    static double JUCE_CALLTYPE findMinimum (const double* src, int numValues) noexcept;

    /** Finds the maximum value in the given array. */
    static float JUCE_CALLTYPE findMaximum (const float* src, int numValues) noexcept;

    /** Finds the maximum value in the given array. */
    static double JUCE_CALLTYPE findMaximum (const double* src, int numValues) noexcept;

    /** On Intel CPUs, this method enables or disables the SSE flush-to-zero mode.
        Effectively, this is a wrapper around a call to _MM_SET_FLUSH_ZERO_MODE
    */
    static void JUCE_CALLTYPE enableFlushToZeroMode (bool shouldEnable) noexcept;
};


#endif   // JUCE_FLOATVECTOROPERATIONS_H_INCLUDED
