/*
  ==============================================================================

   This file is part of the juce_core module of the JUCE library.
   Copyright (c) 2013 - Raw Material Software Ltd.

   Permission to use, copy, modify, and/or distribute this software for any purpose with
   or without fee is hereby granted, provided that the above copyright notice and this
   permission notice appear in all copies.

   THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES WITH REGARD
   TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS. IN
   NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL
   DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER
   IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN
   CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

   ------------------------------------------------------------------------------

   NOTE! This permissive ISC license applies ONLY to files within the juce_core module!
   All other JUCE modules are covered by a dual GPL/commercial license, so if you are
   using any other modules, be sure to check that you also comply with their license.

   For more details, visit www.juce.com

  ==============================================================================
*/

Random::Random (const int64 seedValue) noexcept   : seed (seedValue)
{
}

Random::Random()  : seed (1)
{
    setSeedRandomly();
}

Random::~Random() noexcept
{
}

void Random::setSeed (const int64 newSeed) noexcept
{
    seed = newSeed;
}

void Random::combineSeed (const int64 seedValue) noexcept
{
    seed ^= nextInt64() ^ seedValue;
}

void Random::setSeedRandomly()
{
    static int64 globalSeed = 0;

    combineSeed (globalSeed ^ (int64) (pointer_sized_int) this);
    combineSeed (Time::getMillisecondCounter());
    combineSeed (Time::getHighResolutionTicks());
    combineSeed (Time::getHighResolutionTicksPerSecond());
    combineSeed (Time::currentTimeMillis());
    globalSeed ^= seed;
}

Random& Random::getSystemRandom() noexcept
{
    static Random sysRand;
    return sysRand;
}

//==============================================================================
int Random::nextInt() noexcept
{
    seed = (seed * 0x5deece66dLL + 11) & 0xffffffffffffLL;

    return (int) (seed >> 16);
}

int Random::nextInt (const int maxValue) noexcept
{
    jassert (maxValue > 0);
    return (int) ((((unsigned int) nextInt()) * (uint64) maxValue) >> 32);
}

int Random::nextInt (Range<int> range) noexcept
{
    return range.getStart() + nextInt (range.getLength());
}

int64 Random::nextInt64() noexcept
{
    return (((int64) nextInt()) << 32) | (int64) (uint64) (uint32) nextInt();
}

bool Random::nextBool() noexcept
{
    return (nextInt() & 0x40000000) != 0;
}

float Random::nextFloat() noexcept
{
    return static_cast<uint32> (nextInt()) / (std::numeric_limits<uint32>::max() + 1.0f);
}

double Random::nextDouble() noexcept
{
    return static_cast<uint32> (nextInt()) / (std::numeric_limits<uint32>::max() + 1.0);
}

BigInteger Random::nextLargeNumber (const BigInteger& maximumValue)
{
    BigInteger n;

    do
    {
        fillBitsRandomly (n, 0, maximumValue.getHighestBit() + 1);
    }
    while (n >= maximumValue);

    return n;
}

void Random::fillBitsRandomly (void* const buffer, size_t bytes)
{
    int* d = static_cast<int*> (buffer);

    for (; bytes >= sizeof (int); bytes -= sizeof (int))
        *d++ = nextInt();

    if (bytes > 0)
    {
        const int lastBytes = nextInt();
        memcpy (d, &lastBytes, bytes);
    }
}

void Random::fillBitsRandomly (BigInteger& arrayToChange, int startBit, int numBits)
{
    arrayToChange.setBit (startBit + numBits - 1, true);  // to force the array to pre-allocate space

    while ((startBit & 31) != 0 && numBits > 0)
    {
        arrayToChange.setBit (startBit++, nextBool());
        --numBits;
    }

    while (numBits >= 32)
    {
        arrayToChange.setBitRangeAsInt (startBit, 32, (unsigned int) nextInt());
        startBit += 32;
        numBits -= 32;
    }

    while (--numBits >= 0)
        arrayToChange.setBit (startBit + numBits, nextBool());
}

//==============================================================================
#if JUCE_UNIT_TESTS

class RandomTests  : public UnitTest
{
public:
    RandomTests() : UnitTest ("Random") {}

    void runTest()
    {
        beginTest ("Random");

        Random r = getRandom();

        for (int i = 2000; --i >= 0;)
        {
            expect (r.nextDouble() >= 0.0 && r.nextDouble() < 1.0);
            expect (r.nextFloat() >= 0.0f && r.nextFloat() < 1.0f);
            expect (r.nextInt (5) >= 0 && r.nextInt (5) < 5);
            expect (r.nextInt (1) == 0);

            int n = r.nextInt (50) + 1;
            expect (r.nextInt (n) >= 0 && r.nextInt (n) < n);

            n = r.nextInt (0x7ffffffe) + 1;
            expect (r.nextInt (n) >= 0 && r.nextInt (n) < n);
        }
    }
};

static RandomTests randomTests;

#endif
