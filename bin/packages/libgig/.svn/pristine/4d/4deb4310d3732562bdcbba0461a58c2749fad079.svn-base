#ifndef __LIBGIG_GIGWRITETEST_H__
#define __LIBGIG_GIGWRITETEST_H__

#include <cppunit/TestFixture.h>
#include <cppunit/extensions/HelperMacros.h>

class GigWriteTest : public CppUnit::TestFixture {

    CPPUNIT_TEST_SUITE(GigWriteTest);
    CPPUNIT_TEST(printTestSuiteName);
    CPPUNIT_TEST(createNewGigFile);
    CPPUNIT_TEST(testOpenCreatedGigFile);
    CPPUNIT_TEST(testArticulationsOfCreatedGigFile);
    CPPUNIT_TEST(testWriteSamples);
    CPPUNIT_TEST(testSamplesData);
    CPPUNIT_TEST_SUITE_END();

    public:
        void setUp();
        void tearDown();

        void printTestSuiteName();

        void createNewGigFile();
        void testOpenCreatedGigFile();
        void testArticulationsOfCreatedGigFile();
        void testWriteSamples();
        void testSamplesData();
};

#endif // __LIBGIG_GIGWRITETEST_H__
