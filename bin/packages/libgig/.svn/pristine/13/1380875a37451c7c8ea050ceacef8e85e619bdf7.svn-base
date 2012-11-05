// Unit Test Runner for libgig (text only version)
// -----------------------------------------------
//
// This console application is used to run all written tests against current
// libgig codebase. Progress of the test runs and the final result will be
// printed out as simple text on the console.
//
// The test runner is not compiled by default (means by just running 'make'
// or 'make all' at the top level directory), you have to compile it
// explicitly by running 'make tests' in the toplevel directory or
// 'make libgigtests' in this source directory. Note: you need to have
// cppunit installed on your system to be able to compile the unit tests.
//
// This file usually doesn't have to be changed, especially not for adding
// new tests !

#include <cppunit/extensions/TestFactoryRegistry.h>
#include <cppunit/ui/text/TestRunner.h>

int main(int argc, char** argv) {
    CppUnit::TextUi::TestRunner runner;
    CppUnit::TestFactoryRegistry &registry = CppUnit::TestFactoryRegistry::getRegistry();
    runner.addTest( registry.makeTest() );
    bool wasSuccessful = runner.run( "", false );
    return wasSuccessful;
}
