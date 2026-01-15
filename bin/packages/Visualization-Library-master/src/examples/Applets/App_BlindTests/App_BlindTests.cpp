/**************************************************************************************/
/*                                                                                    */
/*  Visualization Library                                                             */
/*  http://www.visualizationlibrary.org                                               */
/*                                                                                    */
/*  Copyright (c) 2005-2010, Michele Bosi                                             */
/*  All rights reserved.                                                              */
/*                                                                                    */
/*  Redistribution and use in source and binary forms, with or without modification,  */
/*  are permitted provided that the following conditions are met:                     */
/*                                                                                    */
/*  - Redistributions of source code must retain the above copyright notice, this     */
/*  list of conditions and the following disclaimer.                                  */
/*                                                                                    */
/*  - Redistributions in binary form must reproduce the above copyright notice, this  */
/*  list of conditions and the following disclaimer in the documentation and/or       */
/*  other materials provided with the distribution.                                   */
/*                                                                                    */
/*  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND   */
/*  ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED     */
/*  WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE            */
/*  DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR  */
/*  ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES    */
/*  (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;      */
/*  LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON    */
/*  ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT           */
/*  (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS     */
/*  SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.                      */
/*                                                                                    */
/**************************************************************************************/

#include "../BaseDemo.hpp"
#include <vlGraphics/Text.hpp>
#include <vlCore/Time.hpp>
#include <vlGraphics/FontManager.hpp>
#include <vlCore/TextStream.hpp>
#include <vlGraphics/Geometry.hpp>
#include <time.h>

namespace blind_tests
{
  bool test_TypeInfo();
  bool test_filesystem();
  bool test_hfloat();
  bool test_math();
  bool test_signal_slot();
  bool test_UID();
}

using namespace blind_tests;
using namespace vl;

typedef bool (*TestType)();

struct s_Test
{
  TestType mTest;
  const char* mTestName;
};

s_Test g_Tests[] = { 
  { test_TypeInfo,    "TypeInfo"     },
  { test_math,        "Math"         },
  { test_filesystem,  "Filesystem"   },
  { test_hfloat,      "Half Float"   },
  { test_signal_slot, "Signal Slot"  },
  { test_UID,         "UUID"         },
  { NULL, NULL }
};

class App_BlindTests: public BaseDemo
{

public:
  void initEvent()
  {
    vl::Log::notify(appletInfo());
    String msg;
    Time time;

    for(s_Test* test=g_Tests; test->mTestName; ++test)
    {
      time.start();
      bool ok = test->mTest();
      String test_msg = Say("[%s] test \"%s\" (%.2ns)\n") << (ok?"Passed":"FAILED") << test->mTestName << time.elapsed();
      msg += test_msg;
      Log::print( test_msg );
    }

    // display test pass/failure information

    ref<Text> text = new Text;
    text->setText( msg );
    text->setFont( defFontManager()->acquireFont("/font/bitstream-vera/VeraSe.ttf", 12) );
    text->setAlignment( AlignLeft | AlignTop );
    text->setViewportAlignment( AlignLeft | AlignTop );
    ref<Effect> effect = new Effect;
    effect->shader()->enable(EN_BLEND);
    sceneManager()->tree()->addActor(text.get(), effect.get());
  }

};

BaseDemo* Create_App_BlindTests() { return new App_BlindTests; }

