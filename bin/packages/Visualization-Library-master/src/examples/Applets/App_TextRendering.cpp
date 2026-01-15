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

#include "BaseDemo.hpp"
#include <vlCore/Colors.hpp>
#include <vlGraphics/GeometryPrimitives.hpp>
#include <vlGraphics/Text.hpp>
#include <vlGraphics/FontManager.hpp>
#include <vlGraphics/Light.hpp>

class App_TextRendering: public BaseDemo
{
public:
  App_TextRendering(int test): mTestNumber(test) {}

  void updateScene()
  {
    if (_text1)
    {
      _mat1.setIdentity();
      _mat1.rotate( sin( (float)vl::Time::currentTime()) * 30, 0 ,0 ,1);
      _text1->setMatrix(_mat1);
    }
    if (mTestNumber == 4)
    {
      static vl::real velocity[] = {
        vl::random(0,1)*10+10, vl::random(0,1)*10+10, vl::random(0,1)*10+10, vl::random(0,1)*10+10,
        vl::random(0,1)*10+10, vl::random(0,1)*10+10, vl::random(0,1)*10+10, vl::random(0,1)*10+10, vl::random(0,1)*10+10 };

      /* planets */
      for(int i=0; i<9; i++)
      {
        vl::mat4 m;
        m.translate(3.0f*(i+1),0,0);
        m.rotate(vl::Time::currentTime() * velocity[i], 0, 1, 0);
        planet_tr[i]->setLocalMatrix( m );
      }

      /* moon */
      /* note: the Moon's transform is relative to the Earth's transform */
      vl::mat4 m;
      m.translate(1.5f,0,0);
      m.rotate(vl::Time::currentTime() * 90, 0, 1, 0);
      moon_tr->setLocalMatrix( m );
    }
  }

  void initEvent()
  {
    vl::Log::notify(appletInfo());

    vl::ref<vl::Effect> effect = new vl::Effect;
    effect->shader()->enable(vl::EN_BLEND);

    const wchar_t* cprog =
      L"#include <stdio.h>\n"
      L"int main(int argc, char* argv[])\n"
      L"{\n"
      L"  printf(\"Hello Visualization Library!\");\n"
      L"  return 0;\n"
      L"}";

    const wchar_t* raven1 =
      L"Once upon a midnight dreary, while I pondered weak and weary,\n"
      L"Over many a quaint and curious volume of forgotten lore,\n"
      L"While I nodded, nearly napping, suddenly there came a tapping,\n"
      L"As of some one gently rapping, rapping at my chamber door.\n"
      L"'Tis some visitor,' I muttered, 'tapping at my chamber door -\n"
      L"Only this, and nothing more.'";

    const wchar_t* raven2 =
      L"Ah, distinctly I remember it was in the bleak December,\n"
      L"And each separate dying ember wrought its ghost upon the floor.\n"
      L"Eagerly I wished the morrow; - vainly I had sought to borrow\n"
      L"From my books surcease of sorrow - sorrow for the lost Lenore -\n"
      L"For the rare and radiant maiden whom the angels named Lenore -\n"
      L"Nameless here for evermore.";

    const wchar_t* raven3 =
      L"And the silken sad uncertain rustling of each purple curtain\n"
      L"Thrilled me - filled me with fantastic terrors never felt before;\n"
      L"So that now, to still the beating of my heart, I stood repeating\n"
      L"'Tis some visitor entreating entrance at my chamber door -\n"
      L"Some late visitor entreating entrance at my chamber door; -\n"
      L"This it is, and nothing more,'";

    const wchar_t* raven4 =
      L"Presently my soul grew stronger; hesitating then no longer,\n"
      L"'Sir,' said I, 'or Madam, truly your forgiveness I implore;\n"
      L"But the fact is I was napping, and so gently you came rapping,\n"
      L"And so faintly you came tapping, tapping at my chamber door,\n"
      L"That I scarce was sure I heard you' - here I opened wide the door; -\n"
      L"Darkness there, and nothing more.";

    vl::ref<vl::Font> font;
    if (mTestNumber == 0)
    {
      rendering()->as<vl::Rendering>()->camera()->viewport()->setClearColor( vl::white );

      font = vl::defFontManager()->acquireFont("/font/bitstream-vera/VeraMono.ttf", 30);

      vl::ref<vl::Text> text = new vl::Text;
      sceneManager()->tree()->addActor( text.get(), effect.get() );
      text->setFont(font.get());
      text->setKerningEnabled(false);
      text->setText( L"The Raven");
      text->setMode(vl::Text2D);
      text->setTextAlignment( vl::TextAlignJustify );
      text->setAlignment( vl::AlignHCenter | vl::AlignTop );
      text->setViewportAlignment( vl::AlignHCenter | vl::AlignTop );
      text->setColor( vl::red );
      text->setShadowVector( vl::fvec2(5,5) );
      text->setOutlineEnabled(true);
      text->setOutlineColor(vl::black);
      text->translate(0,-20,0);

      float h = (float)text->boundingRectTransformed( rendering()->as<vl::Rendering>()->camera() ).minCorner().y();

      font = vl::defFontManager()->acquireFont("/font/bitstream-vera/VeraMono.ttf", 10);
      text = new vl::Text;
      sceneManager()->tree()->addActor( text.get(), effect.get() );
      text->setFont(font.get());
      text->setText( L"By Egar Allan Poe");
      text->setMode(vl::Text2D);
      text->setTextAlignment( vl::TextAlignJustify );
      text->setAlignment( vl::AlignHCenter | vl::AlignTop );
      text->setViewportAlignment( vl::AlignTop | vl::AlignHCenter );
      text->setColor( vl::black );
      text->setShadowVector( vl::fvec2(5,5) );
      text->translate(0,h-10,0);

      h = (float)text->boundingRectTransformed( rendering()->as<vl::Rendering>()->camera() ).minCorner().y();

      font = vl::defFontManager()->acquireFont("/font/bitstream-vera/Vera.ttf", 8);
      text = new vl::Text;
      sceneManager()->tree()->addActor( text.get(), effect.get() );
      text->setFont(font.get());
      text->setText( raven1 );
      text->setMode(vl::Text2D);
      text->setTextAlignment( vl::TextAlignLeft );
      text->setAlignment( vl::AlignHCenter | vl::AlignTop );
      text->setViewportAlignment( vl::AlignTop | vl::AlignHCenter);
      text->setColor(vl::crimson);
      text->setBorderColor( vl::red );
      text->setShadowColor(vl::fvec4(0,0,0,0.3f));
      text->setBorderEnabled(true);
      text->translate(0,h-10,0);

      h = (float)text->boundingRectTransformed( rendering()->as<vl::Rendering>()->camera() ).minCorner().y();

      text = new vl::Text;
      sceneManager()->tree()->addActor( text.get(), effect.get() );
      text->setFont(font.get());
      text->setText( raven2 );
      text->setMode(vl::Text2D);
      text->setTextAlignment( vl::TextAlignCenter );
      text->setAlignment( vl::AlignHCenter | vl::AlignTop );
      text->setViewportAlignment( vl::AlignTop | vl::AlignHCenter);
      text->setColor(vl::crimson);
      text->setShadowColor(vl::fvec4(0,0,0,0.3f));
      text->setBorderEnabled(true);
      text->setBorderColor( vl::red );
      text->translate(0,h-20,0);

      h = (float)text->boundingRectTransformed( rendering()->as<vl::Rendering>()->camera() ).minCorner().y();

      text = new vl::Text;
      sceneManager()->tree()->addActor( text.get(), effect.get() );
      text->setFont(font.get());
      text->setText( raven3 );
      text->setMode(vl::Text2D);
      text->setTextAlignment( vl::TextAlignRight );
      text->setAlignment( vl::AlignHCenter | vl::AlignTop );
      text->setViewportAlignment( vl::AlignTop | vl::AlignHCenter);
      text->setColor(vl::crimson);
      text->setShadowColor(vl::fvec4(0,0,0,0.3f));
      text->setBorderEnabled(true);
      text->setBorderColor( vl::red );
      text->translate(0,h-20,0);

      h = (float)text->boundingRectTransformed( rendering()->as<vl::Rendering>()->camera() ).minCorner().y();

      text = new vl::Text;
      sceneManager()->tree()->addActor( text.get(), effect.get() );
      text->setFont(font.get());
      text->setText( raven4 );
      text->setMode(vl::Text2D);
      text->setTextAlignment( vl::TextAlignJustify );
      text->setAlignment( vl::AlignHCenter | vl::AlignTop );
      text->setViewportAlignment( vl::AlignTop | vl::AlignHCenter);
      text->setColor(vl::crimson);
      text->setShadowColor(vl::fvec4(0,0,0,0.3f));
      text->setBorderEnabled(true);
      text->setBorderColor( vl::red );
      text->translate(0,h-20,0);
    }
    else
    if (mTestNumber == 1)
    {
      font = vl::defFontManager()->acquireFont("/font/bitstream-vera/VeraMono.ttf", 8);
      vl::ref<vl::Text> text;

      text = new vl::Text;
      sceneManager()->tree()->addActor( text.get(), effect.get() );
      text->setFont(font.get());
      text->setKerningEnabled(false);
      text->setText( L"vl::AlignTop | vl::AlignLeft");
      text->setMode(vl::Text2D);
      text->setTextAlignment( vl::TextAlignJustify );
      text->setAlignment( vl::AlignTop | vl::AlignLeft );
      text->setViewportAlignment( vl::AlignTop | vl::AlignLeft );
      text->setBorderEnabled(true);
      text->setBackgroundEnabled(true);
      text->setColor( vl::black );

      text = new vl::Text;
      sceneManager()->tree()->addActor( text.get(), effect.get() );
      text->setFont(font.get());
      text->setKerningEnabled(false);
      text->setText( L"vl::AlignTop | vl::AlignRight");
      text->setMode(vl::Text2D);
      text->setTextAlignment( vl::TextAlignJustify );
      text->setAlignment( vl::AlignTop | vl::AlignRight );
      text->setViewportAlignment( vl::AlignTop | vl::AlignRight );
      text->setBorderEnabled(true);
      text->setBackgroundEnabled(true);
      text->setColor( vl::black );

      text = new vl::Text;
      sceneManager()->tree()->addActor( text.get(), effect.get() );
      text->setFont(font.get());
      text->setKerningEnabled(false);
      text->setText( L"vl::AlignBottom | vl::AlignLeft");
      text->setMode(vl::Text2D);
      text->setTextAlignment( vl::TextAlignJustify );
      text->setAlignment( vl::AlignBottom | vl::AlignLeft );
      text->setViewportAlignment( vl::AlignBottom | vl::AlignLeft );
      text->setBorderEnabled(true);
      text->setBackgroundEnabled(true);
      text->setColor( vl::black );

      text = new vl::Text;
      sceneManager()->tree()->addActor( text.get(), effect.get() );
      text->setFont(font.get());
      text->setKerningEnabled(false);
      text->setText( L"vl::AlignBottom | vl::AlignRight");
      text->setMode(vl::Text2D);
      text->setTextAlignment( vl::TextAlignJustify );
      text->setAlignment( vl::AlignBottom | vl::AlignRight );
      text->setViewportAlignment( vl::AlignBottom | vl::AlignRight );
      text->setBorderEnabled(true);
      text->setBackgroundEnabled(true);
      text->setColor( vl::black );

      text = new vl::Text;
      sceneManager()->tree()->addActor( text.get(), effect.get() );
      text->setFont(font.get());
      text->setKerningEnabled(false);
      text->setText( L"vl::AlignVCenter | vl::AlignHCenter");
      text->setMode(vl::Text2D);
      text->setAlignment( vl::AlignVCenter | vl::AlignHCenter );
      text->setViewportAlignment( vl::AlignVCenter | vl::AlignHCenter );
      text->setBorderEnabled(true);
      text->setBackgroundEnabled(true);
      text->setColor( vl::black );
    }
    else
    if (mTestNumber == 2)
    {
      _text1 = new vl::Text;
      sceneManager()->tree()->addActor( _text1.get(), effect.get() );
      font = vl::defFontManager()->acquireFont("/font/bitstream-vera/VeraMono.ttf", 10);
      _text1->setFont( font.get() );
      _text1->setText( cprog );
      _text1->setMode(vl::Text2D);
      _text1->setAlignment( vl::AlignVCenter | vl::AlignHCenter );
      _text1->setViewportAlignment( vl::AlignVCenter | vl::AlignHCenter );
      _text1->setColor(vl::black);
      _text1->setMargin(10);
      _text1->setBorderEnabled(true);
      _text1->setBackgroundEnabled(true);
      /* rotated _text1 needs smoothing to be properly rendern */
      _text1->font()->setSmooth(true);
    }
    else
    if (mTestNumber == 3)
    {
      vl::ref<vl::Text> text;

      float space = 75;

      vl::String system_font_directory = get_system_font_directory();

      /* chinese & japanese fonts */
      vl::ref<vl::Font> chinese_font = vl::defFontManager()->acquireFont(system_font_directory+"/simhei.ttf", 16);
      /* english, russian, hebrew, arabic */
      font = vl::defFontManager()->acquireFont(system_font_directory+"/arial.ttf", 16);

      text = new vl::Text;
      sceneManager()->tree()->addActor( text.get(), effect.get() );
      text->setFont( font.get() );
      text->setText( vl::String::loadText("/text/greetings-en.txt") );
      text->setMode(vl::Text2D);
      text->setAlignment( vl::AlignTop | vl::AlignHCenter );
      text->setViewportAlignment( vl::AlignTop | vl::AlignHCenter );
      text->setColor(vl::black);
      text->setMargin(10);
      text->setBorderEnabled(true);
      text->setBackgroundEnabled(true);
      text->setShadowEnabled(true);
      text->setShadowColor( vl::fvec4(0,0,0,0.25f) );
      text->translate(0,-40-space*0,0);

      text = new vl::Text;
      sceneManager()->tree()->addActor( text.get(), effect.get() );
      text->setFont( font.get() );
      text->setText( vl::String::loadText("/text/greetings-ru.txt") );
      text->setMode(vl::Text2D);
      text->setAlignment( vl::AlignTop | vl::AlignHCenter );
      text->setViewportAlignment( vl::AlignTop | vl::AlignHCenter );
      text->setColor(vl::black);
      text->setMargin(10);
      text->setBorderEnabled(true);
      text->setBackgroundEnabled(true);
      text->translate(0,-40-space*1,0);

      text = new vl::Text;
      sceneManager()->tree()->addActor( text.get(), effect.get() );
      text->setFont( font.get() );
      text->setText( vl::String::loadText("/text/greetings-he.txt") );
      text->setMode(vl::Text2D);
      text->setAlignment( vl::AlignTop | vl::AlignHCenter );
      text->setViewportAlignment( vl::AlignTop | vl::AlignHCenter );
      text->setColor(vl::black);
      text->setMargin(10);
      text->setBorderEnabled(true);
      text->setBackgroundEnabled(true);
      text->setLayout(vl::RightToLeftText);
      text->translate(0,-40-space*2,0);

      text = new vl::Text;
      sceneManager()->tree()->addActor( text.get(), effect.get() );
      text->setFont( font.get() );
      text->setText( vl::String::loadText("/text/greetings-ar.txt") );
      text->setMode(vl::Text2D);
      text->setAlignment( vl::AlignTop | vl::AlignHCenter );
      text->setViewportAlignment( vl::AlignTop | vl::AlignHCenter );
      text->setColor(vl::black);
      text->setMargin(10);
      text->setBorderEnabled(true);
      text->setBackgroundEnabled(true);
      text->setLayout(vl::RightToLeftText);
      text->translate(0,-40-space*3,0);

      text = new vl::Text;
      sceneManager()->tree()->addActor( text.get(), effect.get() );
      text->setFont( chinese_font.get() );
      text->setText( vl::String::loadText("/text/greetings-jp.txt") );
      text->setMode(vl::Text2D);
      text->setAlignment( vl::AlignTop | vl::AlignHCenter );
      text->setViewportAlignment( vl::AlignTop | vl::AlignHCenter );
      text->setColor(vl::black);
      text->setMargin(10);
      text->setBorderEnabled(true);
      text->setBackgroundEnabled(true);
      text->translate(0,-40-space*4,0);

      text = new vl::Text;
      sceneManager()->tree()->addActor( text.get(), effect.get() );
      text->setFont( chinese_font.get() );
      text->setText( vl::String::loadText("/text/greetings-ch.txt") );
      text->setMode(vl::Text2D);
      text->setAlignment( vl::AlignTop | vl::AlignHCenter );
      text->setViewportAlignment( vl::AlignTop | vl::AlignHCenter );
      text->setColor(vl::black);
      text->setMargin(10);
      text->setBorderEnabled(true);
      text->setBackgroundEnabled(true);
      text->translate(0,-40-space*5,0);

      font = vl::defFontManager()->acquireFont("/font/bitstream-vera/VeraMono.ttf", 8);

      text = new vl::Text;
      sceneManager()->tree()->addActor( text.get(), effect.get() );
      text->setFont( font.get() );
      text->setText( L"English");
      text->setMode(vl::Text2D);
      text->setAlignment( vl::AlignBottom| vl::AlignHCenter );
      text->setViewportAlignment( vl::AlignTop | vl::AlignHCenter );
      text->setColor(vl::black);
      text->setMargin(0);
      text->translate(0,-40-space*0,0);

      text = new vl::Text;
      sceneManager()->tree()->addActor( text.get(), effect.get() );
      text->setFont( font.get() );
      text->setText( L"Russian");
      text->setMode(vl::Text2D);
      text->setAlignment( vl::AlignBottom| vl::AlignHCenter );
      text->setViewportAlignment( vl::AlignTop | vl::AlignHCenter );
      text->setColor(vl::black);
      text->setMargin(0);
      text->translate(0,-40-space*1,0);

      text = new vl::Text;
      sceneManager()->tree()->addActor( text.get(), effect.get() );
      text->setFont( font.get() );
      text->setText( L"Hebrew");
      text->setMode(vl::Text2D);
      text->setAlignment( vl::AlignBottom| vl::AlignHCenter );
      text->setViewportAlignment( vl::AlignTop | vl::AlignHCenter );
      text->setColor(vl::black);
      text->setMargin(0);
      text->translate(0,-40-space*2,0);

      text = new vl::Text;
      sceneManager()->tree()->addActor( text.get(), effect.get() );
      text->setFont( font.get() );
      text->setText( L"Arabic");
      text->setMode(vl::Text2D);
      text->setAlignment( vl::AlignBottom| vl::AlignHCenter );
      text->setViewportAlignment( vl::AlignTop | vl::AlignHCenter );
      text->setColor(vl::black);
      text->setMargin(0);
      text->translate(0,-40-space*3,0);

      text = new vl::Text;
      sceneManager()->tree()->addActor( text.get(), effect.get() );
      text->setFont( font.get() );
      text->setText( L"Japanese");
      text->setMode(vl::Text2D);
      text->setAlignment( vl::AlignBottom| vl::AlignHCenter );
      text->setViewportAlignment( vl::AlignTop | vl::AlignHCenter );
      text->setColor(vl::black);
      text->setMargin(0);
      text->translate(0,-40-space*4,0);

      text = new vl::Text;
      sceneManager()->tree()->addActor( text.get(), effect.get() );
      text->setFont( font.get() );
      text->setText( L"Chinese");
      text->setMode(vl::Text2D);
      text->setAlignment( vl::AlignBottom| vl::AlignHCenter );
      text->setViewportAlignment( vl::AlignTop | vl::AlignHCenter );
      text->setColor(vl::black);
      text->setMargin(0);
      text->translate(0,-40-space*5,0);
    }
    else
    if (mTestNumber == 4)
    {
      vl::ref<vl::Light> light = new vl::Light;

      vl::ref<vl::Geometry> planet = vl::makeIcosphere(vl::vec3(0,0,0), 1, 2 );
      planet->computeNormals();

      vl::ref<vl::Geometry> moon = vl::makeIcosphere(vl::vec3(0,0,0), 0.5, 1 );
      moon->computeNormals();

      vl::ref<vl::Effect> sun_effect = new vl::Effect;
      sun_effect->shader()->setRenderState( light.get(), 0 );
      sun_effect->shader()->enable(vl::EN_LIGHTING);
      sun_effect->shader()->enable(vl::EN_DEPTH_TEST);
      sun_effect->shader()->enable(vl::EN_CULL_FACE);
      sun_effect->shader()->gocMaterial()->setDiffuse(vl::gold);

      vl::ref<vl::Effect> planet_effect = new vl::Effect;
      planet_effect->shader()->setRenderState( light.get(), 0 );
      planet_effect->shader()->enable(vl::EN_LIGHTING);
      planet_effect->shader()->enable(vl::EN_DEPTH_TEST);
      planet_effect->shader()->enable(vl::EN_CULL_FACE);
      planet_effect->shader()->gocMaterial()->setDiffuse(vl::gray);

      vl::ref<vl::Effect> earth_effect = new vl::Effect;
      earth_effect->shader()->setRenderState( light.get(), 0 );
      earth_effect->shader()->enable(vl::EN_LIGHTING);
      earth_effect->shader()->enable(vl::EN_DEPTH_TEST);
      earth_effect->shader()->enable(vl::EN_CULL_FACE);
      earth_effect->shader()->gocMaterial()->setDiffuse(vl::royalblue);

      vl::ref<vl::Font> font = vl::defFontManager()->acquireFont("/font/bitstream-vera/Vera.ttf", 10);

      vl::ref<vl::Effect> name_effect = new vl::Effect;
      name_effect->shader()->setRenderState( light.get(), 0 );
      name_effect->shader()->enable(vl::EN_LIGHTING);
      name_effect->shader()->enable(vl::EN_DEPTH_TEST);
      name_effect->shader()->enable(vl::EN_CULL_FACE);
      name_effect->shader()->disable(vl::EN_LIGHTING);
      name_effect->shader()->enable(vl::EN_BLEND);
      ///* to avoid clipping artefacts due to partial character overlapping we either disable depth
      //   testing, set depth-write mask to false or enable an appropriate alpha testing. */
      // name_effect->shader()->disable(vl::EN_DEPTH_TEST);
      name_effect->shader()->enable(vl::EN_DEPTH_TEST);

      /* sun */

      sceneManager()->tree()->addActor( planet.get(), sun_effect.get() );
      vl::ref< vl::Text > text = new vl::Text;
      sceneManager()->tree()->addActor( text.get(), name_effect.get(), new vl::Transform );
      text->setFont(font.get());
      text->setMode( vl::Text2D );
      text->setText( L"Sun");
      text->setColor(vl::white);
      text->setAlignment(vl::AlignBottom|vl::AlignLeft);

      /* belts & planets */

      vl::ref<vl::Effect> belt_effect = new vl::Effect;
      belt_effect->shader()->setRenderState( light.get(), 0 );
      belt_effect->shader()->enable(vl::EN_LIGHTING);
      belt_effect->shader()->enable(vl::EN_DEPTH_TEST);
      belt_effect->shader()->enable(vl::EN_CULL_FACE);
      belt_effect->shader()->gocMaterial()->setFlatColor(vl::red);
      belt_effect->shader()->enable(vl::EN_BLEND);
      belt_effect->shader()->enable(vl::EN_LINE_SMOOTH);
      // render this first
      belt_effect->setRenderRank(-1);

      /* planet names */

      const wchar_t* names[] = {
        L"Mercury",
        L"Venus",
        L"Earth",
        L"Mars",
        L"Jupiter",
        L"Saturn",
        L"Uranus",
        L"Neptune",
        L"Pluto"
      };

      vl::ref<vl::Transform> belt_tr = new vl::Transform;
      rendering()->as<vl::Rendering>()->transform()->addChild( belt_tr.get() );

      for(int i=0; i<9; i++)
      {
        vl::ref< vl::Geometry > belt1 = vl::makeCircle(vl::vec3(0,0,0), 3.0f*(i+1));
        sceneManager()->tree()->addActor( belt1.get(), belt_effect.get(), belt_tr.get() );

        planet_tr[i] = new vl::Transform;
        rendering()->as<vl::Rendering>()->transform()->addChild( planet_tr[i].get() );

        if (i == 2)
          /* Earth */
          sceneManager()->tree()->addActor( planet.get(), earth_effect.get(), planet_tr[i].get() );
        else
          /* other planets */
          sceneManager()->tree()->addActor( planet.get(), planet_effect.get(), planet_tr[i].get() );

        vl::ref< vl::Text > text = new vl::Text;
        sceneManager()->tree()->addActor( text.get(), name_effect.get(), planet_tr[i].get() );
        text->setFont(font.get());
        text->setMode( vl::Text2D );
        text->setText( names[i] );
        text->setColor(vl::white);
        text->setAlignment(vl::AlignBottom | vl::AlignLeft);
        /* when the label follows an object the settings for text->setViewportAlignment don't have any effect */
      }

      /* moon */

      /* we want the Moon to rotate around the Earth so we make it's transform a child of the Earth's transform */

      moon_tr = new vl::Transform;
      planet_tr[2]->addChild(moon_tr.get());
      sceneManager()->tree()->addActor( moon.get(), planet_effect.get(), moon_tr.get() );

      text = new vl::Text;
      sceneManager()->tree()->addActor( text.get(), name_effect.get(), moon_tr.get() );
      text->setFont(font.get());
      text->setMode( vl::Text2D );
      text->setText( L"Moon");
      text->setColor(vl::gold);
      text->setBorderColor(vl::royalblue);
      text->setBorderEnabled(true);
      text->setBackgroundEnabled(true);
      text->setBackgroundColor( vl::fvec4(1,1,1,0.35f) );
      text->setAlignment( vl::AlignVCenter | vl::AlignLeft );
      /* note: matrix transformations still apply in pixel coordinates and are often
         useful to offset the label from the followed object. */
      text->translate(5,0,0);

      /* the title */

      font = vl::defFontManager()->acquireFont("/font/bitstream-vera/Vera.ttf", 20);
      text = new vl::Text;
      sceneManager()->tree()->addActor( text.get(), name_effect.get() );
      text->setFont(font.get());
      text->setMode( vl::Text2D );
      text->setText( L"The Solar System");
      text->setColor(vl::white);
      text->setAlignment(vl::AlignTop | vl::AlignHCenter );
      text->setViewportAlignment(vl::AlignTop | vl::AlignHCenter );
      text->translate(0,-20,0);
    }
  }

  vl::String get_system_font_directory()
  {
#ifdef _WIN32
    char buffer[MAX_PATH];
    memset(buffer,0,sizeof(char)*MAX_PATH);
    GetWindowsDirectoryA(buffer,MAX_PATH);
    return vl::String(buffer) + "/fonts";
#else
    /* Ubuntu's Microsoft TrueType Core Fonts directory */
    return "/usr/share/fonts/truetype/msttcorefonts";
#endif
  }

protected:
  vl::fmat4 _mat1;
  vl::ref<vl::Text> _text1;

  vl::ref<vl::Text> _textA;
  vl::ref<vl::Text> _textB;
  vl::ref<vl::Text> _textC;
  vl::ref<vl::Text> _textD;

  vl::ref< vl::Transform > planet_tr[9];
  vl::ref< vl::Transform > moon_tr;

  int mTestNumber;
};

// Have fun!

BaseDemo* Create_App_TextRendering(int test) { return new App_TextRendering(test); }
