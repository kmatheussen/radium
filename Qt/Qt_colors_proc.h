/* Copyright 2012 Kjetil S. Matheussen

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

#ifndef QT_COLORS_PROC_H
#define QT_COLORS_PROC_H

#include "../common/visual_proc.h"

class EditorWidget;
class QWidget;
class QApplication;

extern const char *get_color_display_name(enum ColorNums colornum);
extern bool has_qcolor(int colornum);
extern bool is_qcolor_separator(int colornum);
extern QColor get_qcolor(enum ColorNums colornum);
extern QColor get_custom_qcolor(int colornum);
extern QColor get_config_qcolor(QString colorname);
extern void setWidgetColors(QWidget *widget);
extern void setApplicationColors(QApplication *app);
//extern void setEditorColors(EditorWidget *editor);
extern void testColorInRealtime(enum ColorNums num, QColor color);

void GFX_SetBrightness(struct Tracker_Windows *tvisual, float how_much);

void GFX_ResetColor(enum ColorNums colornum);
void GFX_ResetColors(void);
void GFX_SaveColors(const wchar_t *filename); // if filename==NULL, save to ~/.radium/colors.

#ifdef RADIUM_COMMON_API_PROC_H

/*
static inline void apply_brightness(QColor &color, float brightness){
  if(brightness > 0.5001)
    color = color.lighter(scale(brightness, 0.5, 1.0, 100, 600));
  else if(brightness < 0.4999)
    color = color.darker(scale(brightness, 0.5, 0.0, 100, 600));
}
*/

static inline void apply_saturation_and_brightness(QColor &color, float saturation, float brightness){
  bool do_saturation = saturation < 0.4999 || saturation > 0.5001;
  bool do_brightness = brightness < 0.4999 || brightness > 0.5001;
  if (!do_saturation && !do_brightness)
    return;
  
  qreal h,s,l,a;
  color.getHslF(&h,&s,&l,&a);

  if(do_saturation){
    if (saturation < 0.5)    
      s = scale(saturation, 0, 0.5, 0, s);
    else
      s = scale(saturation, 0.5, 1.0, s, 1);
  }

  if(do_brightness){
    if (brightness < 0.5)    
      l = scale(brightness, 0, 0.5, 0, l);
    else
      l = scale(brightness, 0.5, 1.0, l, 1);
  }
  
  color.setHslF(h, s, l, a);
}

static inline void apply_instrument_colorization(QColor &color){
  apply_saturation_and_brightness(color, getInstrumentSaturation(), getInstrumentBrightness());
}
  
static inline void apply_instrument_in_editor_colorization(QColor &color){
  apply_saturation_and_brightness(color, getInstrumentSaturationInEditor(), getInstrumentBrightnessInEditor());
}
  
static inline QColor get_displayed_instrument_color(const struct Patch *patch){
  QColor color(patch->color);

  apply_instrument_colorization(color);
  
  return color;
}


static inline QColor get_displayed_instrument_color_in_editor(const struct Patch *patch){
  QColor color(patch->color);

  apply_instrument_in_editor_colorization(color);
  
  return color;
}


static inline void apply_block_colorization(QColor &color){
  apply_saturation_and_brightness(color, getBlockSaturation(),  getBlockBrightness());
}
  
static inline QColor get_displayed_block_color(const struct Blocks *block){
  QColor color(block->color);

  apply_block_colorization(color);
  
  return color;
}

#ifdef _RADIUM_AUDIO_SAMPLEREADER_PROC_H
static inline QColor get_displayed_audiofile_color(const wchar_t *filename){
  QColor color(SAMPLEREADER_get_sample_color(filename));

  apply_block_colorization(color);
  
  return color;
}
#endif

#endif


#endif
