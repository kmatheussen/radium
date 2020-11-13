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


#define __STDC_FORMAT_MACROS 1
#include <inttypes.h>

#include <math.h>

#include <QWidget>
#include <QGridLayout>
#include <QTabWidget>
#include <QString>
#include <QFrame>
#include <QLabel>

#include "../common/nsmtracker.h"

#include "Qt_MyQSlider.h"

#include "../audio/SoundPlugin.h"
#include "../audio/SoundPlugin_proc.h"
#include "../common/OS_Player_proc.h"


// Widget generation code is based on / copied from qtractor, written by Rui Nuno Capela.


// !@#$!@#$!@#$ emacs c++ mode bugs. (it screwes up indentation, especially in this file, for some reason)

#include "mQt_PluginWidget.cpp"



PluginWidget *PluginWidget_create(QWidget *parent, struct Patch *patch, SizeType size_type){
  SoundPlugin *plugin = (SoundPlugin*)patch->patchdata;
  const SoundPluginType *type = plugin->type;
  PluginWidget *widget = new PluginWidget(parent, patch);

  //PluginType *pType = m_pPlugin->type();

  bool is_patchbay = !strcmp("Patchbay",plugin->type->type_name);
  bool is_midi_messages = !strcmp("MIDI Messages", plugin->type->type_name);
  
  int MaxYsPerPage     = 8;
  int MaxXsPerPage     = is_midi_messages ? 6 : 8;

  switch(size_type){
    case SIZETYPE_NORMAL:
      if (is_patchbay){
        MaxYsPerPage = 7;
        MaxXsPerPage = 18;
      }
      break;
    case SIZETYPE_HALF:
      if (is_patchbay){
        MaxYsPerPage = 15;
        MaxXsPerPage = 36;
      }else{
        MaxYsPerPage *= 2;
        MaxXsPerPage *= 2;
      }
      break;
    case SIZETYPE_FULL:
      if (is_patchbay){
        MaxYsPerPage = 31;
        MaxXsPerPage = 36;
      }else{
        MaxYsPerPage *= 4;
        MaxXsPerPage *= 2;
      }
      break;
  }

  
  int MaxParamsPerPage = MaxYsPerPage * MaxXsPerPage;


  //const Plugin::Params& params = m_pPlugin->params();
  int iParams = PLUGIN_get_num_visible_effects(plugin);

  if (iParams > MaxParamsPerPage && !is_patchbay && size_type==SIZETYPE_NORMAL){
    MaxYsPerPage     = 7;
    MaxParamsPerPage = MaxYsPerPage * MaxXsPerPage;
  }

  int iParamsPerPage = iParams;
  int iParamsOnLastPage = 0;
  if (iParamsPerPage > MaxParamsPerPage) {
    iParamsPerPage = MaxParamsPerPage;
    iParamsOnLastPage = (iParams % iParamsPerPage);
    while (iParamsOnLastPage > 0
           && iParamsOnLastPage < ((3 * iParamsPerPage) >> 2))
      iParamsOnLastPage = (iParams % --iParamsPerPage);
  }

  int iPages = 1;
  int iYsPerPage = iParamsPerPage;
  int iXsPerPage = 1;

  bool is_multiband = !strcmp(type->type_name,"Faust") && !strcmp(type->name,"Multiband Compressor");
  bool is_tapiir = !strcmp(type->type_name,"Faust") && !strcmp(type->name,"Tapiir");
  bool is_sampleplayer = !strcmp(type->type_name,"Sample Player");

  if (is_multiband){

    iYsPerPage = 8;
    iXsPerPage = 4;

  } else if(is_tapiir){
    
    iYsPerPage = 10;
    iXsPerPage = 8;
    //iPages = 2;

  } else if(is_sampleplayer){
    
    iYsPerPage = 9;
    iXsPerPage = 4;
    //iPages = 2;

  } else {

    if (iYsPerPage > MaxYsPerPage) {
      iPages = (iParams / iParamsPerPage);
      if (iParamsOnLastPage > 0)
        ++iPages;
      while (iYsPerPage > MaxYsPerPage
             && iXsPerPage < MaxXsPerPage)
        iYsPerPage = (iParamsPerPage / ++iXsPerPage);
      if (iParamsPerPage % iXsPerPage) // Adjust to balance.
        ++iYsPerPage;
    }

  }

  // Maybe we need a tabbed widget...
  QVBoxLayout *pVBoxLayout = NULL;

  if (iPages > 1) {
    widget->pTabWidget  = new QTabWidget(widget);
    pVBoxLayout = new QVBoxLayout(widget);
    pVBoxLayout->setMargin(0);
    pVBoxLayout->setSpacing(0);
#if USE_QT5
    static QStyle *style = QStyleFactory::create("plastique");
    if (style!=NULL)
      widget->pTabWidget->setStyle(style); // fusion have too much grey border around tabs
#endif
  }

  QGridLayout *pGridLayout = new QGridLayout; //(widget);
  pGridLayout->setMargin(0);
  pGridLayout->setSpacing(0);

  int iPage = 0;
  const QString sPage = "Page %1";
  QWidget *pPageWidget = NULL;
  if (widget->pTabWidget) {	
    pPageWidget = new QWidget(widget);
    pPageWidget->setLayout(pGridLayout);
    widget->pTabWidget->addTab(pPageWidget, sPage.arg(iPage++));
  }


#if QT_VERSION >= 0x040300
  pGridLayout->setHorizontalSpacing(0);
#endif
  // FIXME: Couldn't stand more than a hundred widgets?
  // or do we have one dedicated editor GUI?
  int iY = 0;
  int iX = 0;

  //Plugin::Params::ConstIterator param = params.constBegin();
  //for ( ; param != params.constEnd(); ++param) {
  for(int effect_num=0;effect_num<type->num_effects;effect_num++){

    const char *effect_name = PLUGIN_get_effect_name(plugin,effect_num);
    
    if(is_multiband && !strcmp(effect_name, "Limiter Bypass"))
      continue;

    if (type->effect_is_visible!=NULL && !type->effect_is_visible(plugin, effect_num))
      continue;

    widget->_num_rows = R_MAX(widget->_num_rows, iY + 1);
  

    //PluginParam *pParam = param.value();
    //PluginParamWidget *pParamWidget = new PluginParamWidget(pParam, this);
    ParamWidget *param_widget = new ParamWidget(widget, patch, effect_num, iPage-1);
    widget->_param_widgets.push_back(param_widget);
    pGridLayout->addWidget(param_widget, iY, iX);
    iY++;
    
    if (iY >= iYsPerPage || (is_sampleplayer && (!strcmp(effect_name, "Ping-Pong Loop") || !strcmp(effect_name, "Release") || !strcmp(effect_name, "Reverse")))) { //|| (is_sampleplayer && iX==1 && iY >=iYsPerPage-1)) {
      iY = 0;
      if (++iX >= iXsPerPage) {
        iX = 0;
        if (widget->pTabWidget && iPage < iPages) {
          pGridLayout = new QGridLayout; //(widget);
          pGridLayout->setMargin(0);
          pGridLayout->setSpacing(0);
          pPageWidget = new QWidget(widget);
          pPageWidget->setLayout(pGridLayout);
          widget->pTabWidget->addTab(pPageWidget, sPage.arg(iPage++));
        }
      }
    }
  }

  if (pVBoxLayout && widget->pTabWidget) {
    pVBoxLayout->addWidget(widget->pTabWidget);
    widget->setLayout(pVBoxLayout);
  } else {
    widget->setLayout(pGridLayout);
  }

  //widget->show();

  // set automation value pointers
  //
  for(ParamWidget *paramWidget : widget->_param_widgets){
    int effect_num = paramWidget->_effect_num;
    
    MyQSlider *slider = paramWidget->_slider;
    if (slider != NULL){
      SLIDERPAINTER_set_automation_value_pointer(slider->_painter, get_effect_color(plugin, effect_num), &plugin->slider_automation_values[effect_num]);
    }
  }

  return widget;
}

