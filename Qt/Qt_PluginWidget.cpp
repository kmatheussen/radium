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


#include <math.h>

#include <QWidget>
#include <QGridLayout>
#include <QTabWidget>
#include <QString>
#include <QFrame>
#include <QLabel>
#include <QHash>

#include "Qt_MyQSlider.h"

#include "../audio/SoundPlugin.h"
#include "../audio/SoundPlugin_proc.h"
#include "../common/OS_Player_proc.h"


// Widget generation code is based on / copied from qtractor, written by Rui Nuno Capela.


// !@#$!@#$!@#$ emacs c++ mode bugs. (it screwes up indentation, especially in this file, for some reason)

#include "mQt_PluginWidget.cpp"



PluginWidget *PluginWidget_create(QWidget *parent, struct Patch *patch){
  SoundPlugin *plugin = (SoundPlugin*)patch->patchdata;
  const SoundPluginType *type = plugin->type;
  PluginWidget *widget = new PluginWidget(parent);

  //PluginType *pType = m_pPlugin->type();

  const int MaxYsPerPage     = 8;
  const int MaxXsPerPage     = 4;
  const int MaxParamsPerPage = MaxYsPerPage * MaxXsPerPage;

  //const Plugin::Params& params = m_pPlugin->params();
  int iParams = PLUGIN_get_num_visible_effects(plugin);

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

  if(is_multiband){

    iYsPerPage = 8;
    iXsPerPage = 4;

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
  QTabWidget  *pTabWidget = NULL;
  QVBoxLayout *pVBoxLayout = NULL;

  if (iPages > 1) {
    pTabWidget  = new QTabWidget();
    pVBoxLayout = new QVBoxLayout();
    pVBoxLayout->setMargin(0);
    pVBoxLayout->setSpacing(0);
  }

  QGridLayout *pGridLayout = new QGridLayout();
  pGridLayout->setMargin(0);
  pGridLayout->setSpacing(0);

  int iPage = 0;
  const QString sPage = "Page %1";
  QWidget *pPageWidget = NULL;
  if (pTabWidget) {	
    pPageWidget = new QWidget();
    pPageWidget->setLayout(pGridLayout);
    pTabWidget->addTab(pPageWidget, sPage.arg(++iPage));
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

    if(is_multiband && !strcmp(PLUGIN_get_effect_name(type,effect_num),"Limiter Bypass"))
      continue;

    //PluginParam *pParam = param.value();
    //PluginParamWidget *pParamWidget = new PluginParamWidget(pParam, this);
    ParamWidget *param_widget = new ParamWidget(widget, patch, effect_num);
    widget->_param_widgets.push_back(param_widget);
    pGridLayout->addWidget(param_widget, iY, iX);
    if (++iY >= iYsPerPage) {
      iY = 0;
      if (++iX >= iXsPerPage) {
        iX = 0;
        if (pTabWidget && iPage < iPages) {
          pGridLayout = new QGridLayout();
          pGridLayout->setMargin(0);
          pGridLayout->setSpacing(0);
          pPageWidget = new QWidget();
          pPageWidget->setLayout(pGridLayout);
          pTabWidget->addTab(pPageWidget, sPage.arg(++iPage));
        }
      }
    }
  }

  if (pVBoxLayout && pTabWidget) {
    pVBoxLayout->addWidget(pTabWidget);
    widget->setLayout(pVBoxLayout);
  } else {
    widget->setLayout(pGridLayout);
  }

  //widget->show();

  return widget;
}

