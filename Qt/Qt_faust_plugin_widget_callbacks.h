#pragma once
/* Copyright 2012-2013 Kjetil S. Matheussen

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

#include <QScrollArea>
#include <QVBoxLayout>
#include <QLineEdit>
#include <QPushButton>
#include <QCheckBox>
#include <QSpinBox>
#include <QJsonArray>
#include <atomic>
#include <memory>

#include <QSvgWidget>
#include <QLabel>
#include <QTextDocumentFragment>
#include <QSvgRenderer>
#include <QXmlStreamReader>
#include <QFile>
#include <QRegularExpression>
#include <algorithm>

#if defined(__GNUC__) && __GNUC__ >= 5
#  pragma GCC diagnostic push
#  pragma GCC diagnostic ignored "-Wsuggest-override"
#endif
#include <Qsci/qscilexerjava.h>
#include <Qsci/qscilexercpp.h>
#if defined(__GNUC__) && __GNUC__ >= 5
#  pragma GCC diagnostic pop
#endif

#include "../audio/SoundPlugin_proc.h"

#include "Qt_plugin_widget_callbacks_proc.h"
#include "Qt_MyQScrollBar.hpp"
#include "Editor.hpp"



static void ADD_UNDO_FUNC(FaustDev_CurrPos(struct Patch *patch, const QString &code, int cursor_line, int cursor_index));


#include "Qt_faust_plugin_widget.h"

namespace
{
  
struct FaustResultSvgView
	: public QSvgWidget
	, public radium::MouseCycleFix
{
	
	FaustResultSvgView(QWidget *parent)
		: QSvgWidget(parent)
	{
		setMouseTracking(true);
	}
	
	QPoint start;
	QPoint start_scrollPos;
	
	QUrl _url;

	struct SvgLink
	{
		QRectF bounds; // in SVG coordinate space
		QString href;
	};
	
	QVector<SvgLink> _links;
	double _lineMinY = 0;
	QVector<QUrl> _navStack;       // E.g: [0]=process.svg, [n-1]=parent, [n]=current
	QVector<QRectF> _segmentRects; // hit-test rects in widget coordinates for _navStack
	int _hoveredSegment = -1;      // which segment the mouse is over, -1 = none
	QLabel *_errorLabel = nullptr;  // overlay for error messages (replaces setHtml)

	void _parseSvgLinks(const QString &filepath)
	{
		_links.clear();

		QFile file(filepath);
		
		if (!file.open(QIODevice::ReadOnly | QIODevice::Text))
			return;

		QXmlStreamReader xml(&file);

		// Parse SVG for viewBox dimensions
		QRectF viewBox;
		
		while (!xml.atEnd() && !xml.hasError())
		{
			xml.readNext();
			if (xml.isStartElement() && xml.name().compare(QStringLiteral("svg"), Qt::CaseInsensitive) == 0)
			{
				QString vb = xml.attributes().value("viewBox").toString();
				if (!vb.isEmpty())
				{
					auto parts = vb.split(QRegularExpression("[,\\s]+"), Qt::SkipEmptyParts);
					if (parts.size() >= 4)
					{
						viewBox = QRectF(parts[0].toDouble(), parts[1].toDouble(),
						                 parts[2].toDouble(), parts[3].toDouble());
					}
				}
				break;
			}
		}

		// If no viewBox found, SVG rendering will use default; skip link parsing.
		if (viewBox.isEmpty())
			return;

		// Parse <a> elements, find the line bounding box,
		// and detect the full-viewBox background link.
		QString currentHref;
		
		QString backHref;
		
		double lineMinY = 1e30;
		
		const double vbArea = viewBox.width() * viewBox.height();

		while (!xml.atEnd() && !xml.hasError())
		{
			xml.readNext();

			if (xml.isStartElement())
			{
				if (xml.name().compare(QStringLiteral("a"), Qt::CaseInsensitive) == 0)
				{
					currentHref = xml.attributes().value("xlink:href").toString();
					
					if (currentHref.isEmpty())
						currentHref = xml.attributes().value("href").toString(); // SVG2
				}
				else if (!currentHref.isEmpty() && xml.name().compare(QStringLiteral("rect"), Qt::CaseInsensitive) == 0)
				{
					double x = xml.attributes().value("x").toDouble();
					double y = xml.attributes().value("y").toDouble();
					double w = xml.attributes().value("width").toDouble();
					double h = xml.attributes().value("height").toDouble();
					
					_links.append({QRectF(x, y, w, h), currentHref});

					// Detect the full-viewBox background link by area
					if (backHref.isEmpty() && (w * h) > vbArea * 0.9)
						backHref = currentHref;
				}
				else if (!currentHref.isEmpty()
						 && xml.name().compare(QStringLiteral("polygon"), Qt::CaseInsensitive) == 0)
				{
					// Compute bounding rect of polygon points
					QString pts = xml.attributes().value("points").toString();
					
					if (!pts.isEmpty())
					{
						auto pairs = pts.split(QRegularExpression("[,\\s]+"), Qt::SkipEmptyParts);
						
						if (pairs.size() >= 4)
						{
							double minX = 1e30, minY = 1e30, maxX = -1e30, maxY = -1e30;
							
							for (int i = 0; i + 1 < pairs.size(); i += 2)
							{
								double px = pairs[i].toDouble();
								double py = pairs[i+1].toDouble();
								minX = qMin(minX, px);
								minY = qMin(minY, py);
								maxX = qMax(maxX, px);
								maxY = qMax(maxY, py);
							}
							
							_links.append({QRectF(minX, minY, maxX - minX, maxY - minY), currentHref});
						}
					}
				}
				else if (xml.name().compare(QStringLiteral("line"), Qt::CaseInsensitive) == 0)
				{
					double y1 = xml.attributes().value("y1").toDouble();
					double y2 = xml.attributes().value("y2").toDouble();
					lineMinY = qMin(lineMinY, qMin(y1, y2));
				}
			}
			else if (xml.isEndElement())
			{
				if (xml.name().compare(QStringLiteral("a"), Qt::CaseInsensitive) == 0)
					currentHref.clear();
			}
		}

		// Store line bounding box for path stuff in the top
		// and remove the full-viewBox background link
		if (lineMinY < 1e6)
			_lineMinY = lineMinY;

		if (!backHref.isEmpty())
		{
			_links.erase(std::remove_if(_links.begin(), _links.end(),
			    [&backHref](const SvgLink &l){ return l.href == backHref; }),
			    _links.end());
		}
	}

	QPointF _widgetToSvg(QPointF widgetPos) const
	{
		QRectF vb = renderer()->viewBoxF();
		if (vb.isEmpty())
			return widgetPos;

		QSize sz = size();
		if (sz.isEmpty())
			return widgetPos;

		// SVG fills the widget (stretched), so map linearly in X and Y independently.
		double svgX = widgetPos.x() * vb.width() / sz.width() + vb.x();
		double svgY = widgetPos.y() * vb.height() / sz.height() + vb.y();

		/*
		printf("   _widgetToSvg: widget=(%d,%d) size=(%dx%d) vb=(%.0fx%.0f) svg=(%.1f,%.1f)\n",
			   (int)widgetPos.x(), (int)widgetPos.y(),
			   sz.width(), sz.height(),
			   vb.width(), vb.height(),
			   svgX, svgY);
		*/
		
		return QPointF(svgX, svgY);
	}

	void setHtml(const QString &html, const QUrl &baseUrl = QUrl())
	{
		(void)baseUrl;

		// Extract text from <big>...</big> in the simple HTML error format
		QString text = html;
		int start = text.indexOf(QStringLiteral("<big>"));
		int end = text.indexOf(QStringLiteral("</big>"));
		
		if (start >= 0 && end > start)
			text = text
				.mid(start + 5, end - start - 5)
				.replace(QStringLiteral("<br>"), QStringLiteral("\n"));
		else
			text = QTextDocumentFragment::fromHtml(html).toPlainText();

		if (!_errorLabel)
		{
			_errorLabel = new QLabel(this);
			_errorLabel->setAlignment(Qt::AlignCenter);
			_errorLabel->setWordWrap(true);
			_errorLabel->setStyleSheet(QStringLiteral("QLabel { background: white; padding: 8px; color: black; }"));
			_errorLabel->setAutoFillBackground(true);
		}
		
		_errorLabel->setText(text);
		_errorLabel->setGeometry(rect());
		_errorLabel->show();
		_errorLabel->raise();
	}

	void setUrl(const QUrl &url)
	{
		if (_errorLabel)
			_errorLabel->hide();
		
		_url = url;
		
		QString filepath = _url.toLocalFile();
		
		if (!QFile::exists(filepath))
			return;
		
		_parseSvgLinks(filepath);
		
		load(filepath);
		
		update();
	}

	void reload(void)
	{
		if (!_url.isEmpty())
			setUrl(_url);
	}

	void fix_mouseMoveEvent(radium::MouseCycleEvent &event) override
	{
		auto *qevent = event.get_qtevent();
			
		if (qevent)
		{
			// Check for path link hover first
			bool over_circle = false;
			int newHover = -1;
			
			if (!_navStack.isEmpty() && _segmentRects.size() == _navStack.size())
			{
				QPointF wp = qevent->pos();
				for (int i = 0; i < _segmentRects.size(); i++)
				{
					if (_segmentRects[i].contains(wp))
					{
						QString name = _navStack[i].fileName();
						
						int dash = name.indexOf(QStringLiteral("-0"));
						
						if (dash > 0)
							name = name.left(dash);
						
						setStatusbarText(qPrintable(name));
						setCursor(Qt::PointingHandCursor);
						
						over_circle = true;
						newHover = i;
						
						break;
					}
				}
			}
			
			if (newHover != _hoveredSegment)
			{
				_hoveredSegment = newHover;
				update(); // repaint to show hover background
			}

			// Check for xlink hover to show pointing hand cursor
			QString href;
			
			bool over_link = false;
			
			if (!over_circle && renderer() != nullptr && !_links.isEmpty())
			{
				QPointF svgPos = _widgetToSvg(QPointF(qevent->pos()));
				for (int i = _links.size() - 1; i >= 0; i--)
				{
					if (_links[i].bounds.contains(svgPos))
					{
						href = _links[i].href;
						over_link = true;
						break;
					}
				}
			}
			if (!over_circle)
			{
				if (over_link)
				{
					setStatusbarText(qPrintable(href));
					setCursor(Qt::PointingHandCursor);
				}
				else
				{
					setStatusbarText("");
					setCursor(Qt::ArrowCursor);
				}
			}
		}
	}
   
	bool fix_mousePressEvent(radium::MouseCycleEvent &event) override
	{
		return true;
	}

	bool fix_mouseReleaseEvent(radium::MouseCycleEvent &event) override
	{
		auto *qevent = event.get_qtevent();
		
		if (qevent)
		{
			bool link_clicked = false;

			// First check path links
			if (!_navStack.isEmpty() && _segmentRects.size() == _navStack.size() && qevent != nullptr)
			{
				QPointF wp = qevent->pos();
				for (int i = 0; i < _segmentRects.size(); i++)
				{
					if (_segmentRects[i].contains(wp))
					{
						//printf("   Path %d clicked -> %s\n", i, _navStack[i].fileName().toUtf8().constData());
						
						QUrl target = _navStack[i];
						
						_navStack.resize(i);
						
						setUrl(target);
						
						link_clicked = true;
						
						break;
					}
				}
			}

			// Then check SVG links
			if (!link_clicked && renderer() != nullptr && !_links.isEmpty())
			{
				QPointF svgPos = _widgetToSvg(QPointF(qevent->pos()));
				
				//printf("   svgPos=(%.1f,%.1f) url=%s\n", svgPos.x(), svgPos.y(), _url.toString().toUtf8().constData());
				
				for (int i = _links.size() - 1; i >= 0; i--)
				{
					bool contains = _links[i].bounds.contains(svgPos);

					/*
					printf("   link[%d] %s: (%.1f,%.1f %.1fx%.1f) %s\n",
						   i, _links[i].href.toUtf8().constData(),
						   _links[i].bounds.x(), _links[i].bounds.y(),
						   _links[i].bounds.width(), _links[i].bounds.height(),
						   contains ? "MATCH" : "");
					*/
					
					if (contains)
					{
						QUrl resolved = _url.resolved(QUrl(_links[i].href));
						
						//printf("   NAVIGATE to %s\n", resolved.toString().toUtf8().constData());
						
						_navStack.push_back(_url);
						
						setUrl(resolved);
						
						link_clicked = true;
						
						break;
					}
				}
			}
		}

		setCursor(Qt::ArrowCursor);
			
		return true;
	}

	MOUSE_CYCLE_CALLBACKS_FOR_QT;
    
	// Seems like QWebView tries to find a smart sizeHint by default. We don't want that.
	QSize sizeHint() const override
	{
		return QSize(-1,-1);
	}

	void resizeEvent(QResizeEvent *event) override
	{
		QSvgWidget::resizeEvent(event);
		
		if (_errorLabel && _errorLabel->isVisible())
			_errorLabel->setGeometry(rect());
	}

	void paintEvent(QPaintEvent *event) override
	{
		if (_errorLabel && _errorLabel->isVisible())
		{
			QPainter p(this);
			p.fillRect(rect(), Qt::white);
			
			return;
		}

		QSvgWidget::paintEvent(event);

		if (_navStack.isEmpty() || _lineMinY <= 0 || renderer() == nullptr)
			return;

		QSize sz = size();
		if (sz.isEmpty() || renderer()->viewBoxF().isEmpty())
			return;

		double stripH = _lineMinY * sz.height() / renderer()->viewBoxF().height();

		_segmentRects.clear();

		QPainter painter(this);

		// Layout: each arrow gets dedicated gap space proportional to strip height
		double arrowGapW = stripH * 0.5;
		double totalArrowW = (_navStack.size() - 1) * arrowGapW;
		double rectW = (sz.width() - totalArrowW) / (double)_navStack.size();
		double rectH = stripH / 2.0;  // half the strip height
		double rectY = stripH / 4.0; // centered vertically

		for (int i = 0; i < _navStack.size(); i++)
		{
			double x = i * (rectW + arrowGapW);
			
			QRectF rect(x, rectY, rectW, rectH);
			
			QString text = _navStack[i].fileName();
			
			int dash = text.indexOf(QStringLiteral("-0x"));
			if (dash > 0)
				text = text.left(dash);
			
			_segmentRects.append(rect);

			// Subtle background to indicate clickability
			painter.setPen(Qt::NoPen);
			if (i == _hoveredSegment)
				painter.setBrush(QColor(0xD2, 0xDC, 0xE6)); // light blue hover
			else
				painter.setBrush(QColor(0xEE, 0xF2, 0xF5)); // barely off-white
			
			painter.drawRect(rect);

			painter.setPen(QColor(0x33, 0x44, 0x55)); // blue-gray text
			myDrawText(painter,
					   rect,
					   text,
					   Qt::AlignCenter,
					   false, 0, true);
		}

		// Paint right-pointing arrows in the gaps between rectangles
		if (_navStack.size() > 1)
		{
			painter.setPen(QColor(0x88, 0x99, 0xAA)); // lighter blue-gray
			
			for (int i = 0; i < _navStack.size() - 1; i++)
			{
				double gapX = (i + 1) * rectW + i * arrowGapW;
				myDrawText(painter,
						   QRectF(gapX, 0, arrowGapW, stripH),
						   QStringLiteral("\u25b8"),
						   Qt::AlignCenter,
						   false, 0, true);
			}
		}
	}
};

} // anon. namespace


static radium::Editor *create_faust_editor(QWidget *parent)
{
	pre_create_editor();
	
	auto *ret = new radium::Editor(parent, new QsciLexerCPP(parent));
	
	post_create_editor();
	
	return ret;
}


// FAUST2_ function declarations (defined in audio/Faust_dev2.cpp)
extern void FAUST2_set_code(struct SoundPlugin *plugin, QString code);
extern void FAUST2_set_options(struct SoundPlugin *plugin, QString options);
extern bool FAUST2_is_compiling(const struct SoundPlugin *plugin);
extern QString FAUST2_get_code(const struct SoundPlugin *plugin);
extern QString FAUST2_get_options(const struct SoundPlugin *plugin);
extern void FAUST2_generate_cpp_code(const struct SoundPlugin *plugin, int generation, std::function<void(int, QString)> callback);
extern QString FAUST2_get_error_message(const struct SoundPlugin *plugin);
extern QString FAUST2_get_svg_path(const struct SoundPlugin *plugin);
extern radium::FAUST_calledRegularlyByParentReply FAUST2_calledRegularlyByParent(struct SoundPlugin *plugin);
extern void FAUST2_start_compilation(struct SoundPlugin *plugin);
extern bool FAUST2_set_use_interpreter_backend(struct SoundPlugin *plugin, bool use_interpreter);
extern bool FAUST2_get_use_interpreter_backend(struct SoundPlugin *plugin);



namespace{

// Dispatch functions that call FAUST_ or FAUST2_ depending on plugin type.
static inline bool faust_disp_is_compiling(const SoundPlugin *plugin){
  if (!strcmp(plugin->type->type_name, "Faust Dev 2"))
    return FAUST2_is_compiling(plugin);
  else
    return FAUST_is_compiling(plugin);
}
static inline QString faust_disp_get_svg_path(const SoundPlugin *plugin){
  if (!strcmp(plugin->type->type_name, "Faust Dev 2"))
    return FAUST2_get_svg_path(plugin);
  else
    return FAUST_get_svg_path(plugin);
}
static inline QString faust_disp_get_code(const SoundPlugin *plugin){
  if (!strcmp(plugin->type->type_name, "Faust Dev 2"))
    return FAUST2_get_code(plugin);
  else
    return FAUST_get_code(plugin);
}
static inline void faust_disp_generate_cpp_code(const SoundPlugin *plugin, int generation, std::function<void(int, QString)> callback){
  if (!strcmp(plugin->type->type_name, "Faust Dev 2"))
    FAUST2_generate_cpp_code(plugin, generation, callback);
  else
    FAUST_generate_cpp_code(plugin, generation, callback);
}
static inline radium::FAUST_calledRegularlyByParentReply faust_disp_calledRegularlyByParent(SoundPlugin *plugin){
  if (!strcmp(plugin->type->type_name, "Faust Dev 2"))
    return FAUST2_calledRegularlyByParent(plugin);
  else
    return FAUST_calledRegularlyByParent(plugin);
}
static inline QString faust_disp_get_error_message(const SoundPlugin *plugin){
  if (!strcmp(plugin->type->type_name, "Faust Dev 2"))
    return FAUST2_get_error_message(plugin);
  else
    return FAUST_get_error_message(plugin);
}
static inline void faust_disp_set_code(SoundPlugin *plugin, QString code){
  if (!strcmp(plugin->type->type_name, "Faust Dev 2"))
    FAUST2_set_code(plugin, code);
  else
    FAUST_set_code(plugin, code);
}
static inline void faust_disp_set_options(SoundPlugin *plugin, QString options){
  if (!strcmp(plugin->type->type_name, "Faust Dev 2"))
    FAUST2_set_options(plugin, options);
  else
    FAUST_set_options(plugin, options);
}
static inline void faust_disp_start_compilation(SoundPlugin *plugin){
  if (!strcmp(plugin->type->type_name, "Faust Dev 2"))
    FAUST2_start_compilation(plugin);
  else
    FAUST_start_compilation(plugin);
}
static inline QString faust_disp_get_options(const SoundPlugin *plugin){
  if (!strcmp(plugin->type->type_name, "Faust Dev 2"))
    return FAUST2_get_options(plugin);
  else
    return FAUST_get_options(plugin);
}
static inline bool faust_disp_get_use_interpreter_backend(SoundPlugin *plugin){
  if (!strcmp(plugin->type->type_name, "Faust Dev 2"))
    return FAUST2_get_use_interpreter_backend(plugin);
  else
    return FAUST_get_use_interpreter_backend(plugin);
}
static inline bool faust_disp_set_use_interpreter_backend(SoundPlugin *plugin, bool use_interpreter){
  if (!strcmp(plugin->type->type_name, "Faust Dev 2"))
    return FAUST2_set_use_interpreter_backend(plugin, use_interpreter);
  else
    return FAUST_set_use_interpreter_backend(plugin, use_interpreter);
}

class Faust_Plugin_widget : public QWidget, public Ui::Faust_Plugin_widget{
  Q_OBJECT;

public:
  QWidget *parent;
  
  PluginWidget *_plugin_widget;
  QLabel *_faust_compilation_status;
  radium::GcHolder<struct Patch> _patch;
  FaustResultSvgView *_svg_view;
	
  QString _svg_view_text;

  //QLabel *_error_message;
  radium::Editor *_faust_editor;
  
  bool _initing;

  SizeType _size_type;
  SizeType _size_type_before_hidden;
  
  int _prev_cursor_line, _cursor_line;
  int _prev_cursor_index, _cursor_index;

  QString _latest_working_code;

  QDialog *_cpp_dialog;
  radium::Editor *_cpp_editor;
  
  QDialog *_options_dialog;
  radium::Editor *_options_editor;

  Faust_Plugin_widget(QWidget *parent, QLabel *faust_compilation_status, struct Patch *patch)
    : QWidget(parent)
    , parent(parent)
    , _faust_compilation_status(faust_compilation_status)
    , _patch(patch)
    , _size_type(SIZETYPE_NORMAL)
    , _size_type_before_hidden(SIZETYPE_NORMAL)
    , _prev_cursor_line(0) , _cursor_line(0)
    , _prev_cursor_index(0) , _cursor_index(0)
    , _cpp_dialog(NULL)
    , _cpp_editor(NULL)
    , _options_dialog(NULL)
    , _options_editor(NULL)
  {
    _initing = true;

    SoundPlugin *plugin = (SoundPlugin*)_patch->patchdata;

    if (faust_disp_is_compiling(plugin))
      _faust_compilation_status->setText("&#8987;");
    //_faust_compilation_status->setText("Initializing... ");
    else
      _faust_compilation_status->setText("<font color=\"green\">&#10004;</font>");
    //_faust_compilation_status->setText("Ready ");

    setupUi(this);

    if(0){
      static QStyle *style = QStyleFactory::create("plastique");
      if (style!=NULL)
        setStyle(style);
    }
    
    _faust_editor = create_faust_editor(this);
    
    develop_layout->insertWidget(0, _faust_editor);

    connect(_faust_editor, SIGNAL(textChanged()), this, SLOT(a_on__faust_editor_textChanged()));
    //connect(_faust_editor, SIGNAL(linesChanged()), this, SLOT(a_on__faust_editor_linesChanged()));
    connect(_faust_editor, SIGNAL(cursorPositionChanged(int,int)), this, SLOT(a_on__faust_editor_cursorPositionChanged(int,int)));

    _svg_view = new FaustResultSvgView(this);
    
    //svg_view->setHtml("<object id=\"svg1\" data=\"file:///home/kjetil/radium/audio/faust_multibandcomp-svg/process.svg\" type=\"image/svg+xml\"></object>");
    //svg_view->setUrl(QUrl("file:///home/kjetil/radium/audio/faust_multibandcomp-svg/process.svg"));
    _svg_view->setUrl(QUrl::fromLocalFile(QDir::fromNativeSeparators(faust_disp_get_svg_path(plugin))));
    printf("    URL: -%s-. native: -%s-, org: -%s-\n",_svg_view->_url.toString().toUtf8().constData(), QDir::fromNativeSeparators(faust_disp_get_svg_path(plugin)).toUtf8().constData(), faust_disp_get_svg_path(plugin).toUtf8().constData());

    faust_webview_layout->addWidget(_svg_view, 4);

    _plugin_widget = PluginWidget_create(this, patch, SIZETYPE_NORMAL);
    faust_interface_layout_radium->insertWidget(0, _plugin_widget);

    update_gui(); // <--- Note, update_gui sets _initing to false.

    _initing = false;
  }

  ~Faust_Plugin_widget() {
    /*
    SoundPlugin *plugin = (SoundPlugin*)_patch->patchdata;
    if (plugin!=NULL)
      FAUST_inform_about_instrument_gui(plugin, NULL);
    */
  }

  void set_text_in__faust_editor_widget(QString new_code){
    //QTextCursor cr = _faust_editor->textCursor();
    _faust_editor->setText(new_code);
    //_faust_editor->setTextCursor(cr);
  }
  
  void update_gui(){
    for(ParamWidget *param_widget : _plugin_widget->_param_widgets)
      param_widget->update_gui_element();

    #if 1
    SoundPlugin *plugin = (SoundPlugin*)_patch->patchdata;
    if (plugin!=NULL) {
      _initing = true;{
        
        QString new_code = faust_disp_get_code(plugin);
        if (new_code != _faust_editor->text())
          set_text_in__faust_editor_widget(new_code);
      
      }_initing = false;
    }
    #endif
  }

  bool showing_svg(void){
    return _svg_view_text=="";
  }

  // These two are here so that an older version is not displayed after a newer version.
  int _cpp_generation = 0;
  int _last_displayed_cpp_generation = -1;

  void update_cpp_editor(struct SoundPlugin *plugin){
    if (_cpp_editor != NULL && _cpp_editor->isVisible()) {

      _cpp_editor->setText("// Please wait, generating C++ code");


      IsAlive is_alive(this);

      faust_disp_generate_cpp_code(plugin, _cpp_generation++, [is_alive, this](int generation, QString cpp_code){

          R_ASSERT(THREADING_is_main_thread());

          if (!is_alive)
            return;
          
          if (generation < _last_displayed_cpp_generation)
            return;

          _last_displayed_cpp_generation = generation;

          if (_cpp_editor != NULL)
            _cpp_editor->setText(cpp_code);
        });
    }
  }
  
  void calledRegularlyByParent(void){

    RETURN_IF_DATA_IS_INACCESSIBLE();

    /*    
    if (Undo_num_undos()==0) // I don't think this can happen, but in case it does, we return since the call to Undo_ReopenLast() below would fail (badly).
      return;

    //R_ASSERT_RETURN_IF_FALSE(Undo_Is_Open()==false);
    if (Undo_Is_Open()==true) // <-- This seems to happen when we have just started to move a chip.
      return;
    */

    SoundPlugin *plugin = (SoundPlugin*)_patch->patchdata;
    if (plugin!=NULL) {

      const radium::FAUST_calledRegularlyByParentReply ready = faust_disp_calledRegularlyByParent(plugin);      

      if (ready.has_new_data==false){
        R_ASSERT(ready.factory_is_ready==false);
        R_ASSERT(ready.svg_is_ready==false);
        return;
      }

      if (ready.factory_is_ready) {

        if (ready.factory_succeeded) {

          _latest_working_code = faust_disp_get_code(plugin);

          _faust_compilation_status->setText("<font color=\"green\">&#10004;</font>");
          
          PluginWidget *old = _plugin_widget;
          _plugin_widget = PluginWidget_create(this, _patch.data(), SIZETYPE_NORMAL);
          
          if (_size_type != SIZETYPE_NORMAL){
            
            faust_webview_layout->removeWidget(old);
            faust_webview_layout->addWidget(_plugin_widget, 1);
            
          }else {
            
            faust_interface_layout_radium->insertWidget(0, _plugin_widget);
            
          }
          
          //_plugin_widget->set_automation_value_pointers(plugin);
          
          delete old;
          

        } else {

          _faust_compilation_status->setText("<font color=\"red\">&#10007;</font>");
          //_faust_compilation_status->setText("Failed ");

        }

      }


      bool svg_file_exists = false;
      
      if (ready.svg_is_ready && ready.svg_succeeded) {
        
        QString svg_path = faust_disp_get_svg_path(plugin);
        svg_file_exists = QFile::exists(QDir::fromNativeSeparators(svg_path));
        
        if (svg_file_exists) {

          _svg_view_text = "";
          
          _svg_view->setUrl(QUrl::fromLocalFile(QDir::fromNativeSeparators(svg_path)));
          
          printf("    URL: -%s-. native: -%s-, org: -%s-\n",_svg_view->_url.toString().toUtf8().constData(), QDir::fromNativeSeparators(svg_path).toUtf8().constData(), faust_disp_get_svg_path(plugin).toUtf8().constData());

          update_cpp_editor(plugin);
        }
      }


      bool factory_failed = ready.factory_is_ready && ready.factory_succeeded==false;
      bool svg_failed = ready.svg_is_ready && ready.svg_succeeded==false;


      if (factory_failed || svg_failed){

        printf("   ERROR BLOCK: factory_failed=%d svg_failed=%d\n", factory_failed, svg_failed);
          
        //_last_svg_view_frame->setScrollBarPolicy(Qt::Horizontal, Qt::ScrollBarAlwaysOff);
        _svg_view_text = 
                     "<!DOCTYPE html>"
                     "<html>"
                     "<body style=\"background-color:white;\"><big>"
                     +faust_disp_get_error_message(plugin)+
                     "</big></body>"
                     "</html>"
          ;
        
        _svg_view->setHtml(_svg_view_text);

      } else if (!_svg_view_text.isEmpty()) {
        
        // Error just cleared but SVG file wasn't ready — reload previous SVG to dismiss error overlay.
        printf("   ERROR CLEARED: reloading SVG from _url=%s\n", _svg_view->_url.toString().toUtf8().constData());
        _svg_view_text = "";
        if (!_svg_view->_url.isEmpty())
          _svg_view->setUrl(_svg_view->_url);
      }
    }
  }

  void set_large(SizeType new_size_type){
    _size_type = new_size_type;

    // Change vertical scroll bar policy (not easy...)
    {
      if (showing_svg())
        _svg_view->reload();
      else
        _svg_view->setHtml(_svg_view_text);
    }
    
    main_layout->addWidget(code_widget);
    
    tab_widget->hide();
    faust_webview_layout->addWidget(_plugin_widget, 1);
  }

  void set_small(void){
    _size_type = SIZETYPE_NORMAL;
    //_is_large = false;
    
    faust_interface_layout_radium->insertWidget(0, _plugin_widget);

    tab_develop_layout->addWidget(code_widget);

    tab_widget->show();
  }
  
  void change_height(SizeType type){
    if (type==SIZETYPE_NORMAL)
      set_small();
    else
      set_large(type);
  }
  
  void start_compilation(QString code){
    SoundPlugin *plugin = (SoundPlugin*)_patch->patchdata;
    if (plugin!=NULL){
      faust_disp_set_code(plugin, code);
      if (_options_editor != NULL)
        faust_disp_set_options(plugin, _options_editor->text());
      _faust_compilation_status->setText("&#8987;");
      faust_disp_start_compilation(plugin);
      //_faust_compilation_status->setText("Compiling... ");
    }
  }

  void revert_to_latest_working_version(void){
    set_text_in__faust_editor_widget(_latest_working_code);
  }

  void load_source(QString filename){
    _faust_editor->load(filename);
  }
  
  void save_source(QString filename){
    _faust_editor->save(filename);
  }

  void show_cpp_source(void){
    SoundPlugin *plugin = (SoundPlugin*)_patch->patchdata;
    if (plugin!=NULL) {
      if (_cpp_dialog==NULL) {
        _cpp_dialog = new RememberGeometryQDialog(this, radium::NOT_MODAL);
        QHBoxLayout *mainLayout = new QHBoxLayout;
      
        _cpp_dialog->setLayout(mainLayout);
        
        _cpp_editor = create_faust_editor(_cpp_dialog);

        mainLayout->addWidget(_cpp_editor);
        
        _cpp_dialog->resize(600,400);
      }
      
      _cpp_dialog->show();
      _cpp_dialog->raise();

      update_cpp_editor(plugin);
    }
  }

  void edit_options(void){
    SoundPlugin *plugin = (SoundPlugin*)_patch->patchdata;
    if (plugin!=NULL) {
      if (_options_dialog==NULL){
        _options_dialog = new RememberGeometryQDialog(this, radium::NOT_MODAL);
        QHBoxLayout *mainLayout = new QHBoxLayout;
      
        _options_dialog->setLayout(mainLayout);
        
        _options_editor = create_faust_editor(_options_dialog);

        mainLayout->addWidget(_options_editor);
        
        _options_dialog->resize(600,400);
      }
      
      _options_editor->setText(faust_disp_get_options(plugin));
      
      _options_dialog->show();
      _options_dialog->raise();
    }
  }

  void hideEvent(QHideEvent * event) override {
    RETURN_IF_DATA_IS_INACCESSIBLE();
    
    _size_type_before_hidden = _size_type;
    
    if(_size_type!=SIZETYPE_NORMAL)
      set_small(); // If not, all instrument widgets will have large height, maybe
  }

  void showEvent(QShowEvent * event) override {
    RETURN_IF_DATA_IS_INACCESSIBLE();
    
    if (_size_type_before_hidden != SIZETYPE_NORMAL)
      set_large(_size_type_before_hidden);
  }

public slots:

  #if 0
  void on_splitter_splitterMoved(int pos, int index){
    int webWidth = splitter->width() - _faust_editor->width() -  10;

    printf("Splitter moved to pos %d. Full width: %d, faust_width: %d, web: %d\n", pos, splitter->width(),_faust_editor->width(), webWidth);

    if (webWidth > 10){
      svg_view->resize(webWidth,svg_view->height());//setMinimumWidth(webWidth);
      svg_view->setMaximumWidth(webWidth+10);
    }
  }
  #endif
  
  void a_on__faust_editor_cursorPositionChanged(int new_line, int new_index){
    _prev_cursor_line = _cursor_line;
    _prev_cursor_index = _cursor_index;
    
    _cursor_line = new_line;
    _cursor_index = new_index;
    
    //_faust_editor->getCursorPosition(&_cursor_line, &_cursor_index);
    //printf("Cursor pos changed to %d\n", _cursor_line);
  }

  /*
  void a_on__faust_editor_linesChanged(){
    _faust_editor->updateMarginWidth();
  }
  */
  
  void a_on__faust_editor_textChanged(){
    //printf("Text changed. pos: %d\n",0);//_faust_editor->textCursor().position());
    if (!_initing){
      SoundPlugin *plugin = (SoundPlugin*)_patch->patchdata;
      if (plugin!=NULL) {
        QString new_code = _faust_editor->text();
        if (new_code != ""){ // <-- QScintilla sometimes gives us empty string in _faust_editor->text() (when there shouldn't be).
          QString old_code = faust_disp_get_code(plugin);
          start_compilation(new_code);
          ADD_UNDO(FaustDev_CurrPos(_patch.data(), old_code, _prev_cursor_line, _prev_cursor_index)); // note: _prev_cursor_pos is not correct when pasting something.
        }
      }
    }
  }
  
};

}


/*
// Doesn't work so well. The gui is deleted at inconvenient times.
QString FAUSTGUI_get_code(QWidget *widget){
  Faust_Plugin_widget *f = dynamic_cast<Faust_Plugin_widget*>(widget);
  R_ASSERT_RETURN_IF_FALSE2(f!=NULL, "");
  return f->_faust_editor->text();
}
*/


namespace{
  struct Undo_FaustDev{
    struct Patch *patch;
    const wchar_t *code; // this is clumsy. Should investigate the time finding out if there are the quirks using bdw-gc with c++.
    int cursor_line;
    int cursor_index;
  };
}

static void *Undo_Do_FaustDev(
                              struct Tracker_Windows *window,
                              struct WBlocks *wblock,
                              struct WTracks *wtrack,
                              int realline,
                              void *pointer
                              );

static void ADD_UNDO_FUNC(FaustDev_CurrPos(struct Patch *patch, const QString &code, int cursor_line, int cursor_index)){
  struct Tracker_Windows *window = root->song->tracker_windows;
  struct WBlocks *wblock = window->wblock;
  
  static double last_undo_block_time = -1000;
  double time_now = TIME_get_ms();

  static Patch *last_patch = NULL;
  static WBlocks *last_wblock = NULL;
  
  if( (time_now-last_undo_block_time) > 1500 // more than 1.5 seconds.
      || wblock!=last_wblock
      || last_patch != patch
      || Undo_get_last_function()!=Undo_Do_FaustDev
      )
    {
      Undo_FaustDev *undo_fd = (Undo_FaustDev*)talloc(sizeof(Undo_FaustDev));
      undo_fd->patch = patch;
      undo_fd->code = STRING_create(code);
      undo_fd->cursor_line = cursor_line;
      undo_fd->cursor_index = cursor_index;

      Undo_Add_dont_stop_playing(
                                 window->l.num,
                                 wblock->l.num,
                                 wblock->wtrack->l.num,
                                 wblock->curr_realline,
                                 undo_fd,
                                 Undo_Do_FaustDev,
                                 "FaustDevCodeChange"
                                 );
      last_patch = patch;
      last_wblock = wblock;
      last_undo_block_time = time_now;
    }
}

static void *Undo_Do_FaustDev(
                            struct Tracker_Windows *window,
                            struct WBlocks *wblock,
                            struct WTracks *wtrack,
                            int realline,
                            void *pointer
                            )
{
  struct Undo_FaustDev *undo_fd=(Undo_FaustDev*)pointer;
  struct Patch *patch = undo_fd->patch;
  SoundPlugin *plugin = (SoundPlugin*)patch->patchdata;
  
  Audio_instrument_widget *audio_instrument_widget = get_audio_instrument_widget(patch);
  R_ASSERT_RETURN_IF_FALSE2(audio_instrument_widget!=NULL, undo_fd);
    
  Faust_Plugin_widget *faust_plugin_widget = AUDIOWIDGET_get_faust_plugin_widget(audio_instrument_widget);
  R_ASSERT_RETURN_IF_FALSE2(faust_plugin_widget!=NULL, undo_fd);

  const wchar_t *new_code = STRING_create(faust_disp_get_code(plugin));

  int new_cursor_line, new_cursor_index;
  faust_plugin_widget->_faust_editor->getCursorPosition(&new_cursor_line, &new_cursor_index);
  
  
  faust_plugin_widget->_initing = true;{

    QString undo_code = STRING_get_qstring(undo_fd->code);
    
    faust_plugin_widget->set_text_in__faust_editor_widget(undo_code);

    //  Chaos trying to set cursor in qscisintella. :-(
    //faust_plugin_widget->_faust_editor->setCursorPosition(undo_fd->cursor_line, undo_fd->cursor_index);
    
    faust_plugin_widget->start_compilation(undo_code);
    
  }faust_plugin_widget->_initing = false;

  
  undo_fd->code = new_code;
  undo_fd->cursor_line = new_cursor_line;
  undo_fd->cursor_index = new_cursor_index;

  return undo_fd;
}

//// UNDO END
