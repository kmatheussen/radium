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
#include <QHBoxLayout>
#include <QLineEdit>
#include <QPushButton>
#include <QCheckBox>
#include <QComboBox>
#include <QSpinBox>
#include <QRadioButton>
#include <QButtonGroup>
#include <QJsonArray>
#include <QJsonDocument>
#include <QStringList>
#include <QFile>

#include "../api/radium_proc.h"
#include <atomic>
#include <memory>

#include <QSvgWidget>
#include <QLabel>
#include <QPlainTextEdit>
#include <QVBoxLayout>
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
#include "Qt_mix_colors.h"
#include "Editor.hpp"
#include "LLM_client.hpp"



static void ADD_UNDO_FUNC(FaustDev_CurrPos(struct Patch *patch, const QString &code, int cursor_line, int cursor_index));


#include "Qt_faust_plugin_widget.h"

namespace
{
  
class Faust_Plugin_widget;

// All live Faust_Plugin_widget instances, so a global toggle (the
// "Show/hide Faust Dev 2 LLM prompt" beta menu entry) can update them.
static QList<QPointer<Faust_Plugin_widget>> g_faust_plugin_widgets;

// Icon for the "Generate" button: a filled green play triangle.
static QIcon llm_play_icon(int size)
{
	QPixmap pixmap(size, size);
	pixmap.fill(Qt::transparent);

	{
		QPainter p(&pixmap);
		p.setRenderHints(QPainter::Antialiasing, true);

		QPolygonF triangle;
		triangle << QPointF(size * 0.15, size * 0.10)
		         << QPointF(size * 0.15, size * 0.90)
		         << QPointF(size * 0.90, size * 0.50);

		p.setPen(Qt::NoPen);
		p.setBrush(QColor(0x4C, 0xAF, 0x50));
		p.drawPolygon(triangle);
	}

	return QIcon(pixmap);
}

// Icon for the "Cancel" button: a filled red stop-sign octagon.
static QIcon llm_stop_icon(int size)
{
	QPixmap pixmap(size, size);
	pixmap.fill(Qt::transparent);

	{
		QPainter p(&pixmap);
		p.setRenderHints(QPainter::Antialiasing, true);

		QPolygonF octagon;
		for (int i = 0; i < 8; i++)
		{
			const double angle = (i + 0.5) * 0.7853981633974483; // pi / 4
			const double radius = size * 0.48;
			octagon << QPointF(size * 0.5 + radius * cos(angle),
			                   size * 0.5 + radius * sin(angle));
		}

		p.setPen(Qt::NoPen);
		p.setBrush(QColor(0xE0, 0x40, 0x40));
		p.drawPolygon(octagon);
	}

	return QIcon(pixmap);
}

// How many prompts the prompt history (persisted across sessions) remembers.
static const int g_llm_prompt_history_max_size = 50;

static const char *g_llm_prompt_history_settings_key = "faustdev2_llm_prompt_history";

// The prompt history is stored in the settings as a compact JSON array of
// strings so that it survives across sessions.
static QStringList load_llm_prompt_history_from_settings(void)
{
	QStringList history;
	const QString raw = SETTINGS_read_string(g_llm_prompt_history_settings_key, "");

	if (!raw.isEmpty())
	{
		const QJsonDocument doc = QJsonDocument::fromJson(raw.toUtf8());
		if (doc.isArray())
		{
			const QJsonArray array = doc.array();
			for (const QJsonValue &value : array)
				if (value.isString())
					history.append(value.toString());
		}
	}

	while (history.size() > g_llm_prompt_history_max_size)
		history.removeFirst();

	return history;
}

static void save_llm_prompt_history_to_settings(const QStringList &history)
{
	QStringList bounded_history = history;
	while (bounded_history.size() > g_llm_prompt_history_max_size)
		bounded_history.removeFirst();

	SETTINGS_write_string(g_llm_prompt_history_settings_key,
	                      QString(QJsonDocument(QJsonArray::fromStringList(bounded_history)).toJson(QJsonDocument::Compact)));
}

struct FaustResultSvgView
	: public QSvgWidget
	, public radium::MouseCycleFix
{
	
	FaustResultSvgView(QWidget *parent)
		: QSvgWidget(parent)
	{
		setMouseTracking(true);

		// The error overlay fills the whole view, so a layout owns its
		// geometry: manual setGeometry tied to resize events did not follow
		// the Half/Full size changes reliably.
		_errorView = new QPlainTextEdit(this);
		_errorView->setReadOnly(true);
		_errorView->setFrameStyle(QFrame::NoFrame);
		_errorView->setStyleSheet(QStringLiteral("QPlainTextEdit { background: white; padding: 8px; color: black; }"));
		_errorView->setAutoFillBackground(true);
		_errorView->hide();

		QVBoxLayout *error_layout = new QVBoxLayout(this);
		error_layout->setContentsMargins(0, 0, 0, 0);
		error_layout->addWidget(_errorView);
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
	QPlainTextEdit *_errorView = nullptr;  // scrollable overlay for error messages (replaces setHtml)

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

		_errorView->setPlainText(text);
		_errorView->show();
	}

	void setUrl(const QUrl &url)
	{
		if (_errorView)
			_errorView->hide();
		
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

	// Clears the navigation state after a recompile, when the view is
	// (re)loaded with the root process.svg. The breadcrumb strip at the
	// top is only meaningful while browsing submodule svgs.
	void reset_navigation(void)
	{
		_navStack.clear();
		_segmentRects.clear();
		_hoveredSegment = -1;
		_lineMinY = 0;
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
	}

	void paintEvent(QPaintEvent *event) override
	{
		if (_errorView && _errorView->isVisible())
		{
			QPainter p(this);
			p.fillRect(rect(), Qt::white);
			
			return;
		}

		QSvgWidget::paintEvent(event);

		if (_url.isEmpty() || renderer() == nullptr)
			return;

		QSize sz = size();
		if (sz.isEmpty() || renderer()->viewBoxF().isEmpty())
			return;

		// The strip sits in the space above the diagram lines. If the svg
		// has no <line> elements (e.g. a trivial root diagram), fall back
		// to a default height so the current svg name is always visible.
		double stripH;
		if (_lineMinY > 0)
			stripH = _lineMinY * sz.height() / renderer()->viewBoxF().height();
		else
			stripH = sz.height() * 0.06;

		_segmentRects.clear();

		QPainter painter(this);

		// The current svg is displayed as the last (non-clickable) segment.
		const int numSegments = _navStack.size() + 1;

		// Layout: each arrow gets dedicated gap space proportional to strip height
		double arrowGapW = stripH * 0.5;
		double totalArrowW = (numSegments - 1) * arrowGapW;
		double rectW = (sz.width() - totalArrowW) / (double)numSegments;
		double rectH = stripH / 2.0;  // half the strip height
		double rectY = stripH / 4.0; // centered vertically

		for (int i = 0; i < numSegments; i++)
		{
			double x = i * (rectW + arrowGapW);
			
			QRectF rect(x, rectY, rectW, rectH);

			const bool is_current = (i == _navStack.size());

			QString text = is_current ? _url.fileName() : _navStack[i].fileName();
			
			int dash = text.indexOf(QStringLiteral("-0x"));
			if (dash > 0)
				text = text.left(dash);
			
			if (!is_current)
				_segmentRects.append(rect);

			const QColor barely_off_white(0xEE, 0xF2, 0xF5);
			
			// Subtle background to indicate clickability. The current svg
			// name is not clickable, so instead of a filled background it
			// gets a nearly-black outline, a few pixels wide.
			if (is_current)
			{
				QPen outline(barely_off_white); //QColor(240, 240, 240));
				outline.setWidthF(rectH/10.0);
				painter.setPen(outline);
				painter.setBrush(Qt::NoBrush);
				rect.adjust(0,0,-10,0); // last rectangle goes outside svg.
			}
			else
			{
				painter.setPen(Qt::NoPen);
				if (i == _hoveredSegment)
					painter.setBrush(QColor(0xD2, 0xDC, 0xE6)); // light blue hover
				else
					painter.setBrush(barely_off_white);
			}
			
			painter.drawRect(rect);

			painter.setPen(QColor(0x33, 0x44, 0x55)); // blue-gray text
			myDrawText(painter,
					   rect,
					   text,
					   Qt::AlignCenter,
					   false, 0, true);
		}

		// Paint right-pointing arrows in the gaps between rectangles
		if (numSegments > 1)
		{
			painter.setPen(QColor(0x88, 0x99, 0xAA)); // lighter blue-gray
			
			for (int i = 0; i < numSegments - 1; i++)
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
extern QString FAUST2_get_default_code(void);
extern QString FAUST2_get_options(const struct SoundPlugin *plugin);
extern void FAUST2_generate_cpp_code(const struct SoundPlugin *plugin, int generation, std::function<void(int, QString)> callback);
extern QString FAUST2_get_error_message(const struct SoundPlugin *plugin);
extern QString FAUST2_get_svg_path(const struct SoundPlugin *plugin);
extern radium::FAUST_calledRegularlyByParentReply FAUST2_calledRegularlyByParent(struct SoundPlugin *plugin);
extern void FAUST2_start_compilation(struct SoundPlugin *plugin);
extern bool FAUST2_set_use_interpreter_backend(struct SoundPlugin *plugin, bool use_interpreter);
extern bool FAUST2_get_use_interpreter_backend(struct SoundPlugin *plugin);
extern QStringList FAUST2_lint_faust_code(const struct SoundPlugin *plugin, const QString &code);



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
static inline QStringList faust_disp_lint_faust_code(const SoundPlugin *plugin, const QString &code){
  if (!strcmp(plugin->type->type_name, "Faust Dev 2"))
    return FAUST2_lint_faust_code(plugin, code);
  else
    return QStringList(); // only Faust Dev 2 has the LLM prompt bar
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

  // Auto-fixing of LLM-generated code that fails to compile.
  bool _llm_fixing_error = false;   // an LLM-generated program is being auto-fixed
  int _llm_compile_attempts = 0;    // how many fix rounds have been done
  int _llm_max_fixes = 3;           // from llm_max_fixes setting
  QString _llm_original_prompt;     // the user's original request (LLM context)
  QString _llm_last_applied_code;   // last code applied by the LLM (detect manual edits/undo)
  bool _llm_applying_code = false;  // distinguishes LLM-applied edits from user edits
  QString _llm_last_fix_error;      // last compile error sent to the LLM (detect fix rounds that changed nothing)
  int _llm_same_error_count = 0;    // consecutive fix rounds ending in the same compile error
  QString _llm_lint_cache_code;         // code the cached lint findings below belong to
  bool _llm_lint_cache_compile_check = false; // whether the cached findings include the compile-based check
  QStringList _llm_lint_cache_findings; // static-analysis findings for that code (shared by the error pane and the LLM fix prompt)
  int _llm_last_progress_total = -1;      // chars shown in llm_status (throttling)
  bool _llm_last_progress_thinking = false; // whether llm_status showed "Thinking..."

  // Multi-turn conversation history (list of {"role","content"} messages) and
  // cancellation.
  QJsonArray _llm_history;
  std::shared_ptr<std::atomic_bool> _llm_cancel;

  // Prompt input history (like a shell's history): every submitted prompt is
  // remembered so the Up/Down arrow keys can re-insert previous prompts.
  QStringList _llm_prompt_history;
  int _llm_prompt_history_index = -1; // -1 = not browsing the history

  QString _llm_prompt_draft;          // text being edited before browsing started
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

    connect(prompt_edit, SIGNAL(returnPressed()), this, SLOT(a_on_generate_prompt_clicked()));
    connect(generate_button, SIGNAL(clicked()), this, SLOT(a_on_generate_prompt_clicked()));
    connect(cancel_button, SIGNAL(clicked()), this, SLOT(a_on_cancel_clicked()));
    cancel_button->setEnabled(false);
    prompt_edit->installEventFilter(this);

    // The hamburger menu replaces the former "New" and "LLM settings" buttons.
    {
      QMenu *menu = new QMenu(llm_menu_button);
      menu->addAction("New", this, [this]()
      {
        a_on_clear_history_clicked();
      });
      menu->addAction("LLM settings", this, [this]()
      {
        a_on_llm_settings_clicked();
      });
      llm_menu_button->setMenu(menu);
    }

    // The Instr./FX combo. The selection is compared by item text; if the
    // items are renamed, llm_combo_is_effect() asserts instead of silently
    // changing the behavior.
    llm_instr_fx_combo->addItem("Instr.");
    llm_instr_fx_combo->addItem("FX");

    // Icons for the Generate and Cancel buttons: a green play triangle and a
    // red stop-sign octagon.
    {
      const int icon_size = get_system_fontheight() * 4 / 3;

      generate_button->setIcon(llm_play_icon(icon_size));
      generate_button->setIconSize(QSize(icon_size, icon_size));

      cancel_button->setIcon(llm_stop_icon(icon_size));
      cancel_button->setIconSize(QSize(icon_size, icon_size));
    }

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

    // The LLM prompt bar is a Faust Dev 2-only feature. Hide it in Faust
    // Dev (1) (otherwise it would show whenever the beta feature is
    // enabled), and don't register in g_faust_plugin_widgets so the global
    // beta toggle never un-hides it.
    if (strcmp(plugin->type->type_name, "Faust Dev 2"))
    {
      llm_prompt_widget->hide();
    }
    else
    {
      // Start parsing the ~1.5 MB Faust library index in the background so the
      // first LLM request doesn't stall the GUI.
      radium::llm::load_library_index_background();

      // Prefetch the token prices from the relay (also triggers loading the
      // on-disk price cache), so the first request's cost is usually already
      // priced by the time its response arrives.
      radium::llm::llm_start_price_fetch();

      //set_llm_prompt_visible(SETTINGS_read_bool("faustdev2_llm_prompt_visible", false));
	  set_llm_prompt_visible(true);

      // Restore the prompt history (the last 50 submitted prompts) from
      // the settings so Up/Down navigation also works across sessions.
      _llm_prompt_history = load_llm_prompt_history_from_settings();

      g_faust_plugin_widgets.push_back(this);
    }
  }

  ~Faust_Plugin_widget() {
    g_faust_plugin_widgets.removeAll(this);
    /*
    SoundPlugin *plugin = (SoundPlugin*)_patch->patchdata;
    if (plugin!=NULL)
      FAUST_inform_about_instrument_gui(plugin, NULL);
    */
  }

  // Show/hide the LLM prompt bar (a beta feature; hidden unless explicitly
  // enabled via Help -> Beta features -> "Show/hide Faust Dev 2 LLM prompt").
  void set_llm_prompt_visible(bool visible){
    llm_prompt_widget->setVisible(visible);
  }

  bool eventFilter(QObject *watched, QEvent *event) override
  {
    if (watched == prompt_edit && event->type() == QEvent::KeyPress)
    {
      QKeyEvent *key_event = static_cast<QKeyEvent*>(event);
      if (key_event->key() == Qt::Key_Up)
        return navigate_llm_prompt_history(true);
      if (key_event->key() == Qt::Key_Down)
        return navigate_llm_prompt_history(false);
    }
    return QWidget::eventFilter(watched, event);
  }

  // Moves through the prompt history with Up (older) / Down (newer),
  // re-inserting the prompt into the widget like a shell's history. Returns
  // true if a history entry was shown.
  bool navigate_llm_prompt_history(bool up)
  {
    if (_llm_prompt_history.isEmpty())
      return false;

    if (up)
    {
      if (_llm_prompt_history_index == -1)
      {
        // Start browsing: remember the text being edited.
        _llm_prompt_draft = prompt_edit->text();
        _llm_prompt_history_index = _llm_prompt_history.size() - 1;
      }
      else if (_llm_prompt_history_index > 0)
        _llm_prompt_history_index--;
      else
        return true; // already at the oldest entry; still consume the key
    }
    else
    {
      if (_llm_prompt_history_index == -1)
        return false;
      _llm_prompt_history_index++;
      if (_llm_prompt_history_index >= _llm_prompt_history.size())
      {
        // Back to the draft the user was typing.
        _llm_prompt_history_index = -1;
        prompt_edit->setText(_llm_prompt_draft);
        prompt_edit->setCursorPosition(_llm_prompt_draft.size());
        return true;
      }
    }

    const QString prompt = _llm_prompt_history.at(_llm_prompt_history_index);
    prompt_edit->setText(prompt);
    prompt_edit->setCursorPosition(prompt.size());
    return true;
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

          const bool llm_compile_done = _llm_fixing_error;
          const bool llm_compile_was_fix = _llm_compile_attempts > 0;
          _llm_fixing_error = false;

          // The LLM status label says "...Compiling..." while an
          // LLM-generated program is being compiled; now that the
          // compilation succeeded, replace it with the final status.
          // If the cheap static checks still find suspicious lines
          // (e.g. a dry/wet expression that cancels out, or a slider
          // that is declared but never used - compilation succeeds, so
          // the auto-fix loop never sees them), send the findings back
          // to the LLM as a cleanup request instead of just warning
          // about them. The counters are only reset when the loop
          // truly ends (no findings left).
          if (llm_compile_done)
          {
            const QStringList lint_warnings = collect_lint_findings(plugin, _faust_editor->text(), false);
            if (!lint_warnings.isEmpty())
            {
              radium::llm::llm_log_note("LLM code compiled, static check findings:\n" + lint_warnings.join("\n"));
              request_llm_lint_cleanup(lint_warnings, llm_compile_was_fix); // keeps _llm_fixing_error=true while the cleanup round is in flight
            }
            else
            {
              if (llm_compile_was_fix)
                radium::llm::llm_log_note("Auto-fix/cleanup done - no static-check findings remain.");
              _llm_compile_attempts = 0;
              _llm_last_fix_error.clear();
              _llm_same_error_count = 0;
              set_llm_status(llm_compile_was_fix ? "Fixed." : "Generated.");
            }
          }

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
          
          update_gui(); // Refresh the new interface from the current DSP.
          
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
          
          _svg_view->reset_navigation();
          
          _svg_view->setUrl(QUrl::fromLocalFile(QDir::fromNativeSeparators(svg_path)));
          
          printf("    URL: -%s-. native: -%s-, org: -%s-\n",_svg_view->_url.toString().toUtf8().constData(), QDir::fromNativeSeparators(svg_path).toUtf8().constData(), faust_disp_get_svg_path(plugin).toUtf8().constData());

          update_cpp_editor(plugin);
        }
      }


      bool factory_failed = ready.factory_is_ready && ready.factory_succeeded==false;
      bool svg_failed = ready.svg_is_ready && ready.svg_succeeded==false;


      if (factory_failed || svg_failed){

        printf("   ERROR BLOCK: factory_failed=%d svg_failed=%d\n", factory_failed, svg_failed);
          
        const QString error_message = faust_disp_get_error_message(plugin);

        // Show the compiler error without the multi-KB dump of the inlined
        // signal graph, plus the static-analysis findings with exact line
        // numbers, so the error pane localizes the bug the same way the LLM
        // fix prompt does. (The compile-based check runs only for the
        // arity/composition error class: it takes ~0.5s and this code runs
        // on every failed compile, including half-written code while
        // typing. The textual checks are cheap and run for every error.)
        QString html_error = radium::llm::truncate_faust_error(error_message).toHtmlEscaped();
        html_error.replace("\n", "<br>");

        QString findings_html;
        if (factory_failed)
        {
          const bool arity_class = radium::llm::is_arity_error(error_message);
          const bool multiple_defs = error_message.contains("multiple definitions");

          // Textual findings (duplicate definitions, JS arrow syntax) are
          // cheap and help for every error class; the compile-based check
          // costs ~0.5s, so it runs only for the arity/composition class.
          const QStringList findings = collect_lint_findings(plugin, _faust_editor->text(), arity_class);
          if (!findings.isEmpty())
          {
            QStringList escaped_findings;
            for (const QString &finding : findings)
              escaped_findings.append(finding.toHtmlEscaped());
            findings_html =
              "<br><br>Static check of the program:<br>"
              + escaped_findings.join("<br>");
          }
          if (arity_class)
            findings_html +=
              "<br><br>(A signal-routing mismatch usually means a filter "
              "or smoother is used as a plain value instead of being "
              "applied to a signal with ':')";
          else if (multiple_defs && findings.isEmpty())
            findings_html +=
              "<br><br>(The same name may be defined more than once.)";
        }

        //_last_svg_view_frame->setScrollBarPolicy(Qt::Horizontal, Qt::ScrollBarAlwaysOff);
        _svg_view_text = 
                     "<!DOCTYPE html>"
                     "<html>"
                     "<body style=\"background-color:white;\"><big>"
                     +html_error
                     +findings_html
                     +"</big></body>"
                     "</html>"
          ;
        
        _svg_view->setHtml(_svg_view_text);

        // In large (Half/Full) mode the plugin's slider widget shares the
        // same vertical layout as the SVG/error view (stretch 4 vs 1), so
        // the error would only get ~80% of the height. Hide the sliders
        // while the error is shown: hidden widgets get no layout space.
        // (Normal mode is unaffected: there the sliders live in another tab.)
        if (_size_type != SIZETYPE_NORMAL && _plugin_widget != NULL)
          _plugin_widget->hide();

        // Feed the compiler error back to the LLM so it can fix the generated code.
        if (factory_failed
            && _llm_fixing_error
            && _llm_compile_attempts < _llm_max_fixes
            && _llm_last_applied_code == _faust_editor->text()) // code hasn't been manually edited/undone
        {
          const QString error_message = faust_disp_get_error_message(plugin);

          // If the error text is unchanged since the previous fix round, the
          // fix did not touch the failing expression. One identical repeat is
          // tolerated (the model may need to see the error again); after two
          // the loop is stopped instead of burning the remaining fix budget.
          if (error_message == _llm_last_fix_error)
            _llm_same_error_count++;
          else
            _llm_same_error_count = 0;

          if (_llm_same_error_count >= 2)
          {
            _llm_fixing_error = false;
            radium::llm::llm_log_note("Compile-error fix loop stopped - same error twice:\n" + error_message);
            set_llm_status("LLM fix attempts keep producing the same error. Giving up - edit the code manually.");
          }
          else
          {
            _llm_last_fix_error = error_message;
            _llm_compile_attempts++;
            request_llm_fix(error_message);
          }
        }else{
          _llm_fixing_error = false;
        }

      } else if (!_svg_view_text.isEmpty()) {
        
        // Error just cleared but SVG file wasn't ready — reload previous SVG to dismiss error overlay.
        printf("   ERROR CLEARED: reloading SVG from _url=%s\n", _svg_view->_url.toString().toUtf8().constData());
        _svg_view_text = "";
        if (_plugin_widget != NULL)
          _plugin_widget->show();
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
    
    // Keep the LLM prompt bar visible: it normally lives inside the Code tab
    // (tab_widget), which is hidden in large mode, so move it out to the top
    // of main_layout. Its explicit visible/hidden state (set by
    // set_llm_prompt_visible) is preserved across the layout move.
    main_layout->addWidget(llm_prompt_widget, 0, 0);
    main_layout->addWidget(code_widget, 1, 0);
    main_layout->setRowStretch(1, 1);
    
    tab_widget->hide();
    faust_webview_layout->addWidget(_plugin_widget, 1);

    // If the error is currently displayed, keep the sliders hidden so the
    // error view gets the full height of the webview column (see
    // calledRegularlyByParent).
    if (!showing_svg() && _plugin_widget != NULL)
      _plugin_widget->hide();
  }

  void set_small(void){
    _size_type = SIZETYPE_NORMAL;
    //_is_large = false;
    
    faust_interface_layout_radium->insertWidget(0, _plugin_widget);

    if (_plugin_widget != NULL)
      _plugin_widget->show(); // may have been hidden while an error was shown in large mode

    tab_develop_layout->insertWidget(0, llm_prompt_widget);
    tab_develop_layout->addWidget(code_widget);

    tab_widget->show();
  }
  
  void change_height(SizeType type)
  {
    if (type==_size_type)
      return;

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
      change_height(SIZETYPE_NORMAL); // If not, all instrument widgets will have large height, maybe
  }

  void showEvent(QShowEvent * event) override {
    RETURN_IF_DATA_IS_INACCESSIBLE();
    
    if (_size_type_before_hidden != SIZETYPE_NORMAL)
      change_height(_size_type_before_hidden);
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
      if (!_llm_applying_code){
        _llm_fixing_error = false; // A manual edit cancels auto-fixing of LLM code.
        _llm_history = QJsonArray(); // ...and invalidates the conversation history.
      }

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

   bool llm_combo_is_effect(void) const
   {
     const QString text = llm_instr_fx_combo->currentText();

     R_ASSERT_NON_RELEASE(text == "Instr." || text == "FX");

     return text == "FX";
   }

   void send_llm_generate_request(const QString &prompt_to_send)
   {
     SoundPlugin *plugin = (SoundPlugin*)_patch->patchdata;

     QString current_code = _faust_editor->text();
     if (current_code.isEmpty() && plugin != NULL)
       current_code = faust_disp_get_code(plugin);

     const radium::llm::LLMConfig config = radium::llm::get_config();

     const bool is_effect = llm_combo_is_effect();

     const QString code_for_request = radium::llm::is_creation_request(prompt_to_send)
                                      ? QString()
                                      : current_code;

    // If the current code fails to compile, tell the model why: the same
    // summarized error and static-analysis findings the fix prompt uses.
    // Only when the error belongs to the code being sent: the error must be
    // currently displayed (the last compile failed) and no compile may be
    // in flight (which would make the displayed error stale).
    QString compile_error;
    if (!code_for_request.isEmpty()
        && !_svg_view_text.isEmpty()
        && plugin != NULL
        && !faust_disp_is_compiling(plugin))
    {
      const QString error_message = faust_disp_get_error_message(plugin);
      const bool arity_class = radium::llm::is_arity_error(error_message);
      const QStringList findings = collect_lint_findings(plugin, current_code, arity_class);
      compile_error = radium::llm::summarize_faust_error(error_message);
      if (!findings.isEmpty())
        compile_error += "\n\nA local static check of the code above found these suspicious lines:\n"
          + findings.join("\n");
    }

    // The user message is NOT appended to the history here: send_request_once
    // adds it to the request itself, so appending it here too would duplicate
    // it (history + current message) in every request. It is appended below,
    // together with the assistant answer, only when the request succeeds.
    const QJsonArray history = _llm_history;
    const std::shared_ptr<std::atomic_bool> cancel = start_llm_request();

    IsAlive is_alive(this);

    radium::llm::send_prompt(config, code_for_request, prompt_to_send,
                             [is_alive, this, config, prompt_to_send, code_for_request, compile_error, current_code, is_effect](bool ok, QString result_or_error)
    {
      if (!is_alive)
        return;

      end_llm_request();

      if (ok && !current_code.trimmed().isEmpty()
          && result_or_error.simplified() == current_code.simplified())
      {
        // The model echoed the current program instead of answering the
        // request (possibly with only whitespace differences - simplified()
        // normalizes those), or recreated it for a creation request. Discard
        // it: applying would be a no-op, and retrying without the current
        // program would strip the context modification requests need - the
        // model then invents a new instrument and replaces the old one
        // (observed).
        printf("LLM: The model returned the current program unchanged. Discarding.\n");

        generate_button->setEnabled(true);
        set_llm_status("The model returned the current program unchanged - please rephrase the request.");
        return;
      }

      generate_button->setEnabled(true);

      if (ok)
      {
        _llm_history.append(QJsonObject{
          {QStringLiteral("role"), QStringLiteral("user")},
          {QStringLiteral("content"), radium::llm::build_full_user_content(code_for_request, prompt_to_send, config.library_context, false, compile_error, is_effect)},
        });
        _llm_history.append(QJsonObject{
          {QStringLiteral("role"), QStringLiteral("assistant")},
          {QStringLiteral("content"), result_or_error},
        });
        trim_llm_history();

        prompt_edit->clear();
        _llm_original_prompt = prompt_to_send;
        _llm_max_fixes = config.max_fixes < 0 ? 0 : config.max_fixes;
        _llm_compile_attempts = 0;
        _llm_last_fix_error.clear();
        _llm_same_error_count = 0;
        _llm_fixing_error = true;
        set_llm_status("Generated. Compiling...");
        apply_llm_code(result_or_error);
      }
      else
      {
        // Nothing was appended to the history for this failed attempt.
        _llm_fixing_error = false;
        show_llm_error(result_or_error);
      }
    },
                             history, cancel, 0.2,
                             [is_alive, this](int reasoning_chars, int content_chars)
    {
      if (!is_alive)
        return;
      update_llm_progress(reasoning_chars, content_chars);
    },
                             false, // skip_examples
                             compile_error,
                             is_effect);
  }

  void a_on_generate_prompt_clicked()
  {
    SoundPlugin *plugin = (SoundPlugin*)_patch->patchdata;
    if (plugin==NULL)
      return;

    const QString prompt = prompt_edit->text().trimmed();
    if (prompt.isEmpty())
      return;

    // Remember the submitted prompt for Up/Down arrow key navigation, and
    // persist the history (the last 50 prompts) across sessions.
    if (_llm_prompt_history.isEmpty() || _llm_prompt_history.last() != prompt)
      _llm_prompt_history.append(prompt);
    while (_llm_prompt_history.size() > g_llm_prompt_history_max_size)
      _llm_prompt_history.removeFirst();
    save_llm_prompt_history_to_settings(_llm_prompt_history);
    _llm_prompt_history_index = -1;

    const radium::llm::LLMConfig config = radium::llm::get_config();

    if (config.api_key.isEmpty() && config.mode != "free")
    {
      set_llm_status("No API key set. Click \"LLM settings\" to configure.");
      return;
    }

    generate_button->setEnabled(false);

    set_llm_status("\u2318 Generating...");
    send_llm_generate_request(prompt);
  }

  // Marks a new in-flight LLM request; cancels any previous one.
  std::shared_ptr<std::atomic_bool> start_llm_request(void)
  {
    if (_llm_cancel)
      *_llm_cancel = true;
    _llm_cancel = std::make_shared<std::atomic_bool>(false);
    cancel_button->setEnabled(true);
    _llm_last_progress_total = -1;
    _llm_last_progress_thinking = false;
    return _llm_cancel;
  }

  // Sets the LLM status text, appending the dollars spent so far in this
  // Radium session. The dollar display is only shown while token prices are
  // available (fetched from the relay); otherwise the cost is unknown and
  // nothing is appended.
  void set_llm_status(const QString &text)
  {
    if (radium::llm::llm_prices_available())
      llm_status->setText(text + "  |  " + radium::llm::llm_format_dollars(radium::llm::llm_session_cost()));
    else
      llm_status->setText(text);
  }

  // Shows the failure message in the status label, and pops up a proper
  // message window for server-side errors ("Error from LLM server: ..."),
  // which are too long for the tiny status label.
  void show_llm_error(const QString &message)
  {
	  if (message.contains("Error from LLM server:"))
	  {
		  //showAsyncMessage(QString("<p>" + message + "</p>").toUtf8().constData());
		  showAsyncMessage(message.toUtf8().constData());
		  set_llm_status("Error from LLM server");
	  }
	  else
	  {
		  set_llm_status(message);
	  }
  }

  // Live-updates the status label with how much the LLM has produced so far:
  // "Thinking..." while the model reasons, "Generating..." once it streams code.
  void update_llm_progress(int reasoning_chars, int content_chars)
  {
    const bool thinking = (reasoning_chars > 0 && content_chars == 0);
    const int total = reasoning_chars + content_chars;
    if (total == _llm_last_progress_total && thinking == _llm_last_progress_thinking)
      return; // throttled; nothing new to show
    _llm_last_progress_total = total;
    _llm_last_progress_thinking = thinking;
    if (thinking)
      set_llm_status(QString("Thinking... (%1 chars)").arg(reasoning_chars));
    else if (content_chars > 0)
      set_llm_status(QString("Generating... (%1 chars)").arg(content_chars));
  }

  void end_llm_request(void){
    _llm_cancel.reset();
    cancel_button->setEnabled(false);
  }

  // Keeps history bounded: at most ~6 user/assistant pairs and ~20K chars.
  void trim_llm_history(void){
    const int max_pairs = 6;
    while (_llm_history.size() > max_pairs * 2)
      _llm_history.removeFirst();

    int total = 0;
    for (const QJsonValue &message : _llm_history)
      total += message.toObject().value("content").toString().size();
    while (total > 20000 && _llm_history.size() > 2){
      total -= _llm_history.at(0).toObject().value("content").toString().size();
      _llm_history.removeFirst();
    }
  }

  // Applies LLM-generated code to the editor. The existing text-changed
  // handler records undo and starts compilation.
   void apply_llm_code(const QString &code){
     _llm_applying_code = true;{
       _initing = false;{
         set_text_in__faust_editor_widget(code);
       }_initing = false;
     }_llm_applying_code = false;
     _llm_last_applied_code = _faust_editor->text();
   }

  // Static-analysis findings for 'code': the textual checks (duplicate
  // definitions, JS arrow syntax), plus (when compile_check is true) the
  // exact per-definition compile check from audio/Faust_dev2.cpp. Cached
  // per code and check type so the error pane and the LLM fix prompt don't
  // run the compile-based check twice for the same failing code.
  QStringList collect_lint_findings(SoundPlugin *plugin, const QString &code, bool compile_check)
  {
    if (_llm_lint_cache_code == code && _llm_lint_cache_compile_check == compile_check)
      return _llm_lint_cache_findings;

    QStringList findings = radium::llm::lint_faust_code(code).split('\n', Qt::SkipEmptyParts);
    if (compile_check)
      findings += faust_disp_lint_faust_code(plugin, code);

    _llm_lint_cache_code = code;
    _llm_lint_cache_compile_check = compile_check;
    _llm_lint_cache_findings = findings;
    return findings;
  }

  // Sends the compiler error back to the LLM so it can fix the code.
  void request_llm_fix(const QString &error_message)
  {
    SoundPlugin *plugin = (SoundPlugin*)_patch->patchdata;
    if (plugin==NULL)
      return;

    QString current_code = _faust_editor->text();
    if (current_code.isEmpty())
      current_code = faust_disp_get_code(plugin);

    const QStringList lint_findings_list = collect_lint_findings(plugin, current_code, true);
    const QString lint_findings = lint_findings_list.join("\n");

    const QString fix_prompt =
      "The Faust compiler reported this error for the code above:\n"
      + radium::llm::summarize_faust_error(error_message) + "\n\n"
      + (lint_findings.isEmpty()
         ? QString()
         : QString("A local static check of the code above found these suspicious lines:\n")
           + lint_findings + "\n\n")
      + "The original request was: " + _llm_original_prompt + "\n\n"
      "Please fix the compile error and respond with ONLY the complete corrected Faust program.\n\n"
      "Before writing the fix, verify that every function call in the program "
      "has the exact number of arguments given in the library list. An arity "
      "error means one call has too many or too few arguments; fix that call "
      "and change nothing else.";

    const radium::llm::LLMConfig config = radium::llm::get_config();

    if (config.api_key.isEmpty() && config.mode != "free")
    {
      _llm_fixing_error = false;
      set_llm_status("No API key set. Cannot fix compile error.");
      return;
    }

    set_llm_status(QString("Compile error. Asking the LLM to fix it (%1/%2)...")
                   .arg(_llm_compile_attempts).arg(_llm_max_fixes));

    radium::llm::llm_log_note(QString("Sending compile-error fix round %1/%2:\n").arg(_llm_compile_attempts).arg(_llm_max_fixes) + radium::llm::truncate_faust_error(error_message));

    const std::shared_ptr<std::atomic_bool> cancel = start_llm_request();

    IsAlive is_alive(this);

    const bool is_effect = llm_combo_is_effect();

    // One fix attempt is fired. Only when it is useless - it returns the
    // failing program unchanged, or the request fails - is a second attempt
    // fired (higher temperature, so different sampling). Firing both in
    // parallel wasted a full stream on every round: the winner cancelled its
    // sibling mid-generation. DeepSeek's thinking mode ignores temperature,
    // so with thinking enabled the second attempt would be identical and is
    // skipped.
    const bool thinking_enabled = radium::llm::is_deepseek(config) && config.reasoning_effort != "off";
    std::shared_ptr<bool> used_fallback = std::make_shared<bool>(false);

    auto fix_progress = [is_alive, this](int reasoning_chars, int content_chars)
    {
      if (!is_alive)
        return;
      update_llm_progress(reasoning_chars, content_chars);
    };

    auto fix_callback = std::make_shared<std::function<void(bool, QString)>>();
    *fix_callback = [is_alive, this, cancel, current_code, used_fallback, thinking_enabled, config, fix_prompt, fix_progress, fix_callback, is_effect](bool ok, QString result_or_error)
    {
      if (!is_alive)
        return;

      if (ok && result_or_error.simplified() != current_code.simplified())
      {
        end_llm_request();
        set_llm_status("LLM fix received. Compiling...");
        apply_llm_code(result_or_error);
        return;
      }

      const QString failure_reason = ok
        ? "The LLM returned the failing program unchanged."
        : result_or_error;

      if (!*used_fallback && !thinking_enabled)
      {
        // The first attempt was useless (echo or request failure). One
        // retry with a higher temperature: different sampling can produce
        // a different - and working - fix. The prompt is escalated too:
        // when the model echoed the failing program, a plain re-ask often
        // just repeats it (observed), so the retry prompt spells out that
        // the returned code did NOT fix the error and that the fix must
        // actually change the failing expression.
        *used_fallback = true;
        set_llm_status("First fix attempt failed. Trying once more...");
        radium::llm::llm_log_note("Fix attempt failed (" + failure_reason + "), retrying hotter.");
        const QString retry_prompt =
          fix_prompt
          + "\n\n"
          + (ok
             ? QString("Your previous fix attempt returned the failing program UNCHANGED, "
                       "so the compile error is still present. Do NOT repeat the program "
                       "above: it does not compile.\n\n")
             : QString("The previous fix attempt failed. The compile error is still present. "
                       "Do NOT repeat the program above: it does not compile.\n\n"))
          + "Locate the exact expression the error message points at and change it. "
            "If the cause is unclear, replace the failing line(s) with a simpler "
            "equivalent construction - for example, drop the dry/wet helper and mix "
            "the dry and wet signals explicitly - and remove any definition that "
            "becomes unused.\n\n"
            "Respond with a DIFFERENT complete Faust program that fixes the error.";
        radium::llm::send_prompt(config, current_code, retry_prompt,
                                 *fix_callback,
                                 QJsonArray(), cancel, 0.7,
                                 fix_progress,
                                 true, // skip the example section: a fix corrects code, it doesn't need program examples
                                 QString(), // compile_error (already part of fix_prompt)
                                 is_effect);
        return;
      }

      end_llm_request();
      _llm_fixing_error = false;
      radium::llm::llm_log_note("Fix failed: " + failure_reason);
      if (failure_reason.contains("429"))
        show_llm_error("LLM quota exhausted (HTTP 429). Giving up on fixing the compile error.");
      else
        show_llm_error("LLM could not fix the compile error: " + failure_reason);
    };

    radium::llm::send_prompt(config, current_code, fix_prompt,
                             *fix_callback,
                             QJsonArray(), cancel, 0.2,
                             fix_progress,
                             true, // skip the example section: a fix corrects code, it doesn't need program examples
                             QString(), // compile_error (already part of fix_prompt)
                             is_effect);
  }

  // Sends the static-check findings back to the LLM when LLM-generated code
  // COMPILES but has suspicious lines (dead sliders, cancelling mix math):
  // compilation succeeds, so the compile-error fix loop never sees them.
  // Rounds share _llm_compile_attempts / _llm_max_fixes with the
  // compile-error fix loop, and stop when the findings repeat unchanged
  // (they contain line numbers, so identical text means nothing changed).
  // If the cleanup code fails to compile, the regular fix loop takes over
  // (the compiler error gets priority).
  void request_llm_lint_cleanup(const QStringList &findings, bool llm_compile_was_fix)
  {
    SoundPlugin *plugin = (SoundPlugin*)_patch->patchdata;
    if (plugin==NULL)
      return;

    QString current_code = _faust_editor->text();
    if (current_code.isEmpty())
      current_code = faust_disp_get_code(plugin);

    const QString findings_text = findings.join("\n");

    if (findings_text == _llm_last_fix_error)
      _llm_same_error_count++;
    else
      _llm_same_error_count = 0;

    _llm_last_fix_error = findings_text;

    if (_llm_same_error_count >= 2)
    {
      radium::llm::llm_log_note("Cleanup gave up - same findings twice:\n" + findings_text);
      set_llm_status("Cleanup gave up - the LLM keeps producing: " + findings.first());
      return;
    }

    if (_llm_compile_attempts >= _llm_max_fixes)
    {
      radium::llm::llm_log_note(QString("Cleanup gave up - round budget exhausted (%1/%2):\n").arg(_llm_compile_attempts).arg(_llm_max_fixes) + findings_text);
      set_llm_status("Cleanup round limit reached: " + findings.first());
      return;
    }

    _llm_compile_attempts++;
    _llm_fixing_error = true;

    const radium::llm::LLMConfig config = radium::llm::get_config();

    if (config.api_key.isEmpty() && config.mode != "free")
    {
      _llm_fixing_error = false;
      set_llm_status("No API key set. Cannot clean up the code.");
      return;
    }

    set_llm_status(QString("%1. Cleaning up (%2/%3)...")
                   .arg(llm_compile_was_fix ? "Fixed" : "Generated")
                   .arg(_llm_compile_attempts).arg(_llm_max_fixes));

    radium::llm::llm_log_note(QString("Sending lint cleanup round %1/%2:\n").arg(_llm_compile_attempts).arg(_llm_max_fixes) + findings_text);

    const QString cleanup_prompt =
      "The program compiles successfully, but a static check of the code above found these suspicious lines:\n"
      + findings_text + "\n\n"
      + "Fix ONLY the issues listed above and respond with ONLY the complete "
        "corrected Faust program. For example: remove a UI control that is "
        "declared but never used, or replace dry/wet mix math that cancels "
        "out. Do not change anything else.\n\n"
      + "The original request was: " + _llm_original_prompt;

    const std::shared_ptr<std::atomic_bool> cancel = start_llm_request();

    IsAlive is_alive(this);

    const bool is_effect = llm_combo_is_effect();

    // Same single-attempt + one hotter retry strategy as request_llm_fix:
    // an unchanged echo (or a failed request) gets one retry with a higher
    // temperature and an escalated prompt. DeepSeek's thinking mode ignores
    // temperature, so with thinking enabled the retry would be identical
    // and is skipped.
    const bool thinking_enabled = radium::llm::is_deepseek(config) && config.reasoning_effort != "off";
    std::shared_ptr<bool> used_fallback = std::make_shared<bool>(false);

    auto cleanup_progress = [is_alive, this](int reasoning_chars, int content_chars)
    {
      if (!is_alive)
        return;
      update_llm_progress(reasoning_chars, content_chars);
    };

    auto cleanup_callback = std::make_shared<std::function<void(bool, QString)>>();
    *cleanup_callback = [is_alive, this, cancel, current_code, used_fallback, thinking_enabled, config, cleanup_prompt, findings_text, cleanup_progress, cleanup_callback, is_effect](bool ok, QString result_or_error)
    {
      if (!is_alive)
        return;

      if (ok && result_or_error.simplified() != current_code.simplified())
      {
        end_llm_request();
        set_llm_status("Cleanup received. Compiling...");
        apply_llm_code(result_or_error);
        return;
      }

      const QString failure_reason = ok
        ? "The LLM returned the program unchanged."
        : result_or_error;

      if (!*used_fallback && !thinking_enabled)
      {
        *used_fallback = true;
        set_llm_status("Cleanup attempt failed. Trying once more...");
        radium::llm::llm_log_note("Cleanup attempt failed (" + failure_reason + "), retrying hotter.");

        const QString retry_prompt =
          cleanup_prompt
          + "\n\n"
          + (ok
             ? QString("Your previous cleanup attempt returned the program UNCHANGED, "
                       "so the issues listed above are still present. Do NOT repeat "
                       "the program above.\n\n")
             : QString("The previous cleanup attempt failed. The issues listed above "
                       "are still present. Do NOT repeat the program above.\n\n"))
          + "Remove or fix the exact suspicious lines listed above, and respond "
            "with a DIFFERENT complete Faust program.";

        radium::llm::send_prompt(config, current_code, retry_prompt,
                                 *cleanup_callback,
                                 QJsonArray(), cancel, 0.7,
                                 cleanup_progress,
                                 true, // skip the example section
                                 QString(), // compile_error
                                 is_effect);
        return;
      }

      end_llm_request();
      _llm_fixing_error = false;
      radium::llm::llm_log_note("Cleanup failed: " + failure_reason);
      if (failure_reason.contains("429"))
        show_llm_error("LLM quota exhausted (HTTP 429). Giving up on cleaning up the code.");
      else
        set_llm_status("Cleanup failed - remaining: " + findings_text.left(200));
    };

    radium::llm::send_prompt(config, current_code, cleanup_prompt,
                             *cleanup_callback,
                             QJsonArray(), cancel, 0.2,
                             cleanup_progress,
                             true, // skip the example section: a cleanup corrects code, it doesn't need program examples
                             QString(), // compile_error
                             is_effect);
  }

  // Resets the LLM prompt to the state of a newly created instrument:
  // cancels any in-flight request, clears the conversation history and all
  // generation state, and replaces the code with the default program.
  void a_on_clear_history_clicked(void){
    if (_llm_cancel)
      *_llm_cancel = true;
    _llm_cancel.reset();
    cancel_button->setEnabled(false);
    generate_button->setEnabled(true);

    _llm_history = QJsonArray();
    _llm_fixing_error = false;
    _llm_compile_attempts = 0;
    _llm_last_fix_error.clear();
    _llm_same_error_count = 0;
    _llm_last_applied_code.clear();
    _llm_original_prompt.clear();
    _llm_lint_cache_code.clear();
    _llm_lint_cache_findings.clear();
    _llm_last_progress_total = -1;
    _llm_last_progress_thinking = false;

    prompt_edit->clear();
    _llm_prompt_history_index = -1;
    _llm_prompt_draft.clear();

    // Replace the code with the default program. Going through the editor
    // triggers the normal text-changed path (undo entry + recompilation).
    set_text_in__faust_editor_widget(FAUST2_get_default_code());

    set_llm_status("New session.");
  }

  void a_on_cancel_clicked(void){
    if (_llm_cancel)
      *_llm_cancel = true;
    _llm_cancel.reset();
    cancel_button->setEnabled(false);
    generate_button->setEnabled(true);
    _llm_fixing_error = false;
    set_llm_status("Cancelled.");
  }

  void a_on_llm_settings_clicked()
  {
    const radium::llm::LLMConfig config = radium::llm::get_dialog_config();

    RememberGeometryQDialog *dialog = new RememberGeometryQDialog(this, radium::NOT_MODAL);
    dialog->setAttribute(Qt::WA_DeleteOnClose);
    dialog->setWindowTitle("LLM settings");

    QVBoxLayout *layout = new QVBoxLayout(dialog);

    auto add_row = [layout](const char *label, QWidget *edit)
    {
      layout->addWidget(new QLabel(label));
      layout->addWidget(edit);
    };

    QRadioButton *free_radio = new QRadioButton("Free");
    QRadioButton *custom_radio = new QRadioButton("Custom");
    QButtonGroup *mode_group = new QButtonGroup(dialog);
    mode_group->addButton(free_radio);
    mode_group->addButton(custom_radio);

    FocusSnifferQLineEdit *url_edit = new FocusSnifferQLineEdit;
    url_edit->setText(config.base_url);
    FocusSnifferQLineEdit *model_edit = new FocusSnifferQLineEdit;
    model_edit->setText(config.model);
    FocusSnifferQLineEdit *key_edit = new FocusSnifferQLineEdit;
    key_edit->setText(config.api_key);
    key_edit->setEchoMode(QLineEdit::Password);
    key_edit->setToolTip("The API key for your LLM provider. Required for a custom provider; Free mode uses the hosted relay and needs no key.");

    FocusSnifferQComboBox *effort_combo = new FocusSnifferQComboBox(NULL);
    effort_combo->addItem("Off (fastest)", QString("off"));
    effort_combo->addItem("Low", QString("low"));
    effort_combo->addItem("High (most careful)", QString("high"));
    const int effort_index = effort_combo->findData(config.reasoning_effort);
    effort_combo->setCurrentIndex(effort_index >= 0 ? effort_index : 0);
    effort_combo->setToolTip("How much the model 'thinks' before generating code. "
                             "Off is fastest. High is most careful (the reasoning is printed to stdout). "
                             "Applies to DeepSeek and to OpenAI reasoning models (gpt-5 / o-series); "
                             "other providers always respond directly and ignore this setting.");

    FocusSnifferQComboBox *library_context_combo = new FocusSnifferQComboBox(NULL);
    library_context_combo->addItem("Off", QString("off"));
    library_context_combo->addItem("Compact (recommended)", QString("compact"));
    library_context_combo->addItem("Full", QString("full"));
    const int library_index = library_context_combo->findData(config.library_context);
    library_context_combo->setCurrentIndex(library_index >= 0 ? library_index : 1);
    library_context_combo->setToolTip("Include the Faust library symbol table in the prompt. "
                                      "Compact lists only the commonly used modules (~half the size, faster/cheaper). "
                                      "Full lists everything. Either way the exact definitions of the functions in your code are added.");

    FocusSnifferQSpinBox *max_fixes_spinbox = new FocusSnifferQSpinBox;
    max_fixes_spinbox->setRange(0, 10);
    max_fixes_spinbox->setValue(config.max_fixes);
    max_fixes_spinbox->setToolTip("How many times to ask the LLM to fix a compile error before giving up (0 = never).");

    FocusSnifferQSpinBox *cutoff_high_spinbox = new FocusSnifferQSpinBox;
    cutoff_high_spinbox->setRange(1000, 100000);
    cutoff_high_spinbox->setValue(config.reasoning_cutoff_high);
    cutoff_high_spinbox->setToolTip("High thinking effort: a request that streams more reasoning characters than this without producing any code is aborted and retried at a lower thinking effort.");

    FocusSnifferQSpinBox *cutoff_low_spinbox = new FocusSnifferQSpinBox;
    cutoff_low_spinbox->setRange(1000, 100000);
    cutoff_low_spinbox->setValue(config.reasoning_cutoff_low);
    cutoff_low_spinbox->setToolTip("Low thinking effort: a request that streams more reasoning characters than this without producing any code is aborted and retried with thinking off.");

    QHBoxLayout *mode_row = new QHBoxLayout;
    mode_row->addWidget(free_radio);
    mode_row->addWidget(custom_radio);
    layout->addLayout(mode_row);

    add_row("Base URL:", url_edit);
    add_row("Model:", model_edit);
    add_row("API key:", key_edit);
    add_row("Thinking effort:", effort_combo);

    QLabel *cutoff_high_label = new QLabel("Reasoning cut-off, high effort (chars):");
    layout->addWidget(cutoff_high_label);
    layout->addWidget(cutoff_high_spinbox);

    QLabel *cutoff_low_label = new QLabel("Reasoning cut-off, low effort (chars):");
    layout->addWidget(cutoff_low_label);
    layout->addWidget(cutoff_low_spinbox);

#if 0 // Don't think we need to set this. The default probably always works best.
    add_row("Library context:", library_context_combo);
#endif
    add_row("Max compile fix attempts:", max_fixes_spinbox);

#if 0
    QPushButton *advanced_button = new QPushButton("Advanced");
    QObject::connect(advanced_button, &QPushButton::clicked, [dialog]()
    {
      // When llm.conf does not exist yet, seed it with the commented-out
      // example settings before opening the editor. NOTE: the conf path
      // must be built directly - OS_get_conf_filename2() exits the program
      // when the file is missing.
      const QString conf_filename = QDir(OS_get_dot_radium_path()).filePath(QStringLiteral("llm.conf"));
      if (!QFile::exists(conf_filename))
      {
        const QString default_filename = QCoreApplication::applicationDirPath() + "/default_llm.conf";
        if (!QFile::copy(default_filename, conf_filename))
          printf("LLM: Warning: could not copy -%s- to -%s-\n",
                 default_filename.toUtf8().constData(),
                 conf_filename.toUtf8().constData());
      }
      if (QFile::exists(conf_filename))
      {
        evalScheme("(FROM_C-show-llm-conf-editor)");
        dialog->close();
      }
      else
      {
        showAsyncMessage(QString("Could not open the llm.conf editor: -%1- does not exist and could not be created.").arg(conf_filename).toUtf8().constData());
      }
    });
    layout->addWidget(advanced_button);
#endif

    QPushButton *ok_button = new QPushButton("OK");
    layout->addWidget(ok_button);
	
    // The reasoning cut-offs only do anything for DeepSeek, so the rows are
    // hidden for OpenAI and other providers. Visibility follows the base URL
    // field (and the Free/Custom mode): Free mode always uses the DeepSeek
    // relay, so the rows are shown there regardless of the forced URL text.
    auto update_cutoff_rows_visibility = [cutoff_high_label, cutoff_high_spinbox, cutoff_low_label, cutoff_low_spinbox, url_edit, free_radio]()
    {
      const bool deepseek = free_radio->isChecked()
                            || url_edit->text().contains("deepseek", Qt::CaseInsensitive);

      cutoff_high_label->setVisible(deepseek);
      cutoff_high_spinbox->setVisible(deepseek);
      cutoff_low_label->setVisible(deepseek);
      cutoff_low_spinbox->setVisible(deepseek);
    };
    QObject::connect(url_edit, &QLineEdit::textChanged, update_cutoff_rows_visibility);
    update_cutoff_rows_visibility();

    // Every widget change is saved to settings immediately (no OK needed),
    // but the forced Free-mode displays ("<Radium server>", the model,
    // empty API key, "Off (fastest)") are display only and never saved: the
    // text fields use textEdited (fires only for user edits) and the effort
    // combo's forced/restored selections are blocked below.
    int last_effort_index = effort_combo->currentIndex();
    QString last_base_url = url_edit->text();
    QString last_model = model_edit->text();
    QString last_api_key = key_edit->text();
    QObject::connect(free_radio, &QRadioButton::toggled, [model_edit, key_edit, effort_combo, url_edit, last_effort_index, last_base_url, last_model, last_api_key](bool free_selected) mutable
    {
      model_edit->setEnabled(!free_selected);
      key_edit->setEnabled(!free_selected);
      url_edit->setEnabled(!free_selected);
      effort_combo->setEnabled(!free_selected);
      if (free_selected)
      {
        last_effort_index = effort_combo->currentIndex();
        {
          QSignalBlocker blocker(effort_combo);
          effort_combo->setCurrentIndex(0);
        }
        last_base_url = url_edit->text();
        url_edit->setText("<Radium server>");
        last_model = model_edit->text();
        model_edit->setText("deepseek-v4-flash");
        last_api_key = key_edit->text();
        key_edit->setText("");
      }
      else
      {
        if (last_effort_index >= 0)
        {
          QSignalBlocker blocker(effort_combo);
          effort_combo->setCurrentIndex(last_effort_index);
        }
        url_edit->setText(last_base_url);
        model_edit->setText(last_model);
        key_edit->setText(last_api_key);
      }
    });
    free_radio->setChecked(config.mode == "free");
    if (config.mode != "free")
      custom_radio->setChecked(true);

    // In Free mode several widgets are locked. The app palette's Disabled
    // group is clobbered when the global app stylesheet is set (it becomes
    // identical to the Active group), so a disabled widget would not look
    // any different. Give these widgets a clearly grayed-out Disabled group,
    // derived from the current theme colors, like updatePalette() does for
    // the widgets that existed when the palette was last applied.
    {
      const QColor active_text = dialog->palette().color(QPalette::Active, QPalette::Text);
      const QColor active_base = dialog->palette().color(QPalette::Active, QPalette::Base);
      const QColor active_window = dialog->palette().color(QPalette::Active, QPalette::Window);

      const QColor disabled_text = mix_colors(active_text, active_base, 0.5);
      const QColor disabled_base = mix_colors(active_base, active_window, 0.5);

      auto set_disabled_look = [disabled_text, disabled_base](QWidget *w)
      {
        QPalette pal = w->palette();
        pal.setColor(QPalette::Disabled, QPalette::Text, disabled_text);
        pal.setColor(QPalette::Disabled, QPalette::WindowText, disabled_text);
        pal.setColor(QPalette::Disabled, QPalette::ButtonText, disabled_text);
        pal.setColor(QPalette::Disabled, QPalette::Base, disabled_base);
        pal.setColor(QPalette::Disabled, QPalette::Button, disabled_base);
        pal.setColor(QPalette::Disabled, QPalette::Window, disabled_base);
        w->setPalette(pal);
      };

      set_disabled_look(url_edit);
      set_disabled_look(model_edit);
      set_disabled_look(key_edit);
      set_disabled_look(effort_combo);
    }

    QObject::connect(free_radio, &QRadioButton::toggled, [](bool free_selected)
    {
      SETTINGS_write_string("llm_mode", free_selected ? QString("free") : QString("custom"));
    });
    QObject::connect(url_edit, &QLineEdit::textEdited, [](const QString &text)
    {
      SETTINGS_write_string("llm_base_url", text.trimmed());
    });
    QObject::connect(model_edit, &QLineEdit::textEdited, [](const QString &text)
    {
      SETTINGS_write_string("llm_model", text.trimmed());
    });
	QObject::connect(key_edit, &QLineEdit::textEdited, [](const QString &text)
	{
		SETTINGS_make_config_file_private("llm_api_key");
		SETTINGS_write_string("llm_api_key", text.trimmed());
	});
    QObject::connect(effort_combo, &QComboBox::currentIndexChanged, [effort_combo](int index)
    {
      SETTINGS_write_string("llm_reasoning_effort", effort_combo->itemData(index).toString());
    });
    QObject::connect(library_context_combo, &QComboBox::currentIndexChanged, [library_context_combo](int index)
    {
      SETTINGS_write_string("llm_library_context", library_context_combo->itemData(index).toString());
    });
    QObject::connect(max_fixes_spinbox, &QSpinBox::valueChanged, [](int value)
    {
      SETTINGS_write_int("llm_max_fixes", value);
    });
    QObject::connect(cutoff_high_spinbox, &QSpinBox::valueChanged, [](int value)
    {
      SETTINGS_write_int("llm_reasoning_cutoff_high", value);
    });
    QObject::connect(cutoff_low_spinbox, &QSpinBox::valueChanged, [](int value)
    {
      SETTINGS_write_int("llm_reasoning_cutoff_low", value);
    });

    QObject::connect(ok_button, &QPushButton::released, [dialog]()
    {
      dialog->close();
    });

    dialog->show();
    dialog->raise();
    dialog->activateWindow();
  }
  
};

} // anon. namespace


#if 0
// Toggles the "Show/hide Faust Dev 2 LLM prompt" beta feature (menu entry in
// Help -> Beta features). Updates all open Faust Dev 2 widgets.
void showHideFaustDev2LLMPrompt(void){
  const bool visible = !SETTINGS_read_bool("faustdev2_llm_prompt_visible", false);
  SETTINGS_write_bool("faustdev2_llm_prompt_visible", visible);

  for (const QPointer<Faust_Plugin_widget> &widget : g_faust_plugin_widgets)
    if (widget)
      widget->set_llm_prompt_visible(visible);
}
#endif


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
