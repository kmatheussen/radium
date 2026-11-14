
#ifndef RADIUM_QT_MYSPLITTER_HPP
#define RADIUM_QT_MYSPLITTER_HPP

#include <QSplitter>
#include <QRegion>
#include <QResizeEvent>

namespace radium
{

#if 1

// The default grab area of a QSplitterHandle is only 4 or 5 pixels wide, and
// it only overlaps the widgets on each side of the splitter by a pixel or
// two. That makes the handle very hard to grab when the mouse pointer is a
// few pixels off, and impossible to grab when a native child window (such as
// the editor's OpenGL window) covers part of it. Make the grab area much
// larger than the visible handle, while keeping the visible handle unchanged.
class SplitterHandle : public QSplitterHandle
{
public:

	SplitterHandle(Qt::Orientation orientation, QSplitter *parent)
		: QSplitterHandle(orientation, parent)
	{
	}

protected:

	void resizeEvent(QResizeEvent *event) override
	{
		if (splitter()->handleWidth() >= 5)
		{
			QSplitterHandle::resizeEvent(event);
			return;
		}

		// This is the same as QSplitterHandle::resizeEvent(), but with a much
		// larger grab area than Qt's 4-5 pixels. (The visible handle is the
		// contents rect below, so it is unchanged.)
		//
		// Note: Do not call QSplitterHandle::resizeEvent() here. It resets the
		// contents margins to Qt's small values, which we would then change
		// again. QWidgetPrivate::updateContentsRect() sends a new resize event
		// every time the margins change, so doing that would recurse forever.
		setAttribute(Qt::WA_MouseNoMask, true);

		if (orientation() == Qt::Horizontal)
			setContentsMargins(5, 0, 5, 0);
		else
			setContentsMargins(0, 5, 0, 5);

		setMask(QRegion(contentsRect()));

		QWidget::resizeEvent(event);
	}
};

class Splitter : public QSplitter
{
	//  Q_OBJECT;

public:

	Splitter(Qt::Orientation orientation, QWidget *parent = Q_NULLPTR)
		: QSplitter(orientation, parent)
	{
	}

protected:

	QSplitterHandle *createHandle() override
	{
		return new SplitterHandle(orientation(), this);
	}
};

#else

class Splitter : public QWidget
{
	QVector<QWidget*> _children;

public:

	Splitter(Qt::Orientation orientation, QWidget *parent = Q_NULLPTR)
		: QWidget(parent)
	{
	}

	void setChildrenCollapsible(bool)
	{
	}

	int count() const
	{
		return _children.size();
	}
};

#endif
}

#endif
