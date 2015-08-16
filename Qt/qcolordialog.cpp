/****************************************************************************
**
** Implementation of QColorDialog class
**
** Created : 990222
**
** Copyright (C) 1999-2008 Trolltech ASA.  All rights reserved.
**
** This file is part of the dialogs module of the Qt GUI Toolkit.
**
** This file may be used under the terms of the GNU General
** Public License versions 2.0 or 3.0 as published by the Free
** Software Foundation and appearing in the files LICENSE.GPL2
** and LICENSE.GPL3 included in the packaging of this file.
** Alternatively you may (at your option) use any later version
** of the GNU General Public License if such license has been
** publicly approved by Trolltech ASA (or its successors, if any)
** and the KDE Free Qt Foundation.
**
** Please review the following information to ensure GNU General
** Public Licensing requirements will be met:
** http://trolltech.com/products/qt/licenses/licensing/opensource/.
** If you are unsure which license is appropriate for your use, please
** review the following information:
** http://trolltech.com/products/qt/licenses/licensing/licensingoverview
** or contact the sales department at sales@trolltech.com.
**
** This file may be used under the terms of the Q Public License as
** defined by Trolltech ASA and appearing in the file LICENSE.QPL
** included in the packaging of this file.  Licensees holding valid Qt
** Commercial licenses may use this file in accordance with the Qt
** Commercial License Agreement provided with the Software.
**
** This file is provided "AS IS" with NO WARRANTY OF ANY KIND,
** INCLUDING THE WARRANTIES OF DESIGN, MERCHANTABILITY AND FITNESS FOR
** A PARTICULAR PURPOSE. Trolltech reserves all rights not granted
** herein.
**
**********************************************************************/

//#ifdef USE_QT3

#include <stdio.h>

#include "qcolordialog.h"

#ifndef QT_NO_COLORDIALOG

#include "qpainter.h"
#include "qlayout.h"
#include "qlabel.h"
#include "qpushbutton.h"
#include "qlineedit.h"
#include "qimage.h"
#include "qpixmap.h"
#include "qdrawutil.h"
#include "qvalidator.h"
#include "q3dragobject.h"
#include "q3gridview.h"
#include "qapplication.h"
#include "qstyle.h"
#include "qsettings.h"
#include "q3popupmenu.h"
//Added by qt3to4:
#include <Q3HBoxLayout>
#include <QDragLeaveEvent>
#include <QKeyEvent>
#include <Q3GridLayout>
#include <Q3PointArray>
#include <Q3Frame>
#include <QDragEnterEvent>
#include <QDragMoveEvent>
#include <QDropEvent>
#include <QFocusEvent>
#include <QMouseEvent>
#include <Q3VBoxLayout>
#include <QPaintEvent>

#include "../OpenGL/Widget_proc.h"

#ifdef Q_WS_MAC
#  undef Q_WS_MAC
#endif

#ifdef Q_WS_MAC
QRgb macGetRgba( QRgb initial, bool *ok, QWidget *parent, const char* name );
QColor macGetColor( const QColor& initial, QWidget *parent, const char *name );
#endif

//////////// QWellArray BEGIN

struct QWellArrayData;

class QWellArray : public Q3GridView
{
    Q_OBJECT
    Q_PROPERTY( int selectedColumn READ selectedColumn )
    Q_PROPERTY( int selectedRow READ selectedRow )

public:
    QWellArray( QWidget* parent=0, const char* name=0, bool popup = FALSE );

    ~QWellArray() {}
    QString cellContent( int row, int col ) const;
    // ### Paul !!! virtual void setCellContent( int row, int col, const QString &);

    int selectedColumn() const { return selCol; }
    int selectedRow() const { return selRow; }

    virtual void setCurrent( int row, int col );
    virtual void setSelected( int row, int col );

    QSize sizeHint() const;

    virtual void setCellBrush( int row, int col, const QBrush & );
    QBrush cellBrush( int row, int col );

signals:
    void selected( int row, int col );

protected:
    void dimensionChange( int oldRows, int oldCols );

    virtual void paintCell( QPainter *, int row, int col );
    virtual void paintCellContents( QPainter *, int row, int col, const QRect& );

    void mousePressEvent( QMouseEvent* );
    void mouseReleaseEvent( QMouseEvent* );
    void mouseMoveEvent( QMouseEvent* );
    void keyPressEvent( QKeyEvent* );
    void focusInEvent( QFocusEvent* );
    void focusOutEvent( QFocusEvent* );

private:
    int curRow;
    int curCol;
    int selRow;
    int selCol;
    bool smallStyle;
public:
    QWellArrayData *d;

private:	// Disabled copy constructor and operator=
#if defined(Q_DISABLE_COPY)
    QWellArray( const QWellArray & );
    QWellArray& operator=( const QWellArray & );
#endif
};


// non-interface ...


struct QWellArrayData {
    QBrush *brush;
};

static QRgb cusrgb[2*8];
static QWellArray *custom_well_array = NULL;
static int curr_colornum=0;
static QColor curr_color;
#include "Qt_colors_proc.h"

static void update_curr_colornum(void){
  if(custom_well_array!=NULL){
    int row = custom_well_array->selectedRow();
    int col = custom_well_array->selectedColumn();
    if(row!=-1 && col!=-1)
      curr_colornum = col*2 + row;
    printf("setselected %d/%d. Curr color: %d\n",row, col,curr_colornum);
  }
}

static void updateRealtime(){
  testColorInRealtime(curr_colornum,curr_color);
  cusrgb[curr_colornum] = curr_color.rgb();
  custom_well_array->repaintContents(FALSE);
}

/*!
    \internal
    \class QWellArray qwellarray_p.h
    \brief The QWellArray class provides a well array.


    \ingroup advanced
*/

QWellArray::QWellArray( QWidget *parent, const char * name, bool popup )
    : Q3GridView( parent, name,
		 (popup ? (Qt::WStyle_Customize|Qt::WStyle_Tool|Qt::WStyle_NoBorder) : Qt::WStyle_NoBorder ) )
{
    d = 0;
    setFocusPolicy( Qt::StrongFocus );
    setVScrollBarMode(AlwaysOff);
    setHScrollBarMode(AlwaysOff);
    viewport()->setBackgroundMode( Qt::PaletteBackground );
    setNumCols( 7 );
    setNumRows( 7 );
    setCellWidth( 24 );
    setCellHeight( 21 );
    smallStyle = popup;

    if ( popup ) {
	setCellWidth( 18 );
	setCellHeight( 18 );
	setFrameStyle(Q3Frame::StyledPanel | Q3Frame::Raised);
	setMargin( 1 );
	setLineWidth( 2 );
    } else {
	setFrameStyle( Q3Frame::NoFrame );
    }
    curCol = 0;
    curRow = 0;
    selCol = -1;
    selRow = -1;

    if ( smallStyle )
	setMouseTracking( TRUE );
}


QSize QWellArray::sizeHint() const
{
    constPolish();
    QSize s = gridSize().boundedTo( QSize(640, 480 ) );
    return QSize( s.width() + 2*frameWidth(), s.height() + 2*frameWidth() );
}


void QWellArray::paintCell( QPainter* p, int row, int col )
{
    int w = cellWidth();			// width of cell in pixels
    int h = cellHeight();			// height of cell in pixels
    int b = 1;

    if ( !smallStyle )
	b = 3;

#if 0
    const QColorGroup & g = colorGroup();
#endif
    p->setPen( QPen( Qt::black, 0, Qt::SolidLine ) );
    if ( !smallStyle && row ==selRow && col == selCol
         //&& style().styleHint(QStyle::SH_GUIStyle) != Qt::MotifStyle
         ) {
	int n = 2;
	p->drawRect( n, n, w-2*n, h-2*n );
    }

#if 0
    style().drawPrimitive(QStyle::PE_Panel, p, QRect(b, b, w-2*b, h-2*b), g,
			  QStyle::State_Enabled | QStyle::State_Sunken);
#endif
    int t = 0;
#if 0
    if (style().styleHint(QStyle::SH_GUIStyle) == Qt::MotifStyle)
	t = ( row == selRow && col == selCol ) ? 2 : 0;
#endif
    b += 2 + t;

    if ( (row == curRow) && (col == curCol) ) {
	if ( smallStyle ) {
	    p->setPen ( Qt::white );
	    p->drawRect( 1, 1, w-2, h-2 );
	    p->setPen ( Qt::black );
	    p->drawRect( 0, 0, w, h );
	    p->drawRect( 2, 2, w-4, h-4 );
	    b = 3;
	} else if ( hasFocus() ) {
          //style().drawPrimitive(QStyle::PE_FocusRect, p, QRect(0, 0, w, h), g);
	}
    }
    paintCellContents( p, row, col, QRect(b, b, w - 2*b, h - 2*b) );
}

/*!
  Reimplement this function to change the contents of the well array.
 */
void QWellArray::paintCellContents( QPainter *p, int row, int col, const QRect &r )
{

    if ( d ) {
	p->fillRect( r, d->brush[row*numCols()+col] );
    } else {
	p->fillRect( r, Qt::white );
	p->setPen( Qt::black );
	p->drawLine( r.topLeft(), r.bottomRight() );
	p->drawLine( r.topRight(), r.bottomLeft() );
    }
}


/*\reimp
*/
void QWellArray::mousePressEvent( QMouseEvent* e )
{
    // The current cell marker is set to the cell the mouse is pressed
    // in.
    QPoint pos = e->pos();
    setCurrent( rowAt( pos.y() ), columnAt( pos.x() ) );
    updateRealtime();
}

/*\reimp
*/
void QWellArray::mouseReleaseEvent( QMouseEvent* )
{
    // The current cell marker is set to the cell the mouse is clicked
    // in.
    setSelected( curRow, curCol );
}


/*\reimp
*/
void QWellArray::mouseMoveEvent( QMouseEvent* e )
{
    //   The current cell marker is set to the cell the mouse is
    //   clicked in.
    if ( smallStyle ) {
	QPoint pos = e->pos();
	setCurrent( rowAt( pos.y() ), columnAt( pos.x() ) );
    }
    updateRealtime();
}

/*
  Sets the cell currently having the focus. This is not necessarily
  the same as the currently selected cell.
*/

void QWellArray::setCurrent( int row, int col )
{

    if ( (curRow == row) && (curCol == col) )
	return;

    if ( row < 0 || col < 0 )
	row = col = -1;

    int oldRow = curRow;
    int oldCol = curCol;

    curRow = row;
    curCol = col;

    updateCell( oldRow, oldCol );
    updateCell( curRow, curCol );
}


/*!
  Sets the currently selected cell to \a row, \a col.  If \a row or \a
  col are less than zero, the current cell is unselected.

  Does not set the position of the focus indicator.
*/
void QWellArray::setSelected( int row, int col )
{
    if ( (selRow == row) && (selCol == col) )
	return;

    int oldRow = selRow;
    int oldCol = selCol;

    if ( row < 0 || col < 0 )
	row = col = -1;

    selCol = col;
    selRow = row;

    updateCell( oldRow, oldCol );
    updateCell( selRow, selCol );
    if ( row >= 0 )
	emit selected( row, col );

#if 0
    if ( isVisible() && ::qt_cast<Q3PopupMenu*>(parentWidget()) )
	parentWidget()->close();
#endif

    update_curr_colornum();
}



/*!\reimp
*/
void QWellArray::focusInEvent( QFocusEvent* )
{
    updateCell( curRow, curCol );
}


/*!
  Sets the size of the well array to be \a rows cells by \a cols.
  Resets any brush information set by setCellBrush().
 */
void QWellArray::dimensionChange( int, int )
{
    if ( d ) {
	if ( d->brush )
	    delete[] d->brush;
	delete d;
	d = 0;
    }
}

void QWellArray::setCellBrush( int row, int col, const QBrush &b )
{
    if ( !d ) {
	d = new QWellArrayData;
	int i = numRows()*numCols();
	d->brush = new QBrush[i];
    }
    if ( row >= 0 && row < numRows() && col >= 0 && col < numCols() )
	d->brush[row*numCols()+col] = b;
#ifdef QT_CHECK_RANGE
    else
	qWarning( "QWellArray::setCellBrush( %d, %d ) out of range", row, col );
#endif
}



/*!
  Returns the brush set for the cell at \a row, \a col. If no brush is set,
  \c NoBrush is returned.
*/

QBrush QWellArray::cellBrush( int row, int col )
{
    if ( d && row >= 0 && row < numRows() && col >= 0 && col < numCols() )
	return d->brush[row*numCols()+col];
    return Qt::NoBrush;
}



/*!\reimp
*/

void QWellArray::focusOutEvent( QFocusEvent* )
{
    updateCell( curRow, curCol );
}

/*\reimp
*/
void QWellArray::keyPressEvent( QKeyEvent* e )
{
    switch( e->key() ) {			// Look at the key code
    case Qt::Key_Left:				// If 'left arrow'-key,
	if( curCol > 0 )			// and cr't not in leftmost col
	    setCurrent( curRow, curCol - 1);	// set cr't to next left column
	break;
    case Qt::Key_Right:				// Correspondingly...
	if( curCol < numCols()-1 )
	    setCurrent( curRow, curCol + 1);
	break;
    case Qt::Key_Up:
	if( curRow > 0 )
	    setCurrent( curRow - 1, curCol);
	else if ( smallStyle )
	    focusNextPrevChild( FALSE );
	break;
    case Qt::Key_Down:
	if( curRow < numRows()-1 )
	    setCurrent( curRow + 1, curCol);
	else if ( smallStyle )
	    focusNextPrevChild( TRUE );
	break;
    case Qt::Key_Space:
    case Qt::Key_Return:
    case Qt::Key_Enter:
	setSelected( curRow, curCol );
	break;
    default:				// If not an interesting key,
	e->ignore();			// we don't accept the event
	return;
    }

}

//////////// QWellArray END

static bool initrgb = FALSE;
static QRgb stdrgb[6*8];
static bool customSet = FALSE;


static void initRGB()
{
    if ( initrgb )
	return;
    initrgb = TRUE;
    int i = 0;
    for ( int g = 0; g < 4; g++ )
	for ( int r = 0;  r < 4; r++ )
	    for ( int b = 0; b < 3; b++ )
		stdrgb[i++] = qRgb( r*255/3, g*255/3, b*255/2 );

    for ( i = 0; i < 2*8; i++ )
	cusrgb[i] = qRgb(0xff,0xff,0xff);
}

/*!
    Returns the number of custom colors supported by QColorDialog3. All
    color dialogs share the same custom colors.
*/
int QColorDialog3::customCount()
{
    return 2*8;
}

/*!
    Returns custom color number \a i as a QRgb.
*/
QRgb QColorDialog3::customColor( int i )
{
    initRGB();
    if ( i < 0 || i >= customCount() ) {
#ifdef QT_CHECK_RANGE
	qWarning( "QColorDialog3::customColor() index %d out of range", i );
#endif
	i = 0;
    }
    return cusrgb[i];
}

/*!
    Sets custom color number \a i to the QRgb value \a c.
*/
void QColorDialog3::setCustomColor( int i, QRgb c )
{
    initRGB();
    if ( i < 0 || i >= customCount() ) {
#ifdef QT_CHECK_RANGE
	qWarning( "QColorDialog3::setCustomColor() index %d out of range", i );
#endif
	return;
    }
    customSet = TRUE;
    cusrgb[i] = c;
}

/*!
    Sets standard color number \a i to the QRgb value \a c.
*/

void QColorDialog3::setStandardColor( int i, QRgb c )
{
    initRGB();
    if ( i < 0 || i >= 6*8 ) {
#ifdef QT_CHECK_RANGE
	qWarning( "QColorDialog3::setStandardColor() index %d out of range", i );
#endif
	return;
    }
    stdrgb[i] = c;
}

static inline void rgb2hsv( QRgb rgb, int&h, int&s, int&v )
{
    QColor c;
    c.setRgb( rgb );
    c.getHsv(&h,&s,&v);
}

class QColorWell : public QWellArray
{
public:
    QColorWell( QWidget *parent, int r, int c, QRgb *vals )
	:QWellArray( parent, "" ), values( vals ), mousePressed( FALSE ), oldCurrent( -1, -1 )
    { setNumRows(r), setNumCols(c); setSizePolicy( QSizePolicy( QSizePolicy::Minimum, QSizePolicy::Minimum) ); }

protected:
    void paintCellContents( QPainter *, int row, int col, const QRect& );
    void mousePressEvent( QMouseEvent *e );
    void mouseMoveEvent( QMouseEvent *e );
    void mouseReleaseEvent( QMouseEvent *e );
#ifndef QT_NO_DRAGANDDROP
    void dragEnterEvent( QDragEnterEvent *e );
    void dragLeaveEvent( QDragLeaveEvent *e );
    void dragMoveEvent( QDragMoveEvent *e );
    void dropEvent( QDropEvent *e );
#endif

private:
    QRgb *values;
    bool mousePressed;
    QPoint pressPos;
    QPoint oldCurrent;

};

void QColorWell::paintCellContents( QPainter *p, int row, int col, const QRect &r )
{
    int i = row + col*numRows();
    p->fillRect( r, QColor( values[i] ) );
}

void QColorWell::mousePressEvent( QMouseEvent *e )
{
    oldCurrent = QPoint( selectedRow(), selectedColumn() );
    QWellArray::mousePressEvent( e );
    mousePressed = TRUE;
    pressPos = e->pos();
    updateRealtime();
}

void QColorWell::mouseMoveEvent( QMouseEvent *e )
{
    QWellArray::mouseMoveEvent( e );
#ifndef QT_NO_DRAGANDDROP
    if ( !mousePressed )
	return;
    if ( ( pressPos - e->pos() ).manhattanLength() > QApplication::startDragDistance() ) {
	setCurrent( oldCurrent.x(), oldCurrent.y() );
	int i = rowAt(pressPos.y()) + columnAt(pressPos.x()) * numRows();
	QColor col( values[ i ] );
	Q3ColorDrag *drg = new Q3ColorDrag( col, this );
	QPixmap pix( cellWidth(), cellHeight() );
	pix.fill( col );
	QPainter p( &pix );
	p.drawRect( 0, 0, pix.width(), pix.height() );
	p.end();
	drg->setPixmap( pix );
	mousePressed = FALSE;
	drg->dragCopy();
    }
#endif
    updateRealtime();
}

#ifndef QT_NO_DRAGANDDROP
void QColorWell::dragEnterEvent( QDragEnterEvent *e )
{
  // GL_lock is needed when using intel gfx driver to avoid crash caused by opening two opengl contexts simultaneously from two threads.
  GL_lock();{
    setFocus();
  }GL_unlock();

    if ( Q3ColorDrag::canDecode( e ) )
	e->accept();
    else
	e->ignore();
}

void QColorWell::dragLeaveEvent( QDragLeaveEvent * )
{

  if ( hasFocus() ) {
    // GL_lock is needed when using intel gfx driver to avoid crash caused by opening two opengl contexts simultaneously from two threads.
    GL_lock();{
      parentWidget()->setFocus();
    }GL_unlock();
  }
}

void QColorWell::dragMoveEvent( QDragMoveEvent *e )
{
    if ( Q3ColorDrag::canDecode( e ) ) {
	setCurrent( rowAt( e->pos().y() ), columnAt( e->pos().x() ) );
	e->accept();
    } else
	e->ignore();
}

void QColorWell::dropEvent( QDropEvent *e )
{
    if ( Q3ColorDrag::canDecode( e ) ) {
	int i = rowAt( e->pos().y() ) + columnAt( e->pos().x() ) * numRows();
	QColor col;
	Q3ColorDrag::decode( e, col );
	values[ i ] = col.rgb();
	repaintContents( FALSE );
	e->accept();
    } else {
	e->ignore();
    }
}

#endif // QT_NO_DRAGANDDROP

void QColorWell::mouseReleaseEvent( QMouseEvent *e )
{
    if ( !mousePressed )
	return;
    QWellArray::mouseReleaseEvent( e );
    mousePressed = FALSE;
}

class QColorPicker : public Q3Frame
{
    Q_OBJECT
public:
    QColorPicker(QWidget* parent=0, const char* name=0);
    ~QColorPicker();

public slots:
    void setCol( int h, int s );

signals:
    void newCol( int h, int s );

protected:
    QSize sizeHint() const;
    void drawContents(QPainter* p);
    void mouseMoveEvent( QMouseEvent * );
    void mousePressEvent( QMouseEvent * );

private:
    int hue;
    int sat;

    QPoint colPt();
    int huePt( const QPoint &pt );
    int satPt( const QPoint &pt );
    void setCol( const QPoint &pt );

    QPixmap *pix;
};

static int pWidth = 200;
static int pHeight = 200;

class QColorLuminancePicker : public QWidget
{
    Q_OBJECT
public:
    QColorLuminancePicker(QWidget* parent=0, const char* name=0);
    ~QColorLuminancePicker();

public slots:
    void setCol( int h, int s, int v );
    void setCol( int h, int s );

signals:
    void newHsv( int h, int s, int v );

protected:
    void paintEvent( QPaintEvent*);
    void mouseMoveEvent( QMouseEvent * );
    void mousePressEvent( QMouseEvent * );

private:
    enum { foff = 3, coff = 4 }; //frame and contents offset
    int val;
    int hue;
    int sat;

    int y2val( int y );
    int val2y( int val );
    void setVal( int v );

    QPixmap *pix;
};


int QColorLuminancePicker::y2val( int y )
{
    int d = height() - 2*coff - 1;
    return 255 - (y - coff)*255/d;
}

int QColorLuminancePicker::val2y( int v )
{
    int d = height() - 2*coff - 1;
    return coff + (255-v)*d/255;
}

QColorLuminancePicker::QColorLuminancePicker(QWidget* parent,
						  const char* name)
    :QWidget( parent, name )
{
    hue = 100; val = 100; sat = 100;
    pix = 0;
    //    setBackgroundMode( NoBackground );
}

QColorLuminancePicker::~QColorLuminancePicker()
{
    delete pix;
}

void QColorLuminancePicker::mouseMoveEvent( QMouseEvent *m )
{
    setVal( y2val(m->y()) );
    updateRealtime();
}
void QColorLuminancePicker::mousePressEvent( QMouseEvent *m )
{
    setVal( y2val(m->y()) );
    updateRealtime();
}

void QColorLuminancePicker::setVal( int v )
{
    if ( val == v )
	return;
    val = QMAX( 0, QMIN(v,255));
    delete pix; pix=0;
    repaint( FALSE ); //###
    emit newHsv( hue, sat, val );
}

//receives from a hue,sat chooser and relays.
void QColorLuminancePicker::setCol( int h, int s )
{
    setCol( h, s, val );
    emit newHsv( h, s, val );
}

void QColorLuminancePicker::paintEvent( QPaintEvent * )
{
    int w = width() - 5;
    QRect r( 0, foff, w, height() - 2*foff );
    int wi = r.width() - 2;
    int hi = r.height() - 2;
    if ( !pix || pix->height() != hi || pix->width() != wi ) {
	delete pix;
	QImage img( wi, hi, 32 );
	int y;
	for ( y = 0; y < hi; y++ ) {
          QColor c( hue, sat, y2val(y+coff), QColor::Hsv );
	    QRgb r = c.rgb();
	    int x;
	    for ( x = 0; x < wi; x++ )
		img.setPixel( x, y, r );
	}
	pix = new QPixmap;
	pix->convertFromImage(img);
    }
    QPainter p(this);
    p.drawPixmap( 1, coff, *pix );
    const QColorGroup &g = colorGroup();
    qDrawShadePanel( &p, r, g, TRUE );
    p.setPen( g.foreground() );
    p.setBrush( g.foreground() );

#if 1
    //Q3PointArray a;
    int y = val2y(val);
    QPointF points[3] = {QPointF(w, y),
                         QPointF(w+5, y+5),
                         QPointF(w+5, y-5)};
    //a.setPoints( 3, w, y, w+5, y+5, w+5, y-5 );
    //erase( w, 0, 5, height() ); // Fix?: This line (which I don't know (or can see) what does) made the next line crash.
    p.drawPolygon( points, 3 );
#endif
}

void QColorLuminancePicker::setCol( int h, int s , int v )
{
    val = v;
    hue = h;
    sat = s;
    delete pix; pix=0;
    repaint( FALSE );//####
}

QPoint QColorPicker::colPt()
{ return QPoint( (360-hue)*(pWidth-1)/360, (255-sat)*(pHeight-1)/255 ); }
int QColorPicker::huePt( const QPoint &pt )
{ return 360 - pt.x()*360/(pWidth-1); }
int QColorPicker::satPt( const QPoint &pt )
{ return 255 - pt.y()*255/(pHeight-1) ; }
void QColorPicker::setCol( const QPoint &pt )
{ setCol( huePt(pt), satPt(pt) ); }

QColorPicker::QColorPicker(QWidget* parent, const char* name )
    : Q3Frame( parent, name )
{
    hue = 0; sat = 0;
    setCol( 150, 255 );
    QImage img( pWidth, pHeight, 32 );
    int x,y;
    for ( y = 0; y < pHeight; y++ )
	for ( x = 0; x < pWidth; x++ ) {
	    QPoint p( x, y );
	    img.setPixel( x, y, QColor(huePt(p), satPt(p),
				       200, QColor::Hsv).rgb() );
	}
    pix = new QPixmap;
    pix->convertFromImage(img);
    setBackgroundMode( Qt::NoBackground );
    setSizePolicy( QSizePolicy( QSizePolicy::Fixed, QSizePolicy::Fixed )  );
}

QColorPicker::~QColorPicker()
{
    delete pix;
}

QSize QColorPicker::sizeHint() const
{
    return QSize( pWidth + 2*frameWidth(), pHeight + 2*frameWidth() );
}

void QColorPicker::setCol( int h, int s )
{
    int nhue = QMIN( QMAX(0,h), 360 );
    int nsat = QMIN( QMAX(0,s), 255);
    if ( nhue == hue && nsat == sat )
	return;
    QRect r( colPt(), QSize(20,20) );
    hue = nhue; sat = nsat;
    r = r.unite( QRect( colPt(), QSize(20,20) ) );
    r.moveBy( contentsRect().x()-9, contentsRect().y()-9 );
    //    update( r );
    repaint( r, FALSE );
}


void QColorPicker::mouseMoveEvent( QMouseEvent *m )
{
    QPoint p = m->pos() - contentsRect().topLeft();
    setCol( p );
    emit newCol( hue, sat );
    updateRealtime();
}

void QColorPicker::mousePressEvent( QMouseEvent *m )
{
    QPoint p = m->pos() - contentsRect().topLeft();
    setCol( p );
    emit newCol( hue, sat );
    updateRealtime();
}

void QColorPicker::drawContents(QPainter* p)
{
    QRect r = contentsRect();

    p->drawPixmap( r.topLeft(), *pix );
    QPoint pt = colPt() + r.topLeft();
    p->setPen( QPen(Qt::black) );

    p->fillRect( pt.x()-9, pt.y(), 20, 2, Qt::black );
    p->fillRect( pt.x(), pt.y()-9, 2, 20, Qt::black );

}

class QColorShowLabel;



class QColIntValidator: public QIntValidator
{
public:
    QColIntValidator( int bottom, int top,
		   QWidget * parent, const char *name = 0 )
	:QIntValidator( bottom, top, parent, name ) {}

    QValidator::State validate( QString &, int & ) const;
};

QValidator::State QColIntValidator::validate( QString &s, int &pos ) const
{
    State state = QIntValidator::validate(s,pos);
    if ( state == QValidator::Intermediate ) {
	long int val = s.toLong();
	// This is not a general solution, assumes that top() > 0 and
	// bottom >= 0
	if ( val < 0 ) {
	    s = "0";
	    pos = 1;
	} else if ( val > top() ) {
	    s.setNum( top() );
	    pos = s.length();
	}
    }
    return state;
}



class QColNumLineEdit : public QLineEdit
{
public:
    QColNumLineEdit( QWidget *parent, const char* name=0 )
	: QLineEdit( parent, name ) { setMaxLength( 3 );}
    QSize sizeHint() const {
	return QSize( fontMetrics().width( "999" ) + 2 * ( margin() + frameWidth() ),
		      QLineEdit::sizeHint().height() ); }
    void setNum( int i ) {
	QString s;
	s.setNum(i);
	bool block = signalsBlocked();
	blockSignals(TRUE);
	setText( s );
	blockSignals(block);
    }
    int val() const { return text().toInt(); }
};


class QColorShower : public QWidget
{
    Q_OBJECT
public:
    QColorShower( QWidget *parent, const char *name=0 );

    //things that don't emit signals
    void setHsv( int h, int s, int v );

    int currentAlpha() const { return alphaEd->val(); }
    void setCurrentAlpha( int a ) { alphaEd->setNum( a ); }
    void showAlpha( bool b );


    QRgb currentColor() const { return curCol; }

public slots:
    void setRgb( QRgb rgb );

signals:
    void newCol( QRgb rgb );
private slots:
    void rgbEd();
    void hsvEd();
private:
    void showCurrentColor();
    int hue, sat, val;
    QRgb curCol;
    QColNumLineEdit *hEd;
    QColNumLineEdit *sEd;
    QColNumLineEdit *vEd;
    QColNumLineEdit *rEd;
    QColNumLineEdit *gEd;
    QColNumLineEdit *bEd;
    QColNumLineEdit *alphaEd;
    QLabel *alphaLab;
    QColorShowLabel *lab;
    bool rgbOriginal;
};

class QColorShowLabel : public Q3Frame
{
    Q_OBJECT

public:
    QColorShowLabel( QWidget *parent ) : Q3Frame( parent, "qt_colorshow_lbl" ) {
	setFrameStyle( Q3Frame::Panel|Q3Frame::Sunken );
	setBackgroundMode( Qt::PaletteBackground );
	setAcceptDrops( TRUE );
	mousePressed = FALSE;
    }
    void setColor( QColor c ) { col = c; }

signals:
    void colorDropped( QRgb );

protected:
    void drawContents( QPainter *p );
    void mousePressEvent( QMouseEvent *e );
    void mouseMoveEvent( QMouseEvent *e );
    void mouseReleaseEvent( QMouseEvent *e );
#ifndef QT_NO_DRAGANDDROP
    void dragEnterEvent( QDragEnterEvent *e );
    void dragLeaveEvent( QDragLeaveEvent *e );
    void dropEvent( QDropEvent *e );
#endif

private:
    QColor col;
    bool mousePressed;
    QPoint pressPos;

};

void QColorShowLabel::drawContents( QPainter *p )
{
  p->fillRect( contentsRect(), col );
  curr_color = col;
}

void QColorShower::showAlpha( bool b )
{
    if ( b ) {
	alphaLab->show();
	alphaEd->show();
    } else {
	alphaLab->hide();
	alphaEd->hide();
    }
}

void QColorShowLabel::mousePressEvent( QMouseEvent *e )
{
    mousePressed = TRUE;
    pressPos = e->pos();
    updateRealtime();
}

void QColorShowLabel::mouseMoveEvent( QMouseEvent *e )
{
#ifndef QT_NO_DRAGANDDROP
    if ( !mousePressed )
	return;
    if ( ( pressPos - e->pos() ).manhattanLength() > QApplication::startDragDistance() ) {
	Q3ColorDrag *drg = new Q3ColorDrag( col, this );
	QPixmap pix( 30, 20 );
	pix.fill( col );
	QPainter p( &pix );
	p.drawRect( 0, 0, pix.width(), pix.height() );
	p.end();
	drg->setPixmap( pix );
	mousePressed = FALSE;
	drg->dragCopy();
    }
#endif
    updateRealtime();
}

#ifndef QT_NO_DRAGANDDROP
void QColorShowLabel::dragEnterEvent( QDragEnterEvent *e )
{
    if ( Q3ColorDrag::canDecode( e ) )
	e->accept();
    else
	e->ignore();
}

void QColorShowLabel::dragLeaveEvent( QDragLeaveEvent * )
{
}

void QColorShowLabel::dropEvent( QDropEvent *e )
{
    if ( Q3ColorDrag::canDecode( e ) ) {
	Q3ColorDrag::decode( e, col );
	repaint( FALSE );
	emit colorDropped( col.rgb() );
	e->accept();
    } else {
	e->ignore();
    }
}
#endif // QT_NO_DRAGANDDROP

void QColorShowLabel::mouseReleaseEvent( QMouseEvent * )
{
    if ( !mousePressed )
	return;
    mousePressed = FALSE;
}

QColorShower::QColorShower( QWidget *parent, const char *name )
    :QWidget( parent, name)
{
    curCol = qRgb( -1, -1, -1 );
    QColIntValidator *val256 = new QColIntValidator( 0, 255, this );
    QColIntValidator *val360 = new QColIntValidator( 0, 360, this );

    Q3GridLayout *gl = new Q3GridLayout( this, 1, 1, 6 );
    lab = new QColorShowLabel( this );
    lab->setMinimumWidth( 60 ); //###
    gl->addMultiCellWidget(lab, 0,-1,0,0);
    connect( lab, SIGNAL( colorDropped(QRgb) ),
	     this, SIGNAL( newCol(QRgb) ) );
    connect( lab, SIGNAL( colorDropped(QRgb) ),
	     this, SLOT( setRgb(QRgb) ) );

    hEd = new QColNumLineEdit( this, "qt_hue_edit" );
    hEd->setValidator( val360 );
    QLabel *l = new QLabel( hEd, QColorDialog3::tr("Hu&e:"), this, "qt_hue_lbl" );
    l->setAlignment( Qt::AlignRight|Qt::AlignVCenter );
    gl->addWidget( l, 0, 1 );
    gl->addWidget( hEd, 0, 2 );

    sEd = new QColNumLineEdit( this, "qt_sat_edit" );
    sEd->setValidator( val256 );
    l = new QLabel( sEd, QColorDialog3::tr("&Sat:"), this, "qt_sat_lbl" );
    l->setAlignment( Qt::AlignRight|Qt::AlignVCenter );
    gl->addWidget( l, 1, 1 );
    gl->addWidget( sEd, 1, 2 );

    vEd = new QColNumLineEdit( this, "qt_val_edit" );
    vEd->setValidator( val256 );
    l = new QLabel( vEd, QColorDialog3::tr("&Val:"), this, "qt_val_lbl" );
    l->setAlignment( Qt::AlignRight|Qt::AlignVCenter );
    gl->addWidget( l, 2, 1 );
    gl->addWidget( vEd, 2, 2 );

    rEd = new QColNumLineEdit( this, "qt_red_edit" );
    rEd->setValidator( val256 );
    l = new QLabel( rEd, QColorDialog3::tr("&Red:"), this, "qt_red_lbl" );
    l->setAlignment( Qt::AlignRight|Qt::AlignVCenter );
    gl->addWidget( l, 0, 3 );
    gl->addWidget( rEd, 0, 4 );

    gEd = new QColNumLineEdit( this, "qt_grn_edit" );
    gEd->setValidator( val256 );
    l = new QLabel( gEd, QColorDialog3::tr("&Green:"), this, "qt_grn_lbl" );
    l->setAlignment( Qt::AlignRight|Qt::AlignVCenter );
    gl->addWidget( l, 1, 3 );
    gl->addWidget( gEd, 1, 4 );

    bEd = new QColNumLineEdit( this, "qt_blue_edit" );
    bEd->setValidator( val256 );
    l = new QLabel( bEd, QColorDialog3::tr("Bl&ue:"), this, "qt_blue_lbl" );
    l->setAlignment( Qt::AlignRight|Qt::AlignVCenter );
    gl->addWidget( l, 2, 3 );
    gl->addWidget( bEd, 2, 4 );

    alphaEd = new QColNumLineEdit( this, "qt_aplha_edit" );
    alphaEd->setValidator( val256 );
    alphaLab = new QLabel( alphaEd, QColorDialog3::tr("A&lpha channel:"), this, "qt_alpha_lbl" );
    alphaLab->setAlignment( Qt::AlignRight|Qt::AlignVCenter );
    gl->addMultiCellWidget( alphaLab, 3, 3, 1, 3 );
    gl->addWidget( alphaEd, 3, 4 );
    alphaEd->hide();
    alphaLab->hide();

    connect( hEd, SIGNAL(textChanged(const QString&)), this, SLOT(hsvEd()) );
    connect( sEd, SIGNAL(textChanged(const QString&)), this, SLOT(hsvEd()) );
    connect( vEd, SIGNAL(textChanged(const QString&)), this, SLOT(hsvEd()) );

    connect( rEd, SIGNAL(textChanged(const QString&)), this, SLOT(rgbEd()) );
    connect( gEd, SIGNAL(textChanged(const QString&)), this, SLOT(rgbEd()) );
    connect( bEd, SIGNAL(textChanged(const QString&)), this, SLOT(rgbEd()) );
    connect( alphaEd, SIGNAL(textChanged(const QString&)), this, SLOT(rgbEd()) );
}

void QColorShower::showCurrentColor()
{
    lab->setColor( currentColor() );
    lab->repaint(FALSE); //###
}

void QColorShower::rgbEd()
{
    rgbOriginal = TRUE;
    if ( alphaEd->isVisible() )
	curCol = qRgba( rEd->val(), gEd->val(), bEd->val(), currentAlpha() );
    else
	curCol = qRgb( rEd->val(), gEd->val(), bEd->val() );

    rgb2hsv(currentColor(), hue, sat, val );

    hEd->setNum( hue );
    sEd->setNum( sat );
    vEd->setNum( val );

    showCurrentColor();
    emit newCol( currentColor() );
}

void QColorShower::hsvEd()
{
    rgbOriginal = FALSE;
    hue = hEd->val();
    sat = sEd->val();
    val = vEd->val();

    curCol = QColor( hue, sat, val, QColor::Hsv ).rgb();

    rEd->setNum( qRed(currentColor()) );
    gEd->setNum( qGreen(currentColor()) );
    bEd->setNum( qBlue(currentColor()) );

    showCurrentColor();
    emit newCol( currentColor() );
}

void QColorShower::setRgb( QRgb rgb )
{
    rgbOriginal = TRUE;
    curCol = rgb;

    rgb2hsv( currentColor(), hue, sat, val );

    hEd->setNum( hue );
    sEd->setNum( sat );
    vEd->setNum( val );

    rEd->setNum( qRed(currentColor()) );
    gEd->setNum( qGreen(currentColor()) );
    bEd->setNum( qBlue(currentColor()) );

    showCurrentColor();
}

void QColorShower::setHsv( int h, int s, int v )
{
    rgbOriginal = FALSE;
    hue = h; val = v; sat = s; //Range check###
    curCol = QColor( hue, sat, val, QColor::Hsv ).rgb();

    hEd->setNum( hue );
    sEd->setNum( sat );
    vEd->setNum( val );

    rEd->setNum( qRed(currentColor()) );
    gEd->setNum( qGreen(currentColor()) );
    bEd->setNum( qBlue(currentColor()) );

    showCurrentColor();
}

class QColorDialogPrivate : public QObject
{
Q_OBJECT
public:
    QColorDialogPrivate( QColorDialog3 *p );
    QRgb currentColor() const { return cs->currentColor(); }
    void setCurrentColor( QRgb rgb );

    int currentAlpha() const { return cs->currentAlpha(); }
    void setCurrentAlpha( int a ) { cs->setCurrentAlpha( a ); }
    void showAlpha( bool b ) { cs->showAlpha( b ); }

public slots:
    void addCustom();

    void newHsv( int h, int s, int v );
    void newColorTypedIn( QRgb rgb );
    void newCustom( int, int );
    void newStandard( int, int );
public:
    QWellArray *custom;
    QWellArray *standard;

    QColorPicker *cp;
    QColorLuminancePicker *lp;
    QColorShower *cs;
    int nextCust;
    bool compact;
};

//sets all widgets to display h,s,v
void QColorDialogPrivate::newHsv( int h, int s, int v )
{
    cs->setHsv( h, s, v );
    cp->setCol( h, s );
    lp->setCol( h, s, v );
}

//sets all widgets to display rgb
void QColorDialogPrivate::setCurrentColor( QRgb rgb )
{
    cs->setRgb( rgb );
    newColorTypedIn( rgb );
}

//sets all widgets exept cs to display rgb
void QColorDialogPrivate::newColorTypedIn( QRgb rgb )
{
    int h, s, v;
    rgb2hsv(rgb, h, s, v );
    cp->setCol( h, s );
    lp->setCol( h, s, v);
}

void QColorDialogPrivate::newCustom( int r, int c )
{
    int i = r+2*c;
    setCurrentColor( cusrgb[i] );
    nextCust = i;
    if (standard)
        standard->setSelected(-1,-1);
}

void QColorDialogPrivate::newStandard( int r, int c )
{
    setCurrentColor( stdrgb[r+c*6] );
    if (custom)
        custom->setSelected(-1,-1);
}

QColorDialogPrivate::QColorDialogPrivate( QColorDialog3 *dialog ) :
    QObject(dialog)
{
    compact = FALSE;
    // small displays (e.g. PDAs cannot fit the full color dialog,
    // so just use the color picker.
#if 0
    if ( qApp->desktop()->width() < 480 || qApp->desktop()->height() < 350 )
	compact = TRUE;
#endif

    nextCust = 0;
    const int lumSpace = 3;
    int border = 12;
    if ( compact )
	border = 6;
    Q3HBoxLayout *topLay = new Q3HBoxLayout( dialog, border, 6 );
    Q3VBoxLayout *leftLay = 0;

    if ( !compact )
	leftLay = new Q3VBoxLayout( topLay );

    initRGB();

    if ( !compact ) {
	standard = new QColorWell( dialog, 6, 8, stdrgb );
	standard->setCellWidth( 28 );
	standard->setCellHeight( 24 );
	QLabel * lab = new QLabel( standard,
				QColorDialog3::tr( "&Basic colors"), dialog, "qt_basiccolors_lbl" );
	connect( standard, SIGNAL(selected(int,int)), SLOT(newStandard(int,int)));
	leftLay->addWidget( lab );
	leftLay->addWidget( standard );


	leftLay->addStretch();

	custom = new QColorWell( dialog, 2, 8, cusrgb );
	custom->setCellWidth( 28 );
	custom->setCellHeight( 24 );
	custom->setAcceptDrops( TRUE );

	connect( custom, SIGNAL(selected(int,int)), SLOT(newCustom(int,int)));
	lab = new QLabel( custom, QColorDialog3::tr( "&Custom colors") , dialog, "qt_custcolors_lbl" );
	leftLay->addWidget( lab );
	leftLay->addWidget( custom );

	QPushButton *custbut =
	    new QPushButton( QColorDialog3::tr("&Define Custom Colors >>"),
						dialog, "qt_def_custcolors_lbl" );
	custbut->setEnabled( FALSE );
	leftLay->addWidget( custbut );
    } else {
	// better color picker size for small displays
	pWidth = 150;
	pHeight = 100;

        custom = 0;
        standard = 0;
    }

    Q3VBoxLayout *rightLay = new Q3VBoxLayout( topLay );

    Q3HBoxLayout *pickLay = new Q3HBoxLayout( rightLay );


    Q3VBoxLayout *cLay = new Q3VBoxLayout( pickLay );
    cp = new QColorPicker( dialog, "qt_colorpicker" );
    cp->setFrameStyle( Q3Frame::Panel + Q3Frame::Sunken );
    cLay->addSpacing( lumSpace );
    cLay->addWidget( cp );
    cLay->addSpacing( lumSpace );

    lp = new QColorLuminancePicker( dialog, "qt_luminance_picker" );
    lp->setFixedWidth( 20 ); //###
    pickLay->addWidget( lp );

    connect( cp, SIGNAL(newCol(int,int)), lp, SLOT(setCol(int,int)) );
    connect( lp, SIGNAL(newHsv(int,int,int)), this, SLOT(newHsv(int,int,int)) );

    rightLay->addStretch();

    cs = new QColorShower( dialog, "qt_colorshower" );
    connect( cs, SIGNAL(newCol(QRgb)), this, SLOT(newColorTypedIn(QRgb)));
    rightLay->addWidget( cs );

    Q3HBoxLayout *buttons;
    if ( compact )
	buttons = new Q3HBoxLayout( rightLay );
    else
	buttons = new Q3HBoxLayout( leftLay );

    QPushButton *ok, *cancel;
    ok = new QPushButton( QColorDialog3::tr("OK"), dialog, "qt_ok_btn" );
    connect( ok, SIGNAL(clicked()), dialog, SLOT(accept()) );
    ok->setDefault(TRUE);
    cancel = new QPushButton( QColorDialog3::tr("Cancel"), dialog, "qt_cancel_btn" );
    connect( cancel, SIGNAL(clicked()), dialog, SLOT(reject()) );
    buttons->addWidget( ok );
    buttons->addWidget( cancel );
    buttons->addStretch();

    if ( !compact ) {
	QPushButton *addCusBt = new QPushButton(
					QColorDialog3::tr("&Add to Custom Colors"),
						 dialog, "qt_add_btn" );
	rightLay->addWidget( addCusBt );
	connect( addCusBt, SIGNAL(clicked()), this, SLOT(addCustom()) );
    }

    custom_well_array = custom;
}

void QColorDialogPrivate::addCustom()
{
    cusrgb[nextCust] = cs->currentColor();
    if (custom)
        custom->repaintContents( FALSE );
    nextCust = (nextCust+1) % 16;
}


/*!
    \class QColorDialog3 qcolordialog.h
    \brief The QColorDialog3 class provides a dialog widget for specifying colors.
    \mainclass
    \ingroup dialogs
    \ingroup graphics

    The color dialog's function is to allow users to choose colors.
    For example, you might use this in a drawing program to allow the
    user to set the brush color.

    The static functions provide modal color dialogs.
    \omit
    If you require a modeless dialog, use the QColorDialog3 constructor.
    \endomit

    The static getColor() function shows the dialog and allows the
    user to specify a color. The getRgba() function does the same but
    also allows the user to specify a color with an alpha channel
    (transparency) value.

    The user can store customCount() different custom colors. The
    custom colors are shared by all color dialogs, and remembered
    during the execution of the program. Use setCustomColor() to set
    the custom colors, and use customColor() to get them.

    \img qcolordlg-w.png
*/

/*!
    Constructs a default color dialog with parent \a parent and called
    \a name. If \a modal is TRUE the dialog will be modal. Use
    setColor() to set an initial value.

    \sa getColor()
*/

QColorDialog3::QColorDialog3(QWidget* parent, const char* name, bool modal) :
    QDialog(parent, name, modal, (  Qt::WType_Dialog | Qt::WStyle_Customize | Qt::WStyle_Title |
                                    Qt::WStyle_DialogBorder | Qt::WStyle_SysMenu ) )
{
    setSizeGripEnabled( FALSE );
    d = new QColorDialogPrivate( this );

#ifndef QT_NO_SETTINGS
    if ( !customSet ) {
	QSettings settings;
	settings.insertSearchPath( QSettings::Windows, "/Trolltech" );
	for ( int i = 0; i < 2*8; ++i ) {
	    bool ok = FALSE;
	    QRgb rgb = (QRgb)settings.readNumEntry( "/Qt/customColors/" + QString::number( i ), 0, &ok );
	    if ( ok )
		cusrgb[i] = rgb;
	}
    }
#endif
}

/*!
    Pops up a modal color dialog, lets the user choose a color, and
    returns that color. The color is initially set to \a initial. The
    dialog is a child of \a parent and is called \a name. It returns
    an invalid (see QColor::isValid()) color if the user cancels the
    dialog. All colors allocated by the dialog will be deallocated
    before this function returns.
*/

QColor QColorDialog3::getColor( const QColor& initial, QWidget *parent,
			       const char *name )
{
  printf("HEPP!\n");
#if defined(Q_WS_MAC)
    return macGetColor(initial, parent, name);
#endif

    //int allocContext = QColor::enterAllocContext();
    //int allocContext = 0;//QColor::enterAllocContext();
    QColorDialog3 *dlg = new QColorDialog3( parent, name, TRUE );  //modal
#ifndef QT_NO_WIDGET_TOPEXTRA
    dlg->setCaption( QColorDialog3::tr( "Select color" ) );
#endif
    dlg->setColor( initial );
    dlg->selectColor( initial );
    int resultCode;
    //GL_lock();{
      resultCode = dlg->exec();
      //}GL_unlock();
    //QColor::leaveAllocContext();
    QColor result;
    if ( resultCode == QDialog::Accepted )
      result = dlg->color();
    //QColor::destroyAllocContext(allocContext);
    delete dlg;
    return result;
}


/*!
    Pops up a modal color dialog to allow the user to choose a color
    and an alpha channel (transparency) value. The color+alpha is
    initially set to \a initial. The dialog is a child of \a parent
    and called \a name.

    If \a ok is non-null, \e *\a ok is set to TRUE if the user clicked
    OK, and to FALSE if the user clicked Cancel.

    If the user clicks Cancel, the \a initial value is returned.
*/

QRgb QColorDialog3::getRgba( QRgb initial, bool *ok,
			    QWidget *parent, const char* name )
{
#if defined(Q_WS_MAC)
    return macGetRgba(initial, ok, parent, name);
#endif

    //int allocContext = QColor::enterAllocContext();
    QColorDialog3 *dlg = new QColorDialog3( parent, name, TRUE );  //modal

    Q_CHECK_PTR( dlg );
#ifndef QT_NO_WIDGET_TOPEXTRA
    dlg->setCaption( QColorDialog3::tr( "Select color" ) );
#endif
    dlg->setColor( initial );
    dlg->selectColor( initial );
    dlg->setSelectedAlpha( qAlpha(initial) );
    int resultCode;
    GL_lock();{
      resultCode = dlg->exec();
    }GL_unlock();
    //QColor::leaveAllocContext();
    QRgb result = initial;
    if ( resultCode == QDialog::Accepted ) {
	QRgb c = dlg->color().rgb();
	int alpha = dlg->selectedAlpha();
	result = qRgba( qRed(c), qGreen(c), qBlue(c), alpha );
    }
    if ( ok )
	*ok = resultCode == QDialog::Accepted;

    //QColor::destroyAllocContext(allocContext);
    delete dlg;
    return result;
}





/*!
    Returns the color currently selected in the dialog.

    \sa setColor()
*/

QColor QColorDialog3::color() const
{
    return QColor(d->currentColor());
}


/*!
    Destroys the dialog and frees any memory it allocated.
*/

QColorDialog3::~QColorDialog3()
{
#ifndef QT_NO_SETTINGS
    if ( !customSet ) {
	QSettings settings;
	settings.insertSearchPath( QSettings::Windows, "/Trolltech" );
	for ( int i = 0; i < 2*8; ++i )
	    settings.writeEntry( "/Qt/customColors/" + QString::number( i ), (int)cusrgb[i] );
    }
#endif
}


/*!
    Sets the color shown in the dialog to \a c.

    \sa color()
*/

void QColorDialog3::setColor( const QColor& c )
{
    d->setCurrentColor( c.rgb() );
}




/*!
    Sets the initial alpha channel value to \a a, and shows the alpha
    channel entry box.
*/

void QColorDialog3::setSelectedAlpha( int a )
{
    d->showAlpha( TRUE );
    d->setCurrentAlpha( a );
}


/*!
    Returns the value selected for the alpha channel.
*/

int QColorDialog3::selectedAlpha() const
{
    return d->currentAlpha();
}

/*!
    Sets focus to the corresponding button, if any.
*/
bool QColorDialog3::selectColor( const QColor& col )
{
    QRgb color = col.rgb();
    int i = 0, j = 0;
    // Check standard colors
    if (d->standard) {
        for ( i = 0; i < 6; i++ ) {
            for ( j = 0; j < 8; j++ ) {
                if ( color == stdrgb[i + j*6] ) {
                    d->newStandard( i, j );
                    d->standard->setCurrent( i, j );
                    d->standard->setSelected( i, j );
                    // GL_lock is needed when using intel gfx driver to avoid crash caused by opening two opengl contexts simultaneously from two threads.
                    GL_lock();{
                      d->standard->setFocus();
                    }GL_unlock();
                    return TRUE;
                }
            }
        }
    }
    // Check custom colors
    if (d->custom) {
        for ( i = 0; i < 2; i++ ) {
            for ( j = 0; j < 8; j++ ) {
                if ( color == cusrgb[i + j*2] ) {
                    d->newCustom( i, j );
                    d->custom->setCurrent( i, j );
                    d->custom->setSelected( i, j );
                    // GL_lock is needed when using intel gfx driver to avoid crash caused by opening two opengl contexts simultaneously from two threads.
                    GL_lock();{
                      d->custom->setFocus();
                    }GL_unlock();
                    return TRUE;
                }
            }
        }
    }
    return FALSE;
}

#include "qcolordialog.moc"

#endif

//#endif

