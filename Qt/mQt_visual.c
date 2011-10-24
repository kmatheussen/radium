/****************************************************************************
** MyQListBox meta object code from reading C++ file 'standard input'
**
** Created: Fri Jan 4 21:48:44 2002
**      by: The Qt MOC ($Id: qt/src/moc/moc.y   2.3.0   edited 2001-02-20 $)
**
** WARNING! All changes made in this file will be lost!
*****************************************************************************/

#if !defined(Q_MOC_OUTPUT_REVISION)
#define Q_MOC_OUTPUT_REVISION 9
#elif Q_MOC_OUTPUT_REVISION != 9
#error "Moc format conflict - please regenerate all moc files"
#endif

#include <qmetaobject.h>
#include <qapplication.h>



const char *MyQListBox::className() const
{
    return "MyQListBox";
}

QMetaObject *MyQListBox::metaObj = 0;

void MyQListBox::initMetaObject()
{
    if ( metaObj )
	return;
    if ( qstrcmp(QListBox::className(), "QListBox") != 0 )
	badSuperclassWarning("MyQListBox","QListBox");
    (void) staticMetaObject();
}

#ifndef QT_NO_TRANSLATION

QString MyQListBox::tr(const char* s)
{
    return qApp->translate( "MyQListBox", s, 0 );
}

QString MyQListBox::tr(const char* s, const char * c)
{
    return qApp->translate( "MyQListBox", s, c );
}

#endif // QT_NO_TRANSLATION

QMetaObject* MyQListBox::staticMetaObject()
{
    if ( metaObj )
	return metaObj;
    (void) QListBox::staticMetaObject();
#ifndef QT_NO_PROPERTIES
#endif // QT_NO_PROPERTIES
    QMetaData::Access *slot_tbl_access = 0;
    typedef void (MyQListBox::*m2_t0)(int);
    typedef void (QObject::*om2_t0)(int);
    m2_t0 v2_0 = &MyQListBox::aselected;
    om2_t0 ov2_0 = (om2_t0)v2_0;
    QMetaData *signal_tbl = QMetaObject::new_metadata(1);
    signal_tbl[0].name = "aselected(int)";
    signal_tbl[0].ptr = (QMember)ov2_0;
    metaObj = QMetaObject::new_metaobject(
	"MyQListBox", "QListBox",
	0, 0,
	signal_tbl, 1,
#ifndef QT_NO_PROPERTIES
	0, 0,
	0, 0,
#endif // QT_NO_PROPERTIES
	0, 0 );
    metaObj->set_slot_access( slot_tbl_access );
#ifndef QT_NO_PROPERTIES
#endif // QT_NO_PROPERTIES
    return metaObj;
}

// SIGNAL aselected
//void MyQListBox::aselected( int t0 )
//{
//    printf("jadda %d\n",t0);
//    activate_signal( "aselected(int)", t0 );
//}
