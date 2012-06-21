/****************************************************************************
** ui.h extension file, included from the uic-generated form implementation.
**
** If you want to add, delete, or rename functions or slots, use
** Qt Designer to update this file, preserving your code.
**
** You should not define a constructor or destructor in this file.
** Instead, write your code in functions called init() and destroy().
** These will automatically be called by the form's constructor and
** destructor.
*****************************************************************************/


void Instruments_widget::gotFocus( QFocusEvent * )
{
    fprintf(stderr,"Got focus!");
}


void Instruments_widget::lostFocus( QFocusEvent * )
{
    fprintf(stderr,"Lost focus!");
}


void Instruments_widget::focusInEvent( QFocusEvent * )
{
    fprintf(stderr,"Got focus!");

}


