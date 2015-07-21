/* Copyright 2013 Kjetil S. Matheussen

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


#include <stdio.h>
#include <sndfile.h>
#include <unistd.h>

#include <QMessageBox>
#include <QFileDialog>
#include <FocusSniffers.h>

#include "../common/nsmtracker.h"
#include "../common/hashmap_proc.h"
#include "../common/OS_string_proc.h"

#include "helpers.h"

#include "Qt_comment_dialog.h"


extern struct Root *root;

class comment_dialog : public QDialog, public Ui::Comment_dialog {
  Q_OBJECT

 public:
  bool _initing;

 comment_dialog(QWidget *parent=NULL)
    : QDialog(parent)
  {
    _initing = true;

    setupUi(this);

    _initing = false;
  }

public slots:

  void on_buttonBox_clicked(QAbstractButton * button){
    this->hide();
  }

};


extern int num_users_of_keyboard;

static comment_dialog *widget=NULL;

static void ensure_widget_is_created(void){
  if(widget==NULL){
    widget = new comment_dialog(NULL);
    widget->setWindowFlags(Qt::WindowStaysOnTopHint);
    //widget->setWindowModality(Qt::ApplicationModal);
  }
}

extern "C"{
  void COMMENTDIALOG_open(void){
    ensure_widget_is_created();

    safeShowOrExec(widget);
  }

  bool COMMENT_show_after_loading(void){
    ensure_widget_is_created();

    return widget->show_after_loading->isChecked();
  }

  hash_t *COMMENT_get_state(void){
    ensure_widget_is_created();

    hash_t *state = HASH_create(3);
    HASH_put_string(state, "author", STRING_create(widget->author->text()));
    HASH_put_string(state, "title", STRING_create(widget->title->text()));

    QStringList lines = widget->comment->text().split("\n");
    for (int i = 0; i < lines.size(); i++)
      HASH_put_string_at(state, "comment", i, STRING_create(lines.at(i)));

    HASH_put_int(state, "show_after_loading", COMMENT_show_after_loading()?1:0);

    return state;
  }

  static void set(QString author, QString title, QString comment, bool show_after_loading){
    ensure_widget_is_created();

    widget->author->setText(author);
    widget->title->setText(title);
    widget->comment->setText(comment);
    widget->show_after_loading->setChecked(show_after_loading);
  }

  void COMMENT_reset(void){
    ensure_widget_is_created();

    set("","","",false);
  }

  static QString get_comment_from_state(hash_t *state){
    ensure_widget_is_created();

    int num_elements = HASH_get_array_size(state);
    QString ret = "";
    for(int i=0;i<num_elements;i++)
      ret += STRING_get_qstring(HASH_get_string_at(state, "comment", i)) + QString("\n");
    return ret;
  }

  void COMMENT_set_state(hash_t *state){
    ensure_widget_is_created();

    set(STRING_get_qstring(HASH_get_string(state, "author")),
        STRING_get_qstring(HASH_get_string(state, "title")),
        get_comment_from_state(state),
        HASH_get_int(state, "show_after_loading")==1?true:false
        );
  }
}

#include "mQt_comment_dialog_callbacks.cpp"

