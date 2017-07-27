#include "../common/nsmtracker.h"
#include "../common/hashmap_proc.h"
#include "../common/OS_string_proc.h"

#include "mQt_comment_dialog_callbacks.h"


extern struct Root *root;

static bool has_been_made = false;
static QPointer<comment_dialog> widget=NULL;

static void ensure_widget_is_created(void){
  if(widget==NULL){
    R_ASSERT(has_been_made==false);
    widget = new comment_dialog(g_main_window);
    g_static_toplevel_widgets.push_back(widget.data());
    has_been_made = true;
  }
}


extern "C"{
  void COMMENTDIALOG_open(void){
    ensure_widget_is_created();

    safeShowOrExec(widget, true);
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

    QStringList lines = widget->comment->toPlainText().split("\n");
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

    int num_elements = HASH_get_array_size(state, "comment");
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

