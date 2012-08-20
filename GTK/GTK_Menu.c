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

#include <gtk/gtk.h>

#include "../common/nsmtracker.h"
#include "../common/visual_proc.h"

static GMainLoop*  g_gloop;
static int g_selected;

static gboolean my_popup_handler(GtkWidget *widget, GdkEvent *event, gpointer callback_data){
  intptr_t num = (intptr_t)callback_data;
  printf("Got something: %p %d. loop: %p\n",callback_data,(int)num,g_gloop);
  g_selected = num;
  g_main_loop_quit(g_gloop);
  return FALSE;
}

static gboolean menu_hidden(GtkWidget *widget, GdkEvent *event, gpointer callback_data){
  printf("hidden signal\n");
  g_main_loop_quit(g_gloop);
  return FALSE;
}

int GFX_Menu(
             struct Tracker_Windows *tvisual,
             ReqType reqtype,
             char *seltext,
             int num_sel,
             char **menutext
             )
{
  if(reqtype==NULL || num_sel>20){

    g_selected = -1;

    GtkWidget *menu = gtk_menu_new ();

    gtk_menu_popup(GTK_MENU(menu),NULL,NULL,NULL,NULL,3, gtk_get_current_event_time());

    intptr_t i;
    for(i=0;i<num_sel;i++){
      GtkWidget *menuitem = gtk_menu_item_new_with_label(menutext[i]);
      gtk_menu_shell_append (GTK_MENU_SHELL (menu), menuitem);

      gtk_widget_show(menuitem);

      gpointer val = (gpointer)i;
      g_signal_connect (menuitem, "button_press_event",
                        G_CALLBACK (my_popup_handler), val);
    }

    g_signal_connect(menu,
                     "hide",
                     G_CALLBACK(menu_hidden),
                     NULL);

    gtk_widget_show(menu);


    {
      g_gloop = g_main_loop_new (NULL, FALSE);
      g_main_loop_run (g_gloop);
      g_main_loop_unref(g_gloop);
    }

    gtk_widget_destroy(menu);

    printf("Returning %d\n",g_selected);

    return g_selected;

  }else{

    return GFX_ReqTypeMenu(tvisual,reqtype,seltext,num_sel,menutext);

  }
}
