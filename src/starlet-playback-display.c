/*
 * starlet-playback-display.c
 *
 * Copyright © 2019-2021 Thomas White <taw@bitwiz.me.uk>
 *
 * This file is part of Starlet.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
 */


#include <gtk/gtk.h>
#include <getopt.h>
#include <string.h>
#include <stdlib.h>
#include <sys/stat.h>
#include <glib.h>
#include <glib/gstdio.h>
#include <libguile.h>

#include <libintl.h>
#define _(x) gettext(x)

#include "repl-connection.h"


#define OVERALL_BORDER (20.0)


struct playback_display
{
	GtkWidget *da;
	ReplConnection *repl;
	int shutdown;
	const char *playback_name;
	double current_cue_number;
	double scanout_rate;
	char *socket;
	int verbose;
};


static void plot_text(cairo_t *cr, const char *text,
                      PangoContext *pc, PangoFontDescription *fontdesc,
                      double x, double y)
{
	PangoLayout *layout;

	layout = pango_layout_new(pc);
	pango_layout_set_text(layout, text, -1);
	pango_layout_set_font_description(layout, fontdesc);
	cairo_set_source_rgb(cr, 1.0, 1.0, 1.0);
	cairo_move_to(cr, x, y);
	pango_cairo_show_layout(cr, layout);
	g_object_unref(layout);
}


static gboolean draw_sig(GtkWidget *widget, cairo_t *cr,
                         struct playback_display *pbd)
{
	PangoContext *pc;
	PangoFontDescription *fontdesc;
	char tmp[32];

	pc = gtk_widget_get_pango_context(widget);
	fontdesc = pango_font_description_from_string("Comfortaa Bold 12");

	/* Overall background */
	if ( pbd->repl == NULL ) {
		cairo_set_source_rgb(cr, 0.2, 0.0, 0.0);
		cairo_paint(cr);
		return FALSE;
	} else {
		cairo_set_source_rgb(cr, 0.0, 0.0, 0.2);
		cairo_paint(cr);
	}

	cairo_save(cr);
	cairo_translate(cr, OVERALL_BORDER, OVERALL_BORDER);

	snprintf(tmp, 32, "Current cue number: %.2f", pbd->current_cue_number);
	plot_text(cr, tmp, pc, fontdesc, 0.0, 0.0);
	snprintf(tmp, 32, "Scanout rate: %.2f per second", pbd->scanout_rate);
	plot_text(cr, tmp, pc, fontdesc, 0.0, 32.0);

	cairo_restore(cr);

	return FALSE;
}


static void redraw(struct playback_display *pbd)
{
	gint w, h;
	w = gtk_widget_get_allocated_width(GTK_WIDGET(pbd->da));
	h = gtk_widget_get_allocated_height(GTK_WIDGET(pbd->da));
	gtk_widget_queue_draw_area(GTK_WIDGET(pbd->da), 0, 0, w, h);
}


static void shutdown_sig(GtkWidget *window, struct playback_display *pbd)
{
	if ( pbd->repl != NULL ) {
		repl_connection_close(pbd->repl);
	}
	pbd->shutdown = TRUE;
}


static void request_playback_status(struct playback_display *pbd)
{
	char tmp[256];
	snprintf(tmp, 256, "(list 'playback-status (list "
	                   "(get-playback-cue-number %s)"
	                   "scanout-freq"
	                   "))",
	                   pbd->playback_name);
	repl_send(pbd->repl, tmp);
}


static gboolean key_press_sig(GtkWidget *da, GdkEventKey *event, struct playback_display *pbd)
{
	int claim = 1;

	switch ( event->keyval ) {

		case GDK_KEY_Escape :
		repl_send(pbd->repl, "(sel #f)");
		break;

		case GDK_KEY_KP_Enter :
		repl_send(pbd->repl, "(go! pb)");
		break;

		case GDK_KEY_KP_Add :
		repl_send(pbd->repl, "(back! pb)");
		break;

		default :
		claim = 0;
		break;

	}

	if ( claim ) return TRUE;
	return FALSE;
}


static gint realise_sig(GtkWidget *da, struct playback_display *pbd)
{
	GdkWindow *win = gtk_widget_get_window(da);

	/* Keyboard and input method stuff */
	gdk_window_set_accept_focus(win, TRUE);
	g_signal_connect(G_OBJECT(da), "key-press-event", G_CALLBACK(key_press_sig), pbd);

	return FALSE;
}


static gboolean redraw_cb(gpointer data)
{
	struct playback_display *pbd = data;
	if ( pbd->repl == NULL ) {
		return G_SOURCE_CONTINUE;
	}
	if ( repl_closed(pbd->repl) ) {
		if ( pbd->shutdown ) {
			gtk_main_quit();
			return G_SOURCE_REMOVE;
		} else {
			pbd->repl = NULL;
			redraw(pbd);
			return G_SOURCE_CONTINUE;
		}
	} else {
		if ( !pbd->shutdown ) {
			request_playback_status(pbd);
			redraw(pbd);
		}
		return G_SOURCE_CONTINUE;
	}
}


static void show_help(const char *s)
{
	printf(_("Syntax: %s [options]\n\n"), s);
	printf(_("Show playback status in Starlet\n"
	         "  -s, --socket  REPL socket for Starlet process (default guile.socket).\n"
	         "  -v, --verbose Show all REPL communications.\n"
	         "  -h, --help    Display this help message.\n"));
}


static int is_list(SCM list)
{
	return scm_is_true(scm_list_p(list));
}


static int symbol_eq(SCM symbol, const char *val)
{
	return scm_is_true(scm_eq_p(symbol, scm_from_utf8_symbol(val)));
}


static void handle_playback_status(struct playback_display *pbd, SCM list)
{
	pbd->current_cue_number = scm_to_double(scm_list_ref(list, scm_from_int(0)));
	pbd->scanout_rate = scm_to_double(scm_list_ref(list, scm_from_int(1)));
}


static void process_line(SCM sexp, void *data)
{
	struct playback_display *pbd = data;

	if ( is_list(sexp) && scm_to_int(scm_length(sexp)) == 2 ) {
		SCM tag = scm_list_ref(sexp, scm_from_int(0));
		SCM contents = scm_list_ref(sexp, scm_from_int(1));
		if ( scm_is_symbol(tag) ) {
			if ( symbol_eq(tag, "playback-status") ) {
				handle_playback_status(pbd, contents);
			}
		}
	}
}


static gboolean try_connect_cb(gpointer data)
{
	struct playback_display *pbd = data;
	if ( pbd->repl == NULL ) {
		pbd->repl = repl_connection_new(pbd->socket, process_line,
		                                 pbd, pbd->verbose);
	}
	return G_SOURCE_CONTINUE;
}


int main(int argc, char *argv[])
{
	struct playback_display pbd;
	int c;
	GtkWidget *mainwindow;
	GtkWidget *da;
	char *socket = NULL;
	char *playback_name = "pb";
	int verbose = 0;

	gtk_init(&argc, &argv);

	const struct option longopts[] = {
		{"help",               0, NULL,               'h'},
		{"socket",             1, NULL,               's'},
		{"playback",           1, NULL,               'p'},
		{"verbose",            0, NULL,               'v'},
		{0, 0, NULL, 0}
	};

	while ((c = getopt_long(argc, argv, "hvs:", longopts, NULL)) != -1) {

		switch (c)
		{
			case 'h' :
			show_help(argv[0]);
			return 0;

			case 's' :
			socket = strdup(optarg);
			break;

			case 'p' :
			playback_name = strdup(optarg);
			break;

			case 'v' :
			verbose = 1;
			break;

			case 0 :
			break;

			default :
			return 1;
		}

	}

	#if !GLIB_CHECK_VERSION(2,36,0)
	g_type_init();
	#endif

	bindtextdomain("starlet", LOCALEDIR);
	textdomain("starlet");

	if ( socket == NULL ) {
		socket = strdup("guile.socket");
	}

	/* Create main window */
	mainwindow = gtk_window_new(GTK_WINDOW_TOPLEVEL);
	gtk_window_set_default_size(GTK_WINDOW(mainwindow), 320, 512);
	g_signal_connect(G_OBJECT(mainwindow), "destroy",
	                 G_CALLBACK(shutdown_sig), &pbd);
	gtk_window_set_title(GTK_WINDOW(mainwindow), "Starlet playback display");

	da = gtk_drawing_area_new();

	pbd.da = da;
	pbd.shutdown = FALSE;
	pbd.playback_name = playback_name;
	pbd.socket = socket;
	pbd.verbose = verbose;
	pbd.repl = NULL;

	gtk_container_add(GTK_CONTAINER(mainwindow), GTK_WIDGET(da));
	gtk_widget_set_can_focus(GTK_WIDGET(da), TRUE);
	gtk_widget_add_events(da, GDK_KEY_PRESS_MASK | GDK_KEY_RELEASE_MASK
	                        | GDK_BUTTON_PRESS_MASK | GDK_BUTTON_RELEASE_MASK
	                        | GDK_BUTTON_MOTION_MASK);
	g_signal_connect(G_OBJECT(da), "draw", G_CALLBACK(draw_sig), &pbd);
	g_signal_connect(G_OBJECT(da), "realize", G_CALLBACK(realise_sig), &pbd);

	gtk_widget_grab_focus(GTK_WIDGET(da));
	gtk_widget_show_all(mainwindow);

	g_timeout_add(50, redraw_cb, &pbd);

	g_timeout_add(1000, try_connect_cb, &pbd);

	gtk_main();

	return 0;
}
