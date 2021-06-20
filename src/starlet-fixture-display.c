/*
 * starlet-fixture-display.c
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
#define FIXTURE_BORDER (5.0)


struct fixture
{
	char *label;
};


struct fixture_display
{
	double fixture_tile_width;
	struct fixture *fixtures;
	int n_fixtures;
	GtkWidget *da;
	ReplConnection *repl;
};


static void draw_fixture(cairo_t *cr,
                         PangoContext *pc,
                         PangoFontDescription *fontdesc,
                         struct fixture_display *fixd,
                         struct fixture *fix)
{
	PangoLayout *layout;
	const double w = 40.0;
	const double h = 3.0/2.0*w;
	//char tmp[32];

	/* Pan/tilt (underneath rectangle) */
//	if ( fix->cls->attributes & PANTILT ) {
//
//		double x = w*(1.0 + fix->v.pan)/2.0;
//		double y = h*(1.0 - fix->v.tilt)/2.0;
//
//		cairo_move_to(cr, x, -1.0);
//		cairo_line_to(cr, x, h+1.0);
//		cairo_set_source_rgb(cr, 1.0, 0.0, 0.0);
//		cairo_set_line_width(cr, 1.0);
//		cairo_stroke(cr);
//
//		cairo_move_to(cr, -1.0, y);
//		cairo_line_to(cr, w+1.0, y);
//		cairo_set_source_rgb(cr, 1.0, 0.0, 0.0);
//		cairo_set_line_width(cr, 1.0);
//		cairo_stroke(cr);
//
//	}

	cairo_rectangle(cr, 0.0, 0.0, w, h);
	//if ( fixture_selected(fixd, fix) ) {
	//	cairo_set_source_rgba(cr, 0.3, 0.3, 0.9, 0.9);
	//} else {
		cairo_set_source_rgba(cr, 0.3, 0.3, 0.3, 0.9);
	//}
	cairo_fill_preserve(cr);
	cairo_set_source_rgb(cr, 1.0, 1.0, 1.0);
	cairo_set_line_width(cr, 1.0);
	cairo_stroke(cr);

	/* Label */
	layout = pango_layout_new(pc);
	pango_layout_set_text(layout, fix->label, -1);
	pango_layout_set_width(layout, (w*PANGO_SCALE)-4.0);
	pango_layout_set_alignment(layout, PANGO_ALIGN_CENTER);
	pango_layout_set_font_description(layout, fontdesc);
	cairo_set_source_rgb(cr, 1.0, 1.0, 1.0);
	cairo_move_to(cr, 0.0, 2.0);
	pango_cairo_show_layout(cr, layout);
	g_object_unref(layout);

	/* Intensity */
	//snprintf(tmp, 32, "%.0f %%", fix->v.intensity*100.0);
	//layout = pango_layout_new(pc);
	//pango_layout_set_text(layout, tmp, -1);
	//pango_layout_set_width(layout, (w*PANGO_SCALE)-4.0);
	//pango_layout_set_alignment(layout, PANGO_ALIGN_CENTER);
	//pango_layout_set_font_description(layout, fontdesc);
	//cairo_set_source_rgb(cr, 1.0, 1.0, 1.0);
	//cairo_move_to(cr, 0.0, 15.0);
	//pango_cairo_show_layout(cr, layout);
	//g_object_unref(layout);
}


static gboolean draw_sig(GtkWidget *widget, cairo_t *cr, struct fixture_display *fixd)
{
	int w, h;
	int i;
	PangoContext *pc;
	PangoFontDescription *fontdesc;
	double x, y;

	w = gtk_widget_get_allocated_width(widget);
	h = gtk_widget_get_allocated_height(widget);
	pc = gtk_widget_get_pango_context(widget);

	/* Overall background */
	cairo_set_source_rgb(cr, 0.0, 0.0, 0.2);
	cairo_paint(cr);

	cairo_save(cr);
	cairo_translate(cr, OVERALL_BORDER, OVERALL_BORDER);
	w -= OVERALL_BORDER*2.0;
	h -= OVERALL_BORDER*2.0;

	/* Fixtures */
	x = FIXTURE_BORDER;
	y = FIXTURE_BORDER;
	fontdesc = pango_font_description_from_string("Comfortaa Bold 8");
	for ( i=0; i<fixd->n_fixtures; i++ ) {
		cairo_save(cr);
		cairo_translate(cr, x, y);
		cairo_scale(cr, fixd->fixture_tile_width/40.0, fixd->fixture_tile_width/40.0);
		draw_fixture(cr, pc, fontdesc, fixd, &fixd->fixtures[i]);
		cairo_restore(cr);
		x += fixd->fixture_tile_width + FIXTURE_BORDER*2;
		if ( x + fixd->fixture_tile_width + FIXTURE_BORDER*2 > w ) {
			x = FIXTURE_BORDER;
			y += fixd->fixture_tile_width*3.0/2.0 + FIXTURE_BORDER*2;
		}
	}

	cairo_restore(cr);

	return FALSE;
}


static void redraw(struct fixture_display *fixd)
{
	gint w, h;
	w = gtk_widget_get_allocated_width(GTK_WIDGET(fixd->da));
	h = gtk_widget_get_allocated_height(GTK_WIDGET(fixd->da));
	gtk_widget_queue_draw_area(GTK_WIDGET(fixd->da), 0, 0, w, h);
}


static gboolean key_press_sig(GtkWidget *da, GdkEventKey *event, struct fixture_display *fixd)
{
	int claim = 1;

	switch ( event->keyval ) {

		case GDK_KEY_Escape :
		repl_send(fixd->repl, "(sel #f)");
		break;

		case GDK_KEY_KP_Enter :
		repl_send(fixd->repl, "(go! pb)");
		break;

		default :
		claim = 0;
		break;

	}

	if ( claim ) return TRUE;
	return FALSE;
}


static gint realise_sig(GtkWidget *da, struct fixture_display *fixd)
{
	GdkWindow *win = gtk_widget_get_window(da);

	/* Keyboard and input method stuff */
	gdk_window_set_accept_focus(win, TRUE);
	g_signal_connect(G_OBJECT(da), "key-press-event", G_CALLBACK(key_press_sig), fixd);

	return FALSE;
}


static gboolean redraw_cb(gpointer data)
{
	redraw((struct fixture_display *)data);
	return G_SOURCE_CONTINUE;
}


static void show_help(const char *s)
{
	printf(_("Syntax: %s [options]\n\n"), s);
	printf(_("Show fixtures in Starlet"
	         "  -s, --socket  REPL socket for Starlet process (default guile.socket).\n"
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


static void handle_patched_fixtures(struct fixture_display *fixd,
                                    SCM list)
{
	int i;
	int nfix;

	if ( !is_list(list) ) {
		fprintf(stderr, "Invalid patched fixture list\n");
		return;
	}

	nfix = scm_to_int(scm_length(list));

	free(fixd->fixtures);
	fixd->fixtures = malloc(nfix*sizeof(struct fixture));
	fixd->n_fixtures = nfix;

	for ( i=0; i<nfix; i++ ) {
		SCM item = scm_list_ref(list, scm_from_int(i));
		if ( is_list(item) && (scm_to_int(scm_length(item)) == 3) ) {

			char tmp[64];
			SCM group_name = scm_list_ref(item, scm_from_int(1));
			SCM idx = scm_list_ref(item, scm_from_int(2));
			SCM name = scm_symbol_to_string(group_name);
			snprintf(tmp, 63, "%s/%i",
			         scm_to_locale_string(name),
			         scm_to_int(idx));
			fixd->fixtures[i].label = strdup(tmp);

		} else {
			SCM name = scm_symbol_to_string(item);
			fixd->fixtures[i].label = scm_to_locale_string(name);
		}
	}
}


static void process_line(SCM sexp, void *data)
{
	struct fixture_display *fixd = data;

	if ( is_list(sexp) && scm_to_int(scm_length(sexp)) == 2 ) {
		SCM tag = scm_list_ref(sexp, scm_from_int(0));
		SCM contents = scm_list_ref(sexp, scm_from_int(1));
		if ( scm_is_symbol(tag) ) {
			if ( symbol_eq(tag, "patched-fixtures") ) {
				handle_patched_fixtures(fixd, contents);
			}
		}
	}
}


int main(int argc, char *argv[])
{
	struct fixture_display fixd;
	int c;
	GtkWidget *mainwindow;
	GtkWidget *da;
	char *socket = NULL;

	gtk_init(&argc, &argv);

	const struct option longopts[] = {
		{"help",               0, NULL,               'h'},
		{"socket",             1, NULL,               's'},
		{0, 0, NULL, 0}
	};

	while ((c = getopt_long(argc, argv, "h", longopts, NULL)) != -1) {

		switch (c)
		{
			case 'h' :
			show_help(argv[0]);
			return 0;

			case 's' :
			socket = strdup(optarg);
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
	gtk_window_set_default_size(GTK_WINDOW(mainwindow), 1024, 768);
	g_signal_connect_swapped(G_OBJECT(mainwindow), "destroy",
	                         gtk_main_quit, NULL);
	gtk_window_set_title(GTK_WINDOW(mainwindow), "Starlet fixture display");

	da = gtk_drawing_area_new();

	fixd.fixture_tile_width = 60.0;
	fixd.fixtures = NULL;
	fixd.n_fixtures = 0;
	fixd.da = da;

	gtk_container_add(GTK_CONTAINER(mainwindow), GTK_WIDGET(da));
	gtk_widget_set_can_focus(GTK_WIDGET(da), TRUE);
	gtk_widget_add_events(da, GDK_KEY_PRESS_MASK | GDK_KEY_RELEASE_MASK
	                        | GDK_BUTTON_PRESS_MASK | GDK_BUTTON_RELEASE_MASK
	                        | GDK_BUTTON_MOTION_MASK);
	g_signal_connect(G_OBJECT(da), "draw", G_CALLBACK(draw_sig), &fixd);
	g_signal_connect(G_OBJECT(da), "realize", G_CALLBACK(realise_sig), &fixd);

	gtk_widget_grab_focus(GTK_WIDGET(da));
	gtk_widget_show_all(mainwindow);

	g_timeout_add(50, redraw_cb, &fixd);

	fixd.repl = repl_connection_new(socket, process_line, &fixd);
	repl_send(fixd.repl, "(list 'patched-fixtures (patched-fixture-names))");

	gtk_main();

	return 0;
}
