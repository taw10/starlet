/*
 * starlet-editor.c
 *
 * Copyright © 2024 Thomas White <taw@bitwiz.me.uk>
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
#include <lo/lo.h>

#include <libintl.h>
#define _(x) gettext(x)

#include "lispmachine.h"


static void show_help(const char *s)
{
	printf(_("Syntax: %s [options]\n\n"), s);
	printf(_("Starlet scene editor\n"
	         "  -h, --help    Display this help message.\n"));
}


static void shutdown_sig(GtkWidget *window, void *vp)
{
	gtk_main_quit();
}


int main(int argc, char *argv[])
{
	int c;
	GtkWidget *mainwindow;

	gtk_init(&argc, &argv);

	const struct option longopts[] = {
		{"help",               0, NULL,               'h'},
		{0, 0, NULL, 0}
	};

	while ((c = getopt_long(argc, argv, "h", longopts, NULL)) != -1) {

		switch (c)
		{
			case 'h' :
			show_help(argv[0]);
			return 0;

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

	/* Create main window */
	mainwindow = gtk_window_new(GTK_WINDOW_TOPLEVEL);
	gtk_window_set_default_size(GTK_WINDOW(mainwindow), 1024, 768);
	gtk_window_set_title(GTK_WINDOW(mainwindow), "Starlet editor");
	g_signal_connect(G_OBJECT(mainwindow), "destroy",
	                 G_CALLBACK(shutdown_sig), NULL);

	GtkWidget *pane = gtk_paned_new(GTK_ORIENTATION_HORIZONTAL);
	gtk_container_add(GTK_CONTAINER(mainwindow), pane);

	GtkWidget *lispmachine = lisp_machine_new();
	gtk_paned_add1(GTK_PANED(pane), lispmachine);
	gtk_paned_add2(GTK_PANED(pane), gtk_drawing_area_new());

	gtk_widget_show_all(mainwindow);

	gtk_main();

	return 0;
}
