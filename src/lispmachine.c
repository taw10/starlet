/*
 * lispmachine.h
 *
 * "Lisp machine" widget
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

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>
#include <gtk/gtk.h>
#include <glib-object.h>


#include "lispmachine.h"


G_DEFINE_TYPE(LispMachine, lisp_machine, GTK_TYPE_DRAWING_AREA)

static gint destroy_sig(GtkWidget *window, LispMachine *lm)
{
	return FALSE;
}


static gint configure_sig(GtkWidget *window, GdkEventConfigure *rec,
                          LispMachine *lm)
{
	lm->visible_width = rec->width;
	lm->visible_height = rec->height;
	return FALSE;
}


static gint draw_sig(GtkWidget *window, cairo_t *cr, LispMachine *lm)
{
	cairo_save(cr);

	/* Overall background */
	cairo_set_source_rgb(cr, 1.0, 1.0, 1.0);
	cairo_paint(cr);

	return FALSE;
}


static GtkSizeRequestMode get_request_mode(GtkWidget *widget)
{
	return GTK_SIZE_REQUEST_CONSTANT_SIZE;
}


static void get_preferred_width(GtkWidget *widget, gint *min, gint *natural)
{
	*min = 0;
	*natural = 480;
}


static void get_preferred_height(GtkWidget *widget, gint *min, gint *natural)
{
	*min = 0;
	*natural = 320;
}


static void lisp_machine_class_init(LispMachineClass *klass)
{
	GTK_WIDGET_CLASS(klass)->get_request_mode = get_request_mode;
	GTK_WIDGET_CLASS(klass)->get_preferred_width = get_preferred_width;
	GTK_WIDGET_CLASS(klass)->get_preferred_height = get_preferred_height;
	GTK_WIDGET_CLASS(klass)->get_preferred_height_for_width = NULL;
}


static void lisp_machine_init(LispMachine *lm)
{
}


GtkWidget *lisp_machine_new()
{
	LispMachine *lm;

	lm = g_object_new(TYPE_LISP_MACHINE, NULL);

	g_signal_connect(G_OBJECT(lm), "destroy",
	                 G_CALLBACK(destroy_sig), lm);
	g_signal_connect(G_OBJECT(lm), "configure-event",
	                 G_CALLBACK(configure_sig), lm);
	g_signal_connect(G_OBJECT(lm), "draw",
	                 G_CALLBACK(draw_sig), lm);

	gtk_widget_set_can_focus(GTK_WIDGET(lm), FALSE);

	gtk_widget_show(GTK_WIDGET(lm));

	return GTK_WIDGET(lm);
}
