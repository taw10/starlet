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

#ifndef LISPMACHINE_H
#define LISPMACHINE_H

#define TYPE_LISP_MACHINE (lisp_machine_get_type())

#define LISP_MACHINE(obj) (G_TYPE_CHECK_INSTANCE_CAST((obj), \
                              TYPE_LISP_MACHINE, LispMachine))

#define IS_LISP_MACHINE(obj) (G_TYPE_CHECK_INSTANCE_TYPE((obj), \
                                 TYPE_LISP_MACHINE))

#define LISP_MACHINE_CLASS(klass) (G_TYPE_CHECK_CLASS_CAST((obj), \
                                    TYPE_LISP_MACHINE, LispMachine))

#define IS_LISP_MACHINE_CLASS(klass) (G_TYPE_CHECK_CLASS_TYPE((obj), \
                                       TYPE_LISP_MACHINE))

#define LISP_MACHINE_GET_CLASS(obj) (G_TYPE_INSTANCE_GET_CLASS((obj), \
                                        TYPE_LISP_MACHINE, LispMachine))


struct _lispmachine
{
	GtkDrawingArea       parent_instance;
	double               visible_width;
	double               visible_height;
};

struct _lispmachineclass
{
	GtkDrawingAreaClass parent_class;
};

typedef struct _lispmachine LispMachine;
typedef struct _lispmachineclass LispMachineClass;

extern GType lisp_machine_get_type(void);
extern GtkWidget *lisp_machine_new(void);

#endif	/* LISPMACHINE_H */
