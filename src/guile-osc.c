/*
 * guile-osc.c
 *
 * Copyright © 2023 Thomas White <taw@bitwiz.org.uk>
 *
 * This file is part of Guile-OSC.
 *
 * Guile-OSC is free software: you can redistribute it and/or modify
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

#include <stdio.h>

#include <libguile.h>
#include <lo/lo.h>


static SCM osc_server_type;
static SCM osc_method_type;


static void error_callback(int num, const char *msg, const char *path)
{
	fprintf(stderr, "liblo error %i (%s) for path %s\n", num, msg, path);
}


static SCM start_osc()
{
	lo_server_thread srv = lo_server_thread_new("7770", error_callback);
	lo_server_thread_start(srv);
	return scm_make_foreign_object_1(osc_server_type, srv);
}


static void finalize_osc_server(SCM obj)
{
	lo_server_thread srv;
	scm_assert_foreign_object_type(osc_server_type, obj);
	srv = scm_foreign_object_ref(obj, 0);
	lo_server_thread_free(srv);
}


struct method_callback_data
{
	SCM proc;
};


static int method_callback(const char *path, const char *types, lo_arg **argv,
                           int argc, lo_message msg, void *vp)
{
	struct method_callback_data *data = vp;
	scm_init_guile();
	scm_call_0(data->proc);
	return 1;
}


static SCM define_osc_method(SCM server_obj, SCM path_obj, SCM proc)
{
	lo_server_thread srv;
	lo_method method;
	char *path;
	struct method_callback_data *data;

	scm_assert_foreign_object_type(osc_server_type, server_obj);
	srv = scm_foreign_object_ref(server_obj, 0);

	data = malloc(sizeof(struct method_callback_data));
	data->proc = proc;
	scm_gc_protect_object(proc);

	path = scm_to_utf8_stringn(path_obj, NULL);
	method = lo_server_thread_add_method(srv, path, "",
	                                     method_callback, data);
	free(path);

	return scm_make_foreign_object_1(osc_method_type, method);
}


void init_guile_osc()
{
	SCM name, slots;

	name = scm_from_utf8_symbol("OSCServer");
	slots = scm_list_1(scm_from_utf8_symbol("data"));
	osc_server_type = scm_make_foreign_object_type(name,
	                                               slots,
	                                               finalize_osc_server);

	name = scm_from_utf8_symbol("OSCMethod");
	slots = scm_list_1(scm_from_utf8_symbol("data"));
	osc_method_type = scm_make_foreign_object_type(name, slots, NULL);

	scm_c_define_gsubr("start-osc", 0, 0, 0, start_osc);
	scm_c_define_gsubr("define-osc-method", 3, 0, 0, define_osc_method);

	scm_add_feature("guile-osc");
}
