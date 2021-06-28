/*
 * repl-connection.c
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


#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include <ctype.h>
#include <glib.h>
#include <gio/gio.h>
#include <gio/gunixsocketaddress.h>
#include <libguile.h>

#include <libintl.h>
#define _(x) gettext(x)

#include "repl-connection.h"

struct _replconnection
{
	GSocketConnection *conn;
	char inbuf[1024];
	char input[2048];
	void (*process_func)(SCM sexp, void *data);
	void *process_func_data;
	int verbose;
};


static char *strip_crap(const char *line_orig)
{
	size_t len;
	char *line;
	size_t outptr;
	int i;
	int escape_seq;

	len = strlen(line_orig);

	line = malloc(len+1);
	if ( line == NULL ) return NULL;

	/* Remove non-ASCII characters and ANSI escape sequences */
	outptr = 0;
	escape_seq = 0;
	for ( i=0; i<len; i++ ) {
		char ch = line_orig[i];
		if ( (ch >= 0x1f ) && (ch < 0xff) && !escape_seq ) {
			line[outptr++] = ch;
		}
		if ( ch == 0x1b ) escape_seq = 1;
		if ( escape_seq && ch == 'm' ) escape_seq = 0;
	}
	line[outptr] = '\0';

	return line;
}


static void process_line(const char *line_orig, ReplConnection *repl)
{
	SCM port, str, sexp;
	char *line = strip_crap(line_orig);

	if ( repl->verbose ) {
		printf("%p recv: '%s'\n", repl, line);
	}

	str = scm_from_utf8_string(line);
	port = scm_open_input_string(str);
	sexp = scm_read(port);

	repl->process_func(sexp, repl->process_func_data);

	scm_close_port(port);
	free(line);
}


static void input_ready(GObject *source, GAsyncResult *res, gpointer vp)
{
	GError *error = NULL;
	ReplConnection *repl = vp;
	size_t len;
	int i;
	char *nl_pos;
	char *remaining;

	len = g_input_stream_read_finish(G_INPUT_STREAM(source), res, &error);

	if ( len == 0 ) {
		printf("%p: EOF\n", repl);
		g_object_unref(repl->conn);
		repl->conn = NULL;
		return;
	}

	if ( len == -1 ) {
		printf("%p: Error: %s\n", repl, error->message);
		g_object_unref(repl->conn);
		repl->conn = NULL;
		return;
	}

	repl->inbuf[len] = '\0';

	for ( i=0; i<len; i++ ) {
		if ( repl->inbuf[i] == '\0' ) {
			repl->inbuf[i] = ' ';
			fprintf(stderr, "Zero byte found in REPL output\n");
		}
	}

	if ( strlen(repl->input) + len + 1 >= 2048 ) {
		fprintf(stderr, "Too much input!\n");
	} else {
		strcat(repl->input, repl->inbuf);
	}

	nl_pos = strchr(repl->input, '\n');
	while ( nl_pos != NULL ) {

		size_t len_remaining = strlen(nl_pos);

		nl_pos[0] = '\0';
		process_line(repl->input, repl);

		memmove(repl->input, nl_pos+1, len_remaining);
		nl_pos = strchr(repl->input, '\n');
	}

	remaining = strip_crap(repl->input);
	if ( strcmp(remaining, "scheme@(guile-user)> ") == 0 ) {
		printf("Prompt!\n");
		repl->input[0] = '\0';
	}
	free(remaining);

	g_input_stream_read_async(g_io_stream_get_input_stream(G_IO_STREAM(repl->conn)),
	                          repl->inbuf, 1023, G_PRIORITY_DEFAULT, NULL,
	                          input_ready, repl);
}


ReplConnection *repl_connection_new(const char *socket,
                                    void (*process_func)(SCM sexp, void *data),
                                    void *data,
                                    int verbose)
{
	ReplConnection *repl;
	GSocketClient *client;
	GSocketAddress *addr;
	GError *error = NULL;

	repl = malloc(sizeof(struct _replconnection));
	if ( repl == NULL ) return NULL;

	addr = g_unix_socket_address_new(socket);
	if ( addr == NULL ) return NULL;

	client = g_socket_client_new();
	repl->conn = g_socket_client_connect(client, G_SOCKET_CONNECTABLE(addr),
	                                     NULL, &error);
	if ( repl->conn == NULL ) {
		fprintf(stderr, "Couldn't connect: %s\n", error->message);
		return NULL;
	}

	repl->input[0] = '\0';
	scm_init_guile();

	repl->process_func = process_func;
	repl->process_func_data = data;
	repl->verbose = verbose;

	repl_send(repl, "(let loop ()"
	                "  (let ((line (read)))"
	                "    (unless (eq? line 'exit)"
	                "      (let ((res (eval line (interaction-environment))))"
	                "        (unless (unspecified? res)"
	                "          (write res)"
	                "          (newline)))"
	                "      (loop))))");

	g_input_stream_read_async(g_io_stream_get_input_stream(G_IO_STREAM(repl->conn)),
	                          repl->inbuf, 1023, G_PRIORITY_DEFAULT, NULL,
	                          input_ready, repl);

	return repl;
}


int repl_send(ReplConnection *repl, const char *line)
{
	GError *error = NULL;
	GOutputStream *out = g_io_stream_get_output_stream(G_IO_STREAM(repl->conn));
	if ( repl->verbose ) {
		printf("%p send: %s\n", repl, line);
	}
	if ( g_output_stream_write(out, line, strlen(line), NULL, &error) == -1 ) {
		fprintf(stderr, "Couldn't send: %s\n", error->message);
		return 1;
	}
	if ( g_output_stream_write(out, "\n", 1, NULL, &error) == -1 ) {
		fprintf(stderr, "Couldn't send newline: %s\n", error->message);
		return 1;
	}
	return 0;
}


int repl_closed(ReplConnection *repl)
{
	return repl->conn == NULL;
}


void repl_connection_close(ReplConnection *repl)
{
	repl_send(repl, "exit");
	repl_send(repl, ",q");
}
