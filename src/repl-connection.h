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


#ifndef REPL_CONNECTION_H
#define REPL_CONNECTION_H

typedef struct _replconnection ReplConnection;

extern ReplConnection *repl_connection_new(const char *socket,
                                           void (*process_func)(SCM sexp, void *data),
                                           void *data,
                                           int verbose);
extern int repl_send(ReplConnection *repl, const char *line);
extern void repl_connection_close(ReplConnection *repl);
extern int repl_closed(ReplConnection *repl);

#endif /* REPL_CONNECTION_H */
