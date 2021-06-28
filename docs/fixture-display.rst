=========================
Graphical fixture display
=========================

The Starlet fixture display tool creates a graphical view of your fixtures,
more like a traditional lighting desk.  It will show a rectangle representing
each patched fixture, indicating the selected fixtures via a highlight.

As well as displaying information, the fixture display responds to a few key
presses: escape to deselect all fixtures, as well as keypad 'enter' and 'plus'
for sending 'go' and 'stop' commands (respectively) to a playback named ``pb``.

This tool communicates with Starlet in the same way that you do, the Guile
REPL.  You'll need to tell Guile to create a Unix domain socket to connect to,
via an additional option::

  guile --listen=/home/user/guile.socket

Now start the fixture display tool with::

  starlet-fixture-display -s /home/user/guile.socket

Obviously, the filenames for both commands must match. Replace it with
something appropriate for your setup.

Note that Guile requires an absolute path for the socket filename, otherwise it
will be interpreted as a network location.  Exposing the REPL via a network
port should work as well, but you should not do it that way unless you
**really** understand the risks.

Add option ``-v`` to the fixture display tool to see the REPL communications.
Warning: extreme console spam if you use this option.

