=================================
Using physical controls (via OSC)
=================================

Starlet's OSC implementation uses liblo via Guile-OSC.  You can use Guile-OSC
procedures directly for many things.  Starlet provides utilities for
higher-level control primitives.

Start OSC by creating a server thread.  This will receive OSC method calls from
other programs.  You can customise the port number and protocol (UDP vs TCP) as
you need::

  (define osc-server (make-osc-server-thread "osc.udp://:7770"))

You'll also need to create an OSC address object for each external program that
will receive OSC calls from Starlet::

  (define controller (make-osc-address "osc.udp://localhost:7771"))

Now, you can call external OSC methods like this::

  (osc-send controller "/my/controller/led/4/set-colour" 'red)

You can also create methods of your own, like this example which reloads a cue
list on a button press::

  (add-osc-method
    osc-server
    "/controller/buttons/30/press"
    ""
    (lambda ()
      (reload-cue-list! pb)
      (reassert-current-cue! pb)))

See the manual for `Guile-OSC <https://github.com/taw10/guile-osc>`_ for more
information about this part.


High-level controls
===================

The functions described below expect the high-level OSC interface as
implemented in `x1k2-midi-osc-alsa <https://github.com/taw10/x1k2-midi-osc-alsa>`_.
See the manual for more information about the protocol.  The sections below
describe how to use them in Starlet.  Since they need bi-directional
communication, you have to provide an OSC server and the OSC address for the
controller.


Selection buttons
-----------------

This gives you a button that adds a fixture or group to the selection when
pressed.  If the button has LEDs, they will be lit orange when selected, and
red otherwise::

  (osc-select-button front-wash osc-server controller-addr "/controller/buttons/18")


Playback controls
-----------------

This lets you control a playback object with buttons, which will light up to
show when a cue is running.  The buttons are (respectively, in argument order)
go, stop and back::

  (osc-playback-controls pb osc-server controller "/controller/buttons/102" "/controller/buttons/32" "/controller/buttons/28")


States on fader (submasters)
----------------------------

You can put an entire state on a fader.  Non-intensity parameters will be
asserted only when the fader is up (non-zero).  If it's not already there, the
fader will need to be picked up at the bottom of its run::

  (osc-state-fader osc-server controller "/controller/faders/4"
                   (lighting-state
                     (at mhL mhR colour (rgb 40 20 70))
                     (at mhL mhR 100)
                     (at front-wash 100)
                     (at domeL domeR 100)))


Parameter encoders and potentiometers
-------------------------------------

These give you physical control of an individual parameter in the programmer
state.  LEDs will be used to indicate whether any of the currently selected
fixtures have the named parameter.

The simplest form is an encoder.  This increases or decreases the parameter
value when turned.  Push and turn to make finer adjustments::

  (osc-parameter-encoder intensity osc-server controller "/controller/encoders/102")


Potentiometers are the same, but include a soft pickup mechanism because the
physical position might not match Starlet's view of the position::

  (osc-smart-potentiometer color-temperature osc-server controller "/controller/potentiometers/4")
