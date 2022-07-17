==================================
Using physical controls (MIDI etc)
==================================

Start MIDI control by creating a controller object.  You need to give the
device node for the controller, and the MIDI channel to use::

  (define controller
    (make-midi-controller "/dev/snd/midiC1D0" 14)

To send a MIDI "note on" command, e.g. to light an LED, use ``send-note-on``.
There is also ``send-note-off``::

  (send-note-on controller 25)
  (send-note-off controller 25)

Buttons
=======

You can assign a procedure to be called when a MIDI button is pressed::

  (register-midi-note-callback!
    controller
    #:note-number 25
    #:func (lambda ()
             (reload-cue-list! pb)
             (reassert-current-cue! pb)))

Let's say you want to use a MIDI button to quickly select all the fixtures in
your front wash.  You could assign a procedure with a call to ``sel`` inside
(see `<basic-control.rst>`_ for more about ``sel``).  However, there is a
helper procedure in module ``(starlet midi-control button-utils)``::

  (select-on-button controller 32
                    (list washL washM washR)
                    #:ready-note 68)

The ``#:ready-note`` is just a note to send, e.g. to light up an LED to
indicate that the button is active - leave it out if you don't need it.  Use
``#f`` as the fixture name to make a "deselect" button.

There are similar utilities for creating *go*, *stop* and *back* buttons
(see `<cue-list.rst>`_)::

  (make-go-button controller pb 12
                  #:ready-note 20
                  #:pause-note 16)

  (make-stop-button controller pb 24
                    #:ready-note 24)

  (make-back-button controller pb 28
                    #:ready-note 28)

With ``make-go-button``, the ``pause-note`` will be lit when the playback has
been paused with ``stop!`` (see `<cue-list.rst>`_), otherwise the
``ready-note`` will be lit.  On my controller (Allen and Heath XONE:K2), these
notes light the same button in different colours.

The ``ready-note`` for ``make-stop-button`` will be lit when a cue is running,
i.e. when the playback can usefully be stopped.

With ``make-go-button``, you can additionally use
``#:min-time-between-presses`` to set a minimum waiting time (in seconds)
between activations.  This sometimes helps with jittery fingers.  The default
is 0.2 seconds.


Putting a state on a fader
==========================

To simply assign a state to a fader::

  (state-on-fader controller 19
                  (lighting-state
                    (at front-wash 100)
                    (at domeL domeR 100)))

The fader's control of the state will be completely separate to other states,
i.e. it will not affect the contents of the programmer.  The programmer state
will have priority over states on faders.


Control map
===========

You can associate MIDI controls with fixture parameters, such that when a
fixture is selected, those controls can be used to change the parameters in the
programmer state.  Here's an example::

  (set-midi-control-map!
    controller
    (fader    16 'intensity                      #:congruent 108  #:incongruent 72)
    (jogwheel  0 'pan                            #:active 124)
    (jogwheel  1 'tilt                           #:active 125)
    (fader     4 (colour-component-id 'cyan)     #:congruent 120  #:incongruent 84)
    (fader     5 (colour-component-id 'magenta)  #:congruent 121  #:incongruent 85)
    (fader     6 (colour-component-id 'yellow)   #:congruent 122  #:incongruent 86)
    (fader     7 'colour-temperature             #:congruent 123  #:incongruent 87))

Use ``fader`` for MIDI *continuous control* parameters (CCs), which might
physically correspond to faders or rotary potentiometers with a minimum and
maximum value (distinct from *jogwheels* - see below).  You need to give the
MIDI CC number, then the parameter which should be controlled.  To control a
single component of the colour, use ``colour-component-id`` as shown in the
example.

Faders (and potentiometers) are somewhat awkward, because there's no guarantee
that the fader will be in the position corresponding to the current value of
the parameter it controls.  To avoid jumps, Starlet will only start changing
the parameter value once the fader passes through the correct value.  This
means that you might have to move the fader to "pick up" the value along its
way.  The ``congruent`` and ``incongruent`` values are optional, but highly
recommended.  They give the note numbers corresponding to two LEDs, one of
which will be lit when the fader position is "congruent" with the underlying
parameter value and moving the fader will change the value.  At other times,
the ``incongruent`` LED will be lit to show that the fader needs to "pick up"
the value.  Starlet will take care of getting this right even when you select
multiple fixtures at once with different values, or when the underlying value
is changed by some other means.

This is somewhat awkward, but on the other hand potentiometers are cheap and
your controller is likely to have a lot of them.  The alternative is to use
``jogwheel``, which is for CCs which don't give a continuous value but rather
either 0 or 127 to increase or decrease the value respectively.  This avoids
the whole "congruence" issue.  For ``jogwheel``, you only need to give the CC
number, the attribute to control, and an optional note to use for lighting an
LED to indicate when the parameter is active.
