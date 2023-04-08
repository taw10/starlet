=======================
Cue lists and playbacks
=======================

Anatomy of a cue
================

Here is cue 4.3 representing a 4 second crossfade to a dim lighting wash::

  (cue 4.3                     ;; Cue number 4.3
    (crossfade 4               ;; Cross-fade in 4 seconds
      (lighting-state          ;;
        (at washL washR 30)    ;; ...to this lighting state
        (at washM 40))         ;;

Other types of transition are possible.  For example, the simplest transition
of all is a "snap" (a hard, immediate change)::

  (cue 4.3
    (snap
      (lighting-state
        (at washL washR 30)
        (at washM 40))

The simplest transition effect is ``snap``, which produces a hard zero-time
transition to the cue.  The usual one is ``crossfade``, which produces a smooth
fade in the time you specify (in seconds).  It gives you a lot of control, for
example, to fade intensities up in 4 seconds but down in 2 seconds::

  (crossfade 4 2 state)

To delay the up fade part by 2 seconds relative to everything else::

  (crossfade 4 #:up-delay 2 state)

To delay the down fade part, use ``#:down-delay``.  To control the fade times
for non-intensity parameters, use ``#:attr-time`` and ``#:attr-delay``.
Putting it all together for a complicated crossfade::

  (crossfade 3 4
             #:up-delay 2
             #:attr-time 1
             #:attr-delay 1.5)

You can write your own transition effects, if you want some other kind of
transition.  Documentation pending, but look at snap-transition.scm and
crossfade.scm for some examples.

The lighting state does not have to feature a literal ``lighting-state`` form.
Let's say you defined a state somewhere else::

  (define my-state
    (lighting-state
      (at spotSR spotSL 50)
      (at spotCS 100)))

You can refer to that state in a cue like this::

  (cue 3
    (crossfade 3 5 my-state))

You can also layer changes on top of the state, by using ``apply-state`` (which
does exactly what its name suggests)::

  (cue 3
    (crossfade 3 5
      (lighting-state
        (apply-state my-state)
        (at upstage-spot))))

You can even make those changes conditional::

  (define spot-needed #f)

  (cue 3
    (crossfade 3 5
      (lighting-state
        (apply-state my-state)
        (when spot-needed
          (at upstage-spot 100)))))


Multi-part cues
===============

Sometimes you need certain fixtures to fade differently during the same cue.
To achieve this, simply add a separate transition for each part.
Here's an example (from a real show)::

  (cue 3

       (crossfade 6 #:up-delay 14
         (lighting-state
           (at highsideL intensity 100.0)
           (at highsideR intensity 100.0)
           (at front-leds colour (cmy 0 93 80))
           (at splitL splitR 70)
           (at washL washR 100)
           (at washM 50)))

       (crossfade 3
         (lighting-state
           (at portrait-spot 100)))

       (crossfade 3 #:up-delay 16
         (lighting-state
           (at front-leds 100))))

In this example, the ``portrait-spot`` fades up first, in 3 seconds.  The main
part of the scene fades up more slowly, in 6 seconds after a delay of 14
seconds.  The ``front-leds`` (a group containing all of the front-light LED
fixtures) fades up a further 2 seconds after that.  Note that the cue parts
don't need to appear in chronological order.  However, the first cue part is
"special", because it's the one *into* which the other parameters will track
(see below).


Cue lists
==========

A cue list is simply a list of cues.  For example::

  (cue-list

    (cue 0.5
         ;; Tab warmers
         (snap
           (lighting-state
             (at washL washR 30)
             (at washM 40))))

    (cue 0.8
         (crossfade 6 blackout))

    (cue 1
         ;; Act 1, Scene 1
         (crossfade 3
           (lighting-state
             (at front-wash 80)
             (at moverL colour (cmy 21 0 0))
             (at moverL 25))))

    (cue 2
         (crossfade 3 4   ;; Separate up/down fade times
           (lighting-state
             (at washM 100))))

    (cue 2.5
         (crossfade 2
           (lighting-state
             (apply-state home-state)
             (at moverR 100))))

Just so you know, the cue list is represented internally as a Scheme *vector*,
not a real list.


Playback objects
================

The cue list doesn't do anything on its own.  To actually see the contents on
the stage, it needs to be loaded into a playback.  In practice, the best way to
work is to put the cue list in a file on its own and create the playback with
a reference to that file::

  (define pb
    (make-playback
      #:cue-list-file "shows/my-show.qlist.scm"
      #:recovery-file "recovery.q"))

The ``#:recovery-file`` is optional but highly recommended, discussed below.

Once the playback has been created like this, if you change the cue list file
then you can re-load it::

  scheme@(guile-user)> (reload-cue-list! pb)
  $8 = cue-list-reloaded

If the modifications to the cue list file involved the currently active cue,
the state shown on the stage will *not* be updated until you say so, with::

  (reassert-current-cue! pb)

The playback object shows useful information when printed::

  scheme@(guile-user)> pb
  $1 = #<<starlet-playback> state: ready current-cue: 43.0 next-cue: 44.0>

For completeness, know that you can also create a playback like this::

  (define my-cue-list
    (cue-list
       (cue ...)))

  (define pb (make-playback #:cue-list my-cue-list))

However, this makes it much harder to make subsequent changes to the cue list.


Running cues
============

To rapidly jump (with a snap transition) to a cue, use ``cut-to-cue-number!``.
To run a cue using the transition specified in the cue list, use
``run-cue-number!``::

  (cut-to-cue-number! pb 1)
  (run-cue-number! pb 4)

Calling ``go!`` will run the next cue in the cue list::

  (go! pb)

Playbacks also implement the other familiar operations:

* ``(stop! pb)`` - immediately pause any running cue.  The next call to
  ``go!`` will continue it.
* ``(cut! pb)`` - run the next cue, using a snap transition regardless of what
  the cue specifies.
* ``(back! pb)`` - go backwards one step in the cue list, using a snap
  transition.


Tracking
========

By default, non-intensity parameters will "track" from one cue into the next
cue.  That helps to avoid unexpected parameter changes, e.g. a moving light
changing position while it dims.  If you run cues *out of order*, the result
will be the same as if you'd run the cues *in order* from the start, to get to
the cue you wanted.  If you're lucky enough to have never encountered a system
that works any other way, just know that it works the way you'd expect it to
work in a theatrical system.

If you additionally want to track *intensities* into a cue, add
``track-intensities`` as the first thing after the cue number::

    (cue 1
         (crossfade 3
           (lighting-state
             (at front-wash 80))))

    (cue 2
         track-intensities
         (crossfade 3
           (lighting-state
             (at spotC 100))))

In this example, cue 2 will include ``spotC`` at full intensity, **and**
``front-wash`` at 80% intensity.


Fixture presetting ("auto move while dark")
===========================================

Starlet tries as hard as it can to get non-intensity parameters into the right
state before running a cue.  In other words, it makes a big effort to avoid the
audience seeing moving lights actually move.  If a fixture's ``intensity``
parameter is zero after running a cue, Starlet will set all its non-intensity
parameters to the values in the next cue.  Of course, if a non-intensity
parameter changes while the intensity is non-zero, the audience will see the
move!


The recovery file
=================

The purpose of the playback recovery file is to make a rapid recovery after a
crash (not that there will be any, of course!).  If the file specified by the
``#:recovery-file`` keyword argument to ``make-playback`` exists when the
playback is created, the playback will immediately jump to the cue number in
the file.  Whenever you run (or jump to) a cue, the cue number in the file will
be updated.  If you don't use a recovery file, the playback will revert to cue
zero on creation and you'll have to use ``cut-to-cue-number!``.  That will
create a blackout of a few seconds while you figure out the right cue number to
pick up from where things went wrong.
