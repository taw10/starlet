===========================================
Basic attribute control and building states
===========================================

Once your fixtures are patched (see `<patching.rst>`_), you can set attributes
using ``at``.  For example, to set the intensity of ``my-dimmer`` to 100%::

  (at my-dimmer intensity 100)

If you leave out the attribute name, then ``intensity`` will be assumed::

  (at my-dimmer 100)

The available parameters are determined by the fixture definition (see
`<new-fixture.rst>`_). For example::

  (at my-moving-light pan 32)
  (at my-moving-light tilt 70)
  (at my-moving-light zoom 60)

Not all fixtures accept a single number.  For example, ``colour``  takes a
colour object, which can be constructed using ``rgb`` or ``cmy``.  Starlet (or
rather,  the fixture definition code) is responsible for converting the colour
to the native representation used by the fixture::

  (at my-moving-light colour (rgb 80 23 25))

You can set attributes for multiple fixtures at once::

  (at spotCS spotSR spotSL 90)

Or you can set attributes for a list of fixtures::

  (define all-spots (list spotSL spotCS spotSR))
  (at all-spots 90)

You can mix individual fixtures and lists of fixtures.  However, you can only
set one attribute (to one value) at a time.


The selection
=============

When setting many attributes for one fixture, you can avoid typing the
fixture name each time by using the selection.  To select a fixture, call
``sel`` with the fixture(s) to select::

  (sel my-dimmer)
  (sel all-spots my-moving-light)

If the fixture name is left out from subsequent calls to  ``at``, they will
apply to the currently selected fixture(s).  To clear the selection, use
``(sel #f)`` or simply ``(sel)``::

  (sel my-moving-light)
  (at 100)
  (at colour (rgb 80 23 25))
  (at tilt 70)
  (at pan 32)
  (sel #f)

The fixture display window (see `<fixture-display.rst>`_) will highlight the
selection, and physical control devices (see `<physical-control.rst>`_) will
affect the selected fixture(s).

To see the contents of the selection, use ``(get-selection)``.


State objects
=============

Attribute values must be stored within a state object.  When you set an
attribute from the Guile REPL, the values will be stored in the programmer
state.  You can examine the contents using ``state-source``, which returns
the Scheme code corresponding to the state's contents::

  scheme@(guile-user)> (state-source programmer-state)
  $6 = (lighting-state (at ledLL colour (cmy 77.0 100.0 100.0)) (at ledRR colour (cmy 77.0 100.0 100.0)) (at washR intensity 85) (at washL intensity 85) (at ledLL intensity 40) (at ledRR intensity 40) (at washM intensity 85))

You can also use ``print-state``, which just pretty-prints the output of
``state-source``.  To reduce typing, you can use ``ps`` as a synonym for
``programmer-state``::

  scheme@(guile-user)> (print-state ps)
  (lighting-state
    (at ledLL colour (cmy 77.0 100.0 100.0))
    (at ledRR colour (cmy 77.0 100.0 100.0))
    (at washR intensity 85)
    (at washL intensity 85)
    (at ledLL intensity 40)
    (at ledRR intensity 40)
    (at washM intensity 85))

You can construct new states by wrapping your ``at`` forms inside
``lighting-state``.  These state objects can be used inside cue lists
(see `<cue-list.rst>`_), bound to variables etc::

  (define my-state
    (lighting-state
      (at spotSR spotSL 50)
      (at spotCS 100)))

The programmer state has priority over everything else (e.g. cue list
playbacks).  To avoid surprises, you should make sure that it's empty before
trying to run a show.  The ``clear-state!`` procedure empties a state object
of its contents, without deleting the state itself::

  (clear-state! ps)

There are some utility routines for handling states:

* ``(state-empty? my-state)`` returns ``#t`` if ``my-state`` is empty,
  otherwise ``#f``.
* ``(remove-fixture-from-state! my-state spotCS)`` removes all attributes for
  ``spotCS`` from ``my-state``.
* ``(remove-fixtures-from-state! my-state (list spotCS spotSL))`` is the same,
  but removes a list of fixtures from the state.
* ``(remove-selection-from-programmer!)`` removes from the programmer any
  attributes referring to fixtures which are currently selected.  It is
  defined as follows::

    (define (remove-selection-from-programmer!)
      (remove-fixtures-from-state!
        programmer-state
        (get-selection)))


Effects
=======

Attribute values aren't restricted to constants.  You can also provide a
function, for example::

  (let ((clock (make-clock)))
    (at washM
      (lambda ()
        (* 50
           (+ 1 (sin (* 2 (elapsed-time clock))))))))

That's obviously quite complicated, so use the functions in module
``(starlet effects)`` instead.  For a sine wave once every 2 seconds (0.5 Hz)
ranging between zero and 100%::

  (at washM (sinewave 0.5 0 100))
