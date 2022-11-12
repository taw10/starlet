===========================
Defining a new fixture type
===========================

To create a new fixture definition ("personality file"), use the
``define-fixture`` macro from ``(starlet fixture)``::

  (define-fixture fixture-class-name
                  (fixture-attributes
                     list of attributes ...)
                  scanout code ...)

The ``fixture-class-name`` should be a GOOPS class name (in triangular
brackets) for the fixture class, of the form ``<manufacturer-model-mode>``,
for example ``<robe-dl7s-mode3>`` for a Robe DL7S profile in mode 3.

Each attribute follows one of the following forms::

  (attr-continuous intensity '(0 100) 0)
  (attr-colour colour white)
  (attr-list prism '(#f 3 5) #f)

In all cases, you need to provide the name of the attribute.  The available
attribute names are enumerated in module (starlet attributes) - please add new
names if you need, but use the available names if possible.  The last argument
in each ``attr-`` form is always the default value for the attribute.

For ``attr-continuous``, you need to give the range of possible values.  For
``attr-list``, you need to give a list of the possible discrete values.  For
``attr-colour``, the value is always a Starlet colour object.

The list of attributes is followed by the `scanout code`.  This code will be
called to convert the attribute values into DMX values.

Retrieve the current values with calls of the form ``(get-attr intensity)``,
and set DMX values using ``(set-chan8 nn val)``, where ``val`` is the DMX value
(0 to 255) and ``nn`` is the channel number.  The channel numbers are indexed
from 1, i.e. ``(set-chan8 1 255)`` will set the fixture's base DMX address to
255.

There is also ``(set-chan16 nn val)``, which will set a pair of values to a
16-bit value between 0 and 65535.  The lower channel address will get the most
significant byte or 'coarse' value.

Note that you don't need access (nor do you get access) to the fixture
instance itself.  The routines ``get-attr``, ``set-chan8`` and ``set-chan16``
automatically know which fixture is being worked on at the time of the
procedure call.


Tips
====

The fixture class name should include enough of the fixture's name to
disambiguate it from other products by the same manufacturer.  For example
<stairville-octagon-theater-cw-ww> includes ``cw-ww`` to distinguish the
`CW/WW` (cold/warm wash) variant from the `CW/WW/A` (cold/warm/amber wash)
variant, which has a different channel layout.

Use the standard names (see below) for attributes as far as possible.  This
means that, for example, the same knob on a MIDI control surface can control
the same attribute across a range of different fixtures.  Note the UK-style
spelling of some of the attributes (e.g. ``colour``).

In spite of the above paragraph, use the exact manufacturer's spelling for the
fixture name itself.

Put the fixture definition into a separate Guile module named
``(starlet fixture-library <manufacturer> <fixture name>)``.
If the fixture has multiple modes, create one fixture class for each and put
them all in the same file.

Use physically meaningful units where possible.  For example, attribute
``colour-temperature`` should be in Kelvin, not arbitrary units.  This means
that a set of different fixtures types can all be set to the same value.  It
also makes it easily possible to substitute one fixture for a different one
without having to re-program the entire show.

Be prepared to do some work in the scanout code.  It's almost never as simple
as a 1:1 translation from the attributes to DMX channels. Even the cheap
5-channel LED cold/warm fixture in the example below includes some maths.


Worked example
==============

Here is an annotated version of the definition for the
`Stairville Octagon Theatre CW/WW <https://www.thomannmusic.com/stairville_octagon_theater_cw_ww_36x1w.htm>`_.
The channels for this fixture are:

  1. Cold LED intensity (0-255 min-max)
  2. Warm LED intensity (0-255 min-max)
  3. Strobe (0-15 off, 16-255 slow-fast)
  4. Macro (0-15 direct control with channels 1&2,
     16-255 various pre-programmed colour temperatures)
  5. Overall intensity

The approximate colour temperature range for the fixture is given in the
manual as 2800K to 6400K.  Temperatures in between will be achieved by mixing
the cold and warm LEDs such that the sum is always constant.  The midpoint
colour temperature (4600K) will therefore correspond to 50% cold and 50% warm
intensity.

Note that this design choice reduces the absolute maximum intensity possible
from this fixture, which would be achieved when channels 1, 2 and 5 are all at
maximum.  However, we gain the fact that the colour temperature and intensity
parameters are orthogonal: changing temperature will not change the intensity,
and vice-versa.  Given that Starlet is a *theatrical* lighting control system,
this kind of trade-off is preferable.

We will totally ignore the pre-programmed colour temperatures in favour of
having direct control over the cold and warm LED values.  The manual does not
even say what the pre-programmed temperatures are, so this is no loss at all.

Unfortunately, the manual doesn't say what frequencies are meant be 'slow' and
'fast' strobe.  We'll assume that 'slow' is 1 Hz, and 'fast' is 25 Hz.

Here is the code::

  ;; Define a Guile module for the fixture
  (define-module (starlet fixture-library stairville octagon-theater-cw-ww)

    #:use-module (starlet fixture)   ;; for define-fixture, attr-continuous etc
    #:use-module (starlet scanout)   ;; for set-chan8, get-attr etc
    #:use-module (starlet utils)     ;; for percent->dmxval8 etc
    #:export (<stairville-octagon-theater-cw-ww>))

  (define-fixture

    ;; Name of the fixture class
    <stairville-octagon-theater-cw-ww>

    ;; List of attributes
    (fixture-attributes
      (attr-continuous 'intensity '(0 100) 0)
      (attr-continuous 'colour-temperature '(2800 6400) 3200)
      (attr-list 'strobe '(#f #t) #f)
      (attr-continuous 'strobe-frequency '(1 25) 1))

    ;; Scanout code follows

    ;; Set unused macro channel to zero
    (set-chan8 4 0)

    ;; Set strobe channel
    (if (get-attr 'strobe)
      (set-chan8 3 (scale-and-clamp-to-range
                     (get-attr 'strobe-frequency)
                     '(1 25)
                     '(16 255)))
      (set-chan8 3 0))

    ;; Set intensity channel
    (set-chan8 5 (percent->dmxval8 (get-attr 'intensity))))

    ;; Set values of warm and cold LEDs according to colour temperature
    (let ((coltemp (get-attr 'colour-temperature)))
      (set-chan8 1 (scale-and-clamp-to-range coltemp '(2800 6400) '(0 255)))
      (set-chan8 2 (scale-and-clamp-to-range coltemp '(2800 6400) '(255 0))))

There are, of course, many more examples in ``guile/starlet/fixture-library``.

