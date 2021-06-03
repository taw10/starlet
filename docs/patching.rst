=================
Patching fixtures
=================

To make Starlet aware of a lighting fixture (in theatrical parlance, to "patch"
a fixture), use ``patch-fixture!`` from module ``(starlet scanout)``.  You will
need to import the fixture definition from the fixture library.  For example,
to patch a simple dimmer with DMX address 32 on universe 4::

  (use-modules
    (starlet scanout)
    (starlet fixture-library generic dimmer))

  (patch-fixture! my-dimmer <generic-dimmer> 32 #:universe 4)

Universe numbering starts from zero (consistent with OLA's numbering), and
channel numbering starts from 1 (consistent with every other system on the
planet).

After the above, the symbol ``my-dimmer`` will be bound to an object
representing the dimmer::

  scheme@(guile-user)> my-dimmer
  $1 = #<<generic-dimmer> 7f9ee817f300>

Intelligent fixtures should go to their home positions immediately after being
patched.


Lists of fixtures
=================

Starlet fixture objects are just normal `GOOPS
<https://www.gnu.org/software/guile/manual/html_node/GOOPS.html>`_ objects.  You
can do normal Scheme-y things with them, such as making lists of them::

  (define red-backlight (list backlight-red-usl
                              backlight-red-usr
                              backlight-red-dsl
                              backlight-red-dsr))

Procedures such as ``at`` work with these lists in the same way that they work
on individual fixtures::

  (at red-backlight 100)

See the documentation about setting attributes for more information on this
topic.


Multiple fixtures with similar names
====================================

At this point, you might be tempted to create a standard file which defines all
the fixtures in your venue, naming them ``dimmer1``, ``dimmer2``, ``dimmer3``
and so on.  This is fine, and certainly one way of using Starlet.  However,
there is no shortcut for patching multiple fixtures with systematically-related
names like this.  The reason for this is something highly technical related to
top-level bindings introduced by macros in Guile modules - `read here
<https://www.gnu.org/software/guile/manual/html_node/Hygiene-and-the-Top_002dLevel.html>`_
if you're interested.

Before you protest too much about this, keep in mind that you are supposed to
use descriptive names for your fixture objects.  When using Starlet, you
shouldn't need to constantly look up fixture names (or worse, numbers) in a
lighting plan.  Instead, give your fixtures names which represent what they do,
for example: ``balcony-front-warm``, ``followspot`` or ``throne-spot``.
Note that you can give a fixture multiple names.  For example, if the spotlight
you use for a throne (``throne-spot``) is re-used to light a table::

  (define table-spot throne-spot)

This leaves the door open for replacing ``table-spot`` with a separate fixture
later on, if the re-usage doesn't work out as you expected.  In that case,
simply replace the above ``define`` call with a new call to ``patch-fixture!``.


Patching multiple fixtures at once
==================================

Despite the above, there will probably be times when you have a large number of
fixtures that should be treated to a greater extent as one entity.  For this
situation, use ``patch-many!``, which patches a series of fixtures of the same
type.  For example, to create eight dimmers with DMX addresses numbered 2, 4,
6... ::

  (patch-many! foh-warm <generic-dimmer> '(2 4 6 8 10 12 14 16))

Symbol ``foh-warm`` will be bound to a list containing the fixture objects::

  scheme@(guile-user)> foh-warm
  $2 = (#<<generic-dimmer> 7f6da7c3dc00> #<<generic-dimmer> 7f6da7c3db80>
        #<<generic-dimmer> 7f6da7c3db00> #<<generic-dimmer> 7f6da7c3da80>
        #<<generic-dimmer> 7f6da7c3da40> #<<generic-dimmer> 7f6da7c3d9c0>
        #<<generic-dimmer> 7f6da7c3d940> #<<generic-dimmer> 7f6da7c3d8c0>)

Instead of explicitly specifying the list of addresses, you could use ``iota``.
For example, this time putting the dimmers on universe 3::

  (patch-many! foh-warm <generic-dimmer> (iota 8 2 2)
               #:universe 3)

Hopefully obviously, the fixtures in one ``patch-many!`` call all need to be on
the same DMX universe.

For the rare situation when you need to control a single fixture from the list
separately to the others, create a new binding to an item from the list::

  (define forestage-warm-patch (list-ref foh-warm 2))

As before, this leaves an easy way to install a dedicated fixture for the
purpose, should it later become necessary, without having to re-write your
entire lighting program.
