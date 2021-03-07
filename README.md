Starlet: Stage lighting control in Lisp
=======================================

Starlet is an experimental Lisp-based domain-specific language (DSL) for theatrical lighting control.  It's based on [Guile](https://www.gnu.org/software/guile/) and sends its DMX output via [OLA](https://openlighting.org) to almost any type of lighting control interface - DMX, sACN, Art-Net etc.

With Starlet, a cue list looks like this:

```
(define my-cue-list
  (cue-list

   (cue 1
        (cue-state (at dim1 '100))
                   (at mh1 'pan 25))
        #:fade-up 3
        #:fade-down 5)

   (cue 2
        (cue-state (at dim1 '50)
                   (at dim2 '100)
                   (at mh1 'pan 50))
        #:fade-up 3
        #:fade-down 1
        #:down-delay 3)

   (cue 3
        (cue-state #f)  ; blackout
        #:fade-down 2
```

Creating a playback object and running a cue list looks like this:

```
(define pb (make-playback my-cue-list))
(register-state! pb)

(cut-to-cue-number! pb 1)
(go! pb)
(go! pb)
(go! pb)  ; and so on
```

Lighting states can be prepared separately and assigned to variables:

```
(define spooky-dungeon
  (lighting-state
    (at dimmer1 20)
    (at dimmer2 20)
    (at moving-light 70)
    (at moving-light 'red 100)
    (at moving-light 'green 10)
    (at moving-light 'blue 12)))
```

You can use pre-prepared states in cues, even if some minor modifications are needed.  This makes it really easy to understand the contents of a cue without having to interpret a screenful of numbers:

```
(cue 57
     (cue-state (apply-state spooky-dungeon)
                (at follow-spot 100))
     #:fade-up 3
     #:fade-down 3)
```

Mult-part cues are supported.  Simply specify the fade parameters and which fixtures should be in the part:

```
(cue 64
     (cue-state (apply-state indoor-act1-general)
     #:fade-up 3
     #:fade-down 3

     (cue-part (dim3
                dim4
                dim8
                (list moving-light 'pan 'tilt))
               #:down-time 2
               #:down-delay 1))
```


Everything from a simple dimmers to wacky multi-parameter fixtures are supported.  New fixture classes can be defined using some simple Scheme code.  Patching fixtures looks like this:

```
(patch-fixture! dimmer1 <generic-dimmer> 1))
(patch-fixture! dimmer2 <generic-dimmer> 3))
(patch-fixture! balcony-backlight1 <generic-dimmer> 18))
(patch-fixture! balcony-backlight2 <generic-dimmer> 19))
(patch-fixture! footlights <generic-dimmer> 23))
;; Universe numbering starts at zero, matching OLA
(patch-fixture! moving-light <robe-dl7s-mode1> 1 #:universe 4))
```

Note that the names of the fixtures are just normal Scheme variables.  They can be anything you like, and you're encouraged to make the names more descriptive than logical channel numbers, where appropriate.

Getting started
---------------

1. Install and set up [OLA](https://openlighting.org) for your lighting environment.
2. Start olad if it's not already running: `olad &`
3. Install [Guile](https://www.gnu.org/software/guile/), if it's not already there (there's a good chance it is).  Version 3 is preferred because it's much faster (= ability to handle more fixtures with less CPU load), but version 2.2 works as well.
4. Run `guile -L /path/to/starlet/guile`
5. Once in the Guile REPL, import the Starlet modules: `(use-modules (starlet base) (starlet playback) (starlet fixture-library generic))`
6. Patch a fixture with `(define fix (patch-fixture! <generic-dimmer> 1 #:universe 2))` - replace 1 and 2 with the DMX address and universe (respectively) of a real dimmer.
7. Turn the dimmer on with `(at fix 100)`
8. Look in the _examples_ folder for more advanced ideas.


Related projects
----------------

There are many stage lighting software projects, but most of them seem to concentrate on "disco style" effects and chases whereas Starlet is aimed more towards theatre shows.  Here are some that I found especially interesting:


* [Fivetwelve-CSS](https://github.com/beyondscreen/fivetwelve-css) DMX lighting control using CSS. [Watch this video](https://www.youtube.com/watch?v=ani_MOZt5_c)
* [Afterglow](https://github.com/Deep-Symmetry/afterglow) Clojure live coding environment using OLA
* [QLC+](https://qlcplus.org/) the most popular open-source lighting control program

It's also worth taking a look at the [stage-lighting topic](https://github.com/topics/stage-lighting) on Github.


Licence
-------

Starlet is free software: you can redistribute it and/or modify it under the
terms of the GNU General Public License as published by the Free Software
Foundation, either version 3 of the License, or (at your option) any later
version.

Starlet is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A
PARTICULAR PURPOSE.  See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License along with
Starlet.  If not, see <http://www.gnu.org/licenses/>.
