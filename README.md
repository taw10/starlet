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
        (cue-state (blackout))
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

Everything from a simple dimmers to wacky multi-parameter fixtures are supported.  New fixture classes can be defined using some simple Scheme code.  Patching fixtures looks like this:

```
(define dimmer1 (patch-fixture! <generic-dimmer> 1))
(define dimmer2 (patch-fixture! <generic-dimmer> 3))
(define balcony-backlight1 (patch-fixture! <generic-dimmer> 18))
(define balcony-backlight2 (patch-fixture! <generic-dimmer> 19))
(define footlights (patch-fixture! <generic-dimmer> 23))
(define moving-light (patch-fixture! <robe-dl7s-mode1> 59))
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