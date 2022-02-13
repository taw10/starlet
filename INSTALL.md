Getting started with Starlet
============================

Set up OLA
----------
First, install and set up [OLA](https://openlighting.org) for your lighting
environment. Start olad if it's not already running: `olad -l 3` (in a separate
terminal).  Patch all the output interfaces for your system (see the OLA
documentation for details).

Use the web interface (http://127.0.0.1:9090/ola.html) to check that you can
control lights properly.


Set up Guile
------------
Install [Guile](https://www.gnu.org/software/guile/), if it's not already
present.  Version 3 is required.  You will need to install the development
files (`guile-devel` or similar) as well.

There may be multiple parallel installations of Guile on your system, so make
sure you know the command for launching version 3.  On Fedora, the command is
`guile3.0`.


Compile Starlet code
--------------------
Most of Starlet is written in pure Scheme, but there is a small interface
library written in C++ as well as some GUI programs written in C.  Compile and
install them as follows:

```
$ meson build
$ ninja -C build
$ sudo ninja -C build install
```


Starting up
-----------

Run `guile`, giving it the path of the Starlet scheme code.  From the top-level
Starlet folder:

```
$ guile -L guile --listen=/home/myself/guile.socket
```

`/home/myself/guile.socket` will be the name of the Unix domain socket to which
the GUI utilities.  You can also connect an interactive coding system such as
[Conjure](https://conjure.fun/) or [Geiser](http://www.nongnu.org/geiser/).

Continue with [patching fixtures](docs/patching.rst).

