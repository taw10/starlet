#!/bin/sh

set -x
g++ guile-ola.cpp -o libguile-ola.so -lola --shared -fPIC \
                  `pkg-config --cflags --libs guile-3.0`

echo Now run: sudo cp libguile-ola.so /usr/local/lib64/
