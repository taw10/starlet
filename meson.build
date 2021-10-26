# Meson file for Starlet
project('starlet', ['c', 'cpp'],
        version: '0.1.0',
        license: 'GPL3+',
        default_options: ['buildtype=debugoptimized'])


# Localisation
subdir('po')
add_project_arguments('-DLOCALEDIR="'+join_paths(get_option('prefix'), get_option('localedir'))+'"',
                      language: 'c')


# Dependencies
gnome = import('gnome')
cc = meson.get_compiler('c')
gtk_dep = dependency('gtk+-3.0', required: true)
cairo_dep = dependency('cairo', required: true)
pango_dep = dependency('pango', required: true)
pangocairo_dep = dependency('pangocairo', required: true)
guile_dep = dependency('guile-3.0', required: true)
ola_dep = dependency('libola', required: true)

# The installation location for Scheme files
guile_sitedir = guile_dep.get_pkgconfig_variable('sitedir')


# Guile OLA library
library('guile-ola', ['src/guile-ola.cpp'],
        dependencies: [guile_dep, ola_dep],
        install: true)


# Fixture display tool
executable('starlet-fixture-display',
           ['src/starlet-fixture-display.c',
            'src/repl-connection.c'],
           dependencies: [gtk_dep,
                          cairo_dep,
                          pango_dep,
                          pangocairo_dep,
                          guile_dep],
           install: true)


# Playback display tool
executable('starlet-playback-display',
           ['src/starlet-playback-display.c',
            'src/repl-connection.c'],
           dependencies: [gtk_dep,
                          cairo_dep,
                          pango_dep,
                          pangocairo_dep,
                          guile_dep],
           install: true)


# Install Scheme source files (all at once)
install_subdir('guile/starlet', install_dir: guile_sitedir)
