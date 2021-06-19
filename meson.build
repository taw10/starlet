# Meson file for Starlet
project('starlet', ['c', 'cpp'],
        version : '0.1.0',
        license : 'GPL3+',
        default_options : ['buildtype=debugoptimized'])

# Localisation
subdir('po')
add_project_arguments('-DLOCALEDIR="'+join_paths(get_option('prefix'), get_option('localedir'))+'"',
                      language : 'c')


# Dependencies
gnome = import('gnome')
cc = meson.get_compiler('c')
mdep = cc.find_library('m', required : false)
gtk_dep = dependency('gtk+-3.0', required : true)
cairo_dep = dependency('cairo', required : true)
pango_dep = dependency('pango', required : true)
pangocairo_dep = dependency('pangocairo', required : true)
guile_dep = dependency('guile-3.0', required : true)
ola_dep = dependency('libola', required : true)


# Guile OLA library
library('guile-ola', ['src/guile-ola.cpp'],
        dependencies : [guile_dep, ola_dep],
	install: true)


# Fixture display tool
executable('starlet-fixture-display',
           ['src/starlet-fixture-display.c',
            'src/repl-connection.c'],
           dependencies : [gtk_dep],
           install : true)
