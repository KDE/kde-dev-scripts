# Creates GDB autoloader scripts for the Qt5 pretty-printers
#
# Create a directory for autoload scripts, like
# /home/myself/.gdb-autoload
#
# Add this directory to the GDB auto-load scripts-directory and
# safe-path settings in your ~/.gdbinit
#   set auto-load scripts-directory $debugdir:$datadir/auto-load:/home/myself/.gdb-autoload
#   set auto-load safe-path $debugdir:$datadir/auto-load:/home/myself/.gdb-autoload
#
# Run this script as
#   python create-qt5-autoloaders.py /home/myself/.gdb-autoload /usr/lib
# where /usr/lib should be replaced with the directory containing the
# Qt5 libraries (libQt5Core.so.5.*, etc)
#
# GDB will then auto-load the Qt5 pretty printers whenever the Qt5
# libraries are loaded (and unload them again when they are unloaded).
#


# Copyright 2014 Alex Merry <alex.merry@kde.org>
#
# Permission to use, copy, modify, and distribute this software
# and its documentation for any purpose and without fee is hereby
# granted, provided that the above copyright notice appear in all
# copies and that both that the copyright notice and this
# permission notice and warranty disclaimer appear in supporting
# documentation, and that the name of the author not be used in
# advertising or publicity pertaining to distribution of the
# software without specific, written prior permission.
#
# The author disclaims all warranties with regard to this
# software, including all implied warranties of merchantability
# and fitness.  In no event shall the author be liable for any
# special, indirect or consequential damages or any damages
# whatsoever resulting from loss of use, data or profits, whether
# in an action of contract, negligence or other tortious action,
# arising out of or in connection with the use or performance of
# this software.


import glob
import os
import os.path
import re
import sys

if len(sys.argv) != 3:
    print ("Bad number of arguments to " + sys.argv[0])
    print ("See the comments at the top of the script for usage")
    sys.exit(1)

autoloaddir = sys.argv[1]
libdir = sys.argv[2]

if not os.path.isdir(autoloaddir):
    print (autoloaddir + " is not a directory")
    print ("See the comments at the top of the script for usage")
    sys.exit(1)

autoloaddir = os.path.abspath(autoloaddir)

if not os.path.isdir(libdir):
    print (libdir + " is not a directory")
    print ("See the comments at the top of the script for usage")
    sys.exit(1)

libdir = os.path.abspath(libdir)

candidates = glob.iglob(libdir + "/*Qt5*")

file_contents = """
import gdb.printing

moddir = "{0}"
if not moddir in sys.path:
    sys.path.insert(0, moddir)

import qt5printers.{{0}}
gdb.printing.register_pretty_printer(gdb.current_objfile(), qt5printers.{{0}}.printer)
""".format(os.path.abspath(os.path.dirname(__file__)))

# NB: this works on Linux; the stuff for other platforms is my best
#     guess for what those files look like
mod_re = re.compile(r"(?:lib)?Qt5([^.]*)\.(?:so\.5\..*|dll|.*\.dylib)", re.I)

# keep track of what files we have generated, as we may encounter some
# that are symlinks to others (and we resolve all the symlinks)
seen = set()

for lib in candidates:
    lib = os.path.realpath(os.path.abspath(lib))
    if not lib in seen:
        target_filename = autoloaddir + lib + "-gdb.py"
        filename = os.path.basename(lib)
        match = mod_re.match(filename)
        if not match is None:
            seen.add(lib)
            module = match.group(1).lower()
            if os.path.exists('qt5printers/' + module + '.py') or os.path.exists('qt5printers/' + module + '/__init__.py'):
                if not os.path.isdir(os.path.dirname(target_filename)):
                    os.makedirs(os.path.dirname(target_filename))
                print ("Creating loader script for " + filename)
                with open(target_filename, 'w') as f:
                    f.write(file_contents.format(module, filename))
            else:
                print ("No pretty-printer module available for " + filename)

