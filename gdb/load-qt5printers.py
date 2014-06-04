# Loads pretty printers for Qt5.
#
# Source this from gdb, like:
# source /path/to/kde-dev-scripts/gdb/load-qt5printers.py
#
# Note that, unless you work exclusively with Qt5-based software, you
# probably do not want to load this in your .gdbinit, as if you debug
# a Qt4-based piece of software, the pretty printers will just break.
#
# See the create-qt5-autoloaders.py script for an alternative loading
# mechanism.
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


import sys
import os.path

moddir = os.path.dirname(__file__)
if not moddir in sys.path:
    sys.path.insert(0, moddir)

import qt5printers
qt5printers.register_printers(gdb.current_objfile())
