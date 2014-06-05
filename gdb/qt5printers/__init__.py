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

import gdb.printing
import core

"""Qt5 Pretty Printers for GDB.

The printers are split into submodules, one for each Qt library. Each
submodule has a "printer" attribute that contains a pretty-printer for
that library.
"""

def register_printers(obj):
    """Registers all known Qt5 pretty-printers."""
    gdb.printing.register_pretty_printer(obj, core.printer)
