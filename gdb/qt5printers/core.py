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
import typeinfo

"""Qt5Core pretty printer for GDB."""

# TODO:
# QDate/QTime/QDateTime
# QHash
# QPair? Is a pretty version any better than the normal dump?
# QSet
# QStringBuilder?
# QTimeZone? - just needs to print the m_id from the private header?
# QUrl

class ArrayIter:
    """Iterates over a fixed-size array."""
    def __init__(self, array, size):
        self.array = array
        self.i = -1
        self.size = size

    def __iter__(self):
        return self

    def __next__(self):
        if self.i + 1 >= self.size:
            raise StopIteration
        self.i += 1
        return ('[%d]' % self.i, self.array[self.i])

    def next(self):
        return self.__next__()

class QBitArrayPrinter:
    """Print a Qt5 QBitArray"""

    class Iter:
        def __init__(self, data, size):
            self.data = data
            self.i = -1
            self.size = size

        def __iter__(self):
            return self

        def __next__(self):
            if self.i + 1 >= self.size:
                raise StopIteration
            self.i += 1
            if self.data[1 + (self.i >> 3)] & (1 << (self.i&7)):
                return (str(self.i), 1)
            else:
                return (str(self.i), 0)

        def next(self):
            return self.__next__()

    def __init__(self, val):
        self.val = val

    def children(self):
        d = self.val['d']['d']
        data = d.reinterpret_cast(gdb.lookup_type('char').pointer()) + d['offset']
        size = (int(d['size']) << 3) - int(data[0])

        return self.Iter(data, size)

    def to_string(self):
        d = self.val['d']['d']
        data = d.reinterpret_cast(gdb.lookup_type('char').pointer()) + d['offset']
        size = (int(d['size']) << 3) - int(data[0])
        if size == 0:
            return '<empty>'
        return ''

    def display_hint(self):
        return 'array'

class QByteArrayPrinter:
    """Print a Qt5 QByteArray"""

    def __init__(self, val):
        self.val = val

    def children(self):
        d = self.val['d']
        data = d.reinterpret_cast(gdb.lookup_type('char').pointer()) + d['offset']
        return ArrayIter(data, d['size'])

    def to_string(self):
        d = self.val['d']
        data = d.reinterpret_cast(gdb.lookup_type('char').pointer()) + d['offset']
        return data.string('', 'replace', d['size'])

    def display_hint(self):
        return 'string'

class QLatin1StringPrinter:
    """Print a Qt5 QLatin1String"""

    def __init__(self, val):
        self.val = val

    def to_string(self):
        return self.val['m_data'].string('', 'replace', self.val['m_size'])

    def display_hint(self):
        return 'string'

class QLinkedListPrinter:
    """Print a Qt5 QLinkedList"""

    class Iter:
        def __init__(self, tail, size):
            self.current = tail
            self.i = -1
            self.size = size

        def __iter__(self):
            return self

        def __next__(self):
            if self.i + 1 >= self.size:
                raise StopIteration
            self.i += 1
            self.current = self.current['n']
            return (str(self.i), self.current['t'])

        def next(self):
            return self.__next__()

    def __init__(self, val):
        self.val = val

    def children(self):
        size = int(self.val['d']['size'])

        if size == 0:
            return []

        return self.Iter(self.val['e'], size)

    def to_string(self):
        # if we return an empty list from children, gdb doesn't print anything
        if self.val['d']['size'] == 0:
            return '<empty>'
        return ''

    def display_hint(self):
        return 'array'

class QListPrinter:
    """Print a Qt5 QList"""

    class Iter:
        def __init__(self, array, begin, end, typ):
            self.array = array
            self.end = end
            self.begin = begin
            self.offset = 0
            if typ.name == 'QStringList':
                self.el_type = gdb.lookup_type('QString')
            elif typ.name == 'QVariantList':
                self.el_type = gdb.lookup_type('QVariant')
            else:
                self.el_type = typ.template_argument(0)

            if ((self.el_type.sizeof > gdb.lookup_type('void').pointer().sizeof)
                    or typeinfo.type_is_known_static(self.el_type)):
                self.is_pointer = True
            elif (typeinfo.type_is_known_movable(self.el_type) or
                    typeinfo.type_is_known_primitive(self.el_type)):
                self.is_pointer = False
            else:
                raise ValueError("Could not determine whether QList stores " +
                        self.el_type.name + " directly or as a pointer: to fix " +
                        "this, add it to one of the variables in the "+
                        "qt5printers.typeinfo module")
            self.node_type = gdb.lookup_type(typ.name + '::Node').pointer()

        def __iter__(self):
            return self

        def __next__(self):
            if self.begin + self.offset >= self.end:
                raise StopIteration
            node = self.array[self.begin + self.offset].reinterpret_cast(self.node_type)
            if self.is_pointer:
                p = node['v']
            else:
                p = node
            self.offset += 1
            return ((str(self.offset), p.cast(self.el_type)))

        def next(self):
            return self.__next__()

    def __init__(self, val):
        self.val = val

    def children(self):
        d = self.val['d']
        begin = int(d['begin'])
        end = int(d['end'])

        if begin == end:
            return []

        return self.Iter(d['array'], begin, end, self.val.type)

    def to_string(self):
        # if we return an empty list from children, gdb doesn't print anything
        if self.val['d']['begin'] == self.val['d']['end']:
            return '<empty>'
        return ''

    def display_hint(self):
        return 'array'

class QMapPrinter:
    """Print a Qt5 QMap"""

    class Iter:
        def __init__(self, root, node_p_type):
            self.root = root
            self.current = None
            self.node_p_type = node_p_type
            self.next_is_key = True
            self.i = -1
            # we store the path here to avoid keeping re-fetching
            # values from the inferior (also, skips the pointer
            # arithmetic involved in using the parent pointer)
            self.path = []

        def __iter__(self):
            return self

        def moveToNextNode(self):
            if self.current is None:
                # find the leftmost node
                if not self.root['left']:
                    return False
                self.current = self.root
                while self.current['left']:
                    self.path.append(self.current)
                    self.current = self.current['left']
            elif self.current['right']:
                self.path.append(self.current)
                self.current = self.current['right']
                while self.current['left']:
                    self.path.append(self.current)
                    self.current = self.current['left']
            else:
                last = self.current
                self.current = self.path.pop()
                while self.current['right'] == last:
                    last = self.current
                    self.current = self.path.pop()
                # if there are no more parents, we are at the root
                if len(self.path) == 0:
                    return False
            return True

        def __next__(self):
            if self.next_is_key:
                if not self.moveToNextNode():
                    raise StopIteration
                self.current_typed = self.current.reinterpret_cast(self.node_p_type)
                self.next_is_key = False
                self.i += 1
                return ('key' + str(self.i), self.current_typed['key'])
            else:
                self.next_is_key = True
                return ('value' + str(self.i), self.current_typed['value'])

        def next(self):
            return self.__next__()

    def __init__(self, val):
        self.val = val

    def children(self):
        d = self.val['d']
        size = int(d['size'])

        if size == 0:
            return []

        keytype = self.val.type.template_argument(0)
        valtype = self.val.type.template_argument(1)
        node_type = gdb.lookup_type('QMapData<' + keytype.name + ',' + valtype.name + '>::Node')

        return self.Iter(d['header'], node_type.pointer())

    def to_string(self):
        # if we return an empty list from children, gdb doesn't print anything
        if self.val['d']['size'] == 0:
            return '<empty>'
        return ''

    def display_hint(self):
        return 'map'

class QStringPrinter:
    """Print a Qt5 QString"""

    def __init__(self, val):
        self.val = val

    def to_string(self):
        d = self.val['d']
        data = d.reinterpret_cast(gdb.lookup_type('char').pointer()) + d['offset']
        data_len = d['size'] * gdb.lookup_type('unsigned short').sizeof
        return data.string('utf-16', 'replace', data_len)

    def display_hint(self):
        return 'string'

class QVarLengthArrayPrinter:
    """Print a Qt5 QVarLengthArray"""

    def __init__(self, val):
        self.val = val

    def children(self):
        size = int(self.val['s'])

        if size == 0:
            return []

        return ArrayIter(self.val['ptr'], size)

    def to_string(self):
        # if we return an empty list from children, gdb doesn't print anything
        if self.val['s'] == 0:
            return '<empty>'
        return ''

    def display_hint(self):
        return 'array'

class QVectorPrinter:
    """Print a Qt5 QVector"""

    def __init__(self, val):
        self.val = val

    def children(self):
        d = self.val['d']
        el_type = self.val.type.template_argument(0)
        data_len = int(d['size'])

        if data_len == 0:
            return []

        data_char = d.reinterpret_cast(gdb.lookup_type('char').pointer()) + d['offset']
        data = data_char.reinterpret_cast(el_type.pointer())

        return ArrayIter(data, data_len)

    def to_string(self):
        # if we return an empty list from children, gdb doesn't print anything
        if self.val['d']['size'] == 0:
            return '<empty>'
        return ''

    def display_hint(self):
        return 'array'


def build_pretty_printer():
    """Builds the pretty printer for Qt5Core."""
    pp = gdb.printing.RegexpCollectionPrettyPrinter("Qt5Core")
    pp.add_printer('QBitArray', '^QBitArray$', QBitArrayPrinter)
    pp.add_printer('QByteArray', '^QByteArray$', QByteArrayPrinter)
    pp.add_printer('QLatin1String', '^QLatin1String$', QLatin1StringPrinter)
    pp.add_printer('QLinkedList', '^QLinkedList<.*>$', QLinkedListPrinter)
    pp.add_printer('QList', '^QList<.*>$', QListPrinter)
    pp.add_printer('QMap', '^QMap<.*>$', QMapPrinter)
    pp.add_printer('QQueue', '^QQueue<.*>$', QListPrinter)
    pp.add_printer('QStack', '^QStack<.*>$', QVectorPrinter)
    pp.add_printer('QString', '^QString$', QStringPrinter)
    pp.add_printer('QStringList', '^QStringList$', QListPrinter)
    pp.add_printer('QVariantList', '^QVariantList$', QListPrinter)
    pp.add_printer('QVector', '^QVector<.*>$', QVectorPrinter)
    pp.add_printer('QVarLengthArray', '^QVarLengthArray<.*>$', QVarLengthArrayPrinter)
    return pp

printer = build_pretty_printer()
"""The pretty printer for Qt5Core.

This can be registered using gdb.printing.register_pretty_printer().
"""
