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

class QBitArrayPrinter:
    """Print a Qt5 QBitArray"""

    def __init__(self, val):
        self.val = val

    def to_string(self):
        d = self.val['d']['d']
        data = d.reinterpret_cast(gdb.lookup_type('char').pointer()) + d['offset']
        size = (int(d['size']) << 3) - int(data[0])

        bits = []
        for i in range(size):
            if data[1 + (i >> 3)] & (1 << (i&7)):
                bits.append('1')
            else:
                bits.append('0')
        return ''.join(bits)

    def display_hint(self):
        return 'bitstring'

class QByteArrayPrinter:
    """Print a Qt5 QByteArray"""

    def __init__(self, val):
        self.val = val

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

    def __init__(self, val):
        self.val = val

    def children(self):
        size = int(self.val['d']['size'])

        if size == 0:
            return []

        el_type = self.val.type.template_argument(0)

        result = []
        node = self.val['e']['n']
        for i in range(size):
            result.append((str(i), node['t']))
            node = node['n']
        return result

    def to_string(self):
        # if we return an empty list from children, gdb doesn't print anything
        if self.val['d']['size'] == 0:
            return '= {}'
        return ''

    def display_hint(self):
        return 'array'

class QListPrinter:
    """Print a Qt5 QList"""

    def __init__(self, val):
        self.val = val

    def children(self):
        d = self.val['d']
        array = d['array']
        begin = int(d['begin'])
        size = int(d['end']) - begin

        if size == 0:
            return []

        if self.val.type.name == 'QStringList':
            el_type = gdb.lookup_type('QString')
        else:
            el_type = self.val.type.template_argument(0)

        if ((el_type.sizeof > gdb.lookup_type('void').pointer().sizeof)
                or typeinfo.type_is_known_static(el_type)):
            is_pointer = True
        elif (typeinfo.type_is_known_movable(el_type) or
                typeinfo.type_is_known_primitive(el_type)):
            is_pointer = False
        else:
            raise ValueError("Could not determine whether QList stores " +
                    el_type.name + " directly or as a pointer")
        node_type = gdb.lookup_type(self.val.type.name + '::Node').pointer()
        result = []
        for i in range(size):
            node = array[begin + i].reinterpret_cast(node_type)
            if is_pointer:
                p = node['v']
            else:
                p = node
            result.append((str(i), p.cast(el_type)))
        return result

    def to_string(self):
        # if we return an empty list from children, gdb doesn't print anything
        if self.val['d']['begin'] == self.val['d']['end']:
            return '= {}'
        return ''

    def display_hint(self):
        return 'array'

class QMapPrinter:
    """Print a Qt5 QMap"""

    def __init__(self, val):
        self.val = val

    def node_to_list(self, node):
        if node:
            left = node['left'].reinterpret_cast(node.type)
            right = node['right'].reinterpret_cast(node.type)
            return (self.node_to_list(left) +
                    [('key',node['key']),('value',node['value'])] +
                    self.node_to_list(right))
        else:
            return []

    def children(self):
        d = self.val['d']
        size = int(d['size'])

        if size == 0:
            return []

        keytype = self.val.type.template_argument(0)
        valtype = self.val.type.template_argument(1)
        node_type = gdb.lookup_type('QMapData<' + keytype.name + ',' + valtype.name + '>::Node')

        return self.node_to_list(d['header']['left'].reinterpret_cast(node_type.pointer()))

    def to_string(self):
        # if we return an empty list from children, gdb doesn't print anything
        if self.val['d']['size'] == 0:
            return '= {}'
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

        data = self.val['ptr']

        result = []
        for i in range(size):
            result.append((str(i),data[i]))
        return result

    def to_string(self):
        # if we return an empty list from children, gdb doesn't print anything
        if self.val['s'] == 0:
            return '= {}'
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

        result = []
        for i in range(data_len):
            result.append((str(i),data[i]))
        return result

    def to_string(self):
        # if we return an empty list from children, gdb doesn't print anything
        if self.val['d']['size'] == 0:
            return '= {}'
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
    pp.add_printer('QVector', '^QVector<.*>$', QVectorPrinter)
    pp.add_printer('QVarLengthArray', '^QVarLengthArray<.*>$', QVarLengthArrayPrinter)
    return pp

printer = build_pretty_printer()
"""The pretty printer for Qt5Core.

This can be registered using gdb.printing.register_pretty_printer().
"""
