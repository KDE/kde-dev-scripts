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

This top-level module contains some shared code and information. In
particular, since the QTypeInfo information is not necessarily available
at debug time, it contains useful type information about standard and Qt
types (such as whether a type is movable) that is necessary for the
operation of the printers. This information allows the QList printer,
for example, to determine how the elements are stored in the list.
"""

primitive_types = set([
    'QFlag',
    'QIncompatibleFlag',
    'QStaticPlugin',
    'QUuid',
    'HB_GlyphAttributes',
    'HB_FixedPoint',
    'QRegExpCharClassRange',
    'QRegExpAtom',
    'QRegExpAnchorAlternation',
    'QStringRef',
    'QTzType',
    'QCharAttributes'
    ])
"""Primitive (non-template) types.

This does not need to include compiler-primitive types (like int).

If you use the Q_DECLARE_TYPEINFO macro with Q_PRIMITIVE_TYPE flag, you
should add the type to this set. This is particularly important for
types that are the same size as a pointer or smaller.
"""

primitive_tpl_types = set(['QFlags'])
"""Primitive template types.

If you use the Q_DECLARE_TYPEINFO_BODY macro with Q_PRIMITIVE_TYPE flag
on a type with template parameters, you should add the type to this
set. This is particularly important for types that are the same size as
a pointer or smaller.

Entries should just be the base typename, without any template
parameters (eg: "QFlags", rather than "QFlags<T>").
"""

movable_types = set([
    'Point2D',
    'QFileSystemWatcherPathKey',
    'QLoggingRule',
    'QProcEnvValue',
    'QProcEnvKey',
    'QResourceRoot',
    'QModelIndex',
    'QItemSelectionRange',
    'QBasicTimer',
    'QMetaMethod',
    'QMetaEnum',
    'QMetaClassInfo',
    'QCustomTypeInfo',
    'QObjectPrivate::Connection',
    'QObjectPrivate::Sender',
    'QMimeMagicRule',
    'QPostEvent',
    'QChar',
    'QDate',
    'QTime',
    'QEasingCurve',
    'QHashDummyValue',
    'QLine',
    'QLineF',
    'QLocale',
    'QMargins',
    'QMarginsF',
    'QPoint',
    'QPointF',
    'QRect',
    'QRectF',
    'QRegExpAutomatonState',
    'QRegExpCharClass',
    'QRegExp',
    'QSize',
    'QSizeF',
    'QLatin1String',
    'QCharRef',
    'QString',
    'QStringList',
    'QTimeZone::OffsetData',
    'QXmlStreamAttribute',
    'QXmlStreamNamespaceDeclaration',
    'QXmlStreamNotationDeclaration',
    'QXmlStreamEntityDeclaration'
    ])
"""Movable (non-template) types.

If you use the Q_DECLARE_TYPEINFO macro with Q_MOVABLE_TYPE flag, you
should add the type to this set. This is particularly important for
types that are the same size as a pointer or smaller.
"""

movable_tpl_types = set([
    'QPointer',
    'QSharedDataPointer',
    'QExplicitlySharedDataPointer',
    'QWeakPointer',
    'QSharedPointer',
    'QList',
    'QVector',
    'QQueue',
    'QStack',
    'QLinkedList',
    'QSet'
    ])
"""Movable template types.

If you use the Q_DECLARE_TYPEINFO_BODY macro with Q_MOVABLE_TYPE flag
on a type with template parameters, you should add the type to this
set. This is particularly important for types that are the same size as
a pointer or smaller.

Entries should just be the base typename, without any template
parameters (eg: "QFlags", rather than "QFlags<T>").
"""

static_types = set()
"""Static (non-template) types.

If you define a custom type that is neither primitive nor movable, you
can add the type to this set to indicate this. This is particularly
important for types that are the same size as a pointer or smaller.
"""

static_tpl_types = set()
"""Static template types.

If you define a custom type with template parameters that is neither
primitive nor movable, you can add the type to this set to indicate
this. This is particularly important for types that are the same size as
a pointer or smaller.

Entries should just be the base typename, without any template
parameters (eg: "QFlags", rather than "QFlags<T>").
"""

def type_is_known_primitive(typ):
    """Returns True if the given gdb type is known to be primitive."""
    if typ.code == gdb.TYPE_CODE_PTR or typ.code == gdb.TYPE_CODE_INT or typ.code == gdb.TYPE_CODE_FLT or typ.code == gdb.TYPE_CODE_CHAR or typ.code == gdb.TYPE_CODE_BOOL:
        return True
    pos = typ.name.find('<')
    if pos > 0:
        return typ.name[0:pos] in primitive_tpl_types
    else:
        return typ.name in primitive_types

def type_is_known_movable(typ):
    """Returns True if the given gdb type is known to be movable."""
    pos = typ.name.find('<')
    if pos > 0:
        return typ.name[0:pos] in movable_tpl_types
    else:
        return typ.name in movable_types

def type_is_known_static(typ):
    """Returns True if the given gdb type is known to be neither primitive nor movable."""
    pos = typ.name.find('<')
    if pos > 0:
        return typ.name[0:pos] in static_tpl_types
    else:
        return typ.name in static_types

def register_printers(obj):
    """Registers all known Qt5 pretty-printers."""
    gdb.printing.register_pretty_printer(obj, core.printer)
