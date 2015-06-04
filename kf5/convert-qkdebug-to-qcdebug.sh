#!/bin/sh

if test $# != 3; then
  echo "Usage : $0 <new log name> <new category name> <debug name file>"
  echo "example : convert-qdebug-to-qcdebug.sh log_smtp SMTP_LOG smtp_debug"
  exit 1
fi


newlogname="$1"
newcategoryname="$2"
debugnamefile="$3"

upname=`echo $debugnamefile | tr 'a-z' 'A-Z'`

# create debug.h file
cat > $debugnamefile.h <<EOF
/*  This file is part of the KDE project
    Copyright (C) 2015 Laurent Montel <montel@kde.org>

    This library is free software; you can redistribute it and/or
    modify it under the terms of the GNU Library General Public
    License as published by the Free Software Foundation; either
    version 2 of the License, or (at your option) any later version.

    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Library General Public License for more details.

    You should have received a copy of the GNU Library General Public License
    along with this library; see the file COPYING.LIB.  If not, write to
    the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
    Boston, MA 02110-1301, USA.
*/

#ifndef ${upname}_H
#define ${upname}_H

#include <QLoggingCategory>
Q_DECLARE_LOGGING_CATEGORY($newcategoryname)

#endif

EOF


#create debug.cpp file
cat > $debugnamefile.cpp <<EOF
/*  This file is part of the KDE project
    Copyright (C) 2015 Laurent Montel <montel@kde.org>

    This library is free software; you can redistribute it and/or
    modify it under the terms of the GNU Library General Public
    License as published by the Free Software Foundation; either
    version 2 of the License, or (at your option) any later version.

    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Library General Public License for more details.

    You should have received a copy of the GNU Library General Public License
    along with this library; see the file COPYING.LIB.  If not, write to
    the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
    Boston, MA 02110-1301, USA.
*/

#include "$debugnamefile.h"
Q_LOGGING_CATEGORY($newcategoryname, "$newlogname")


EOF

#convert debug to new qCDebug/qCWarning/qCCritical

find -iname "*.cpp" -o -iname "*.h" | xargs perl -pi -e "s,qDebug\s*\(\s*\),qCDebug\($newcategoryname\),"
find -iname "*.cpp" -o -iname "*.h" | xargs perl -pi -e "s,qWarning\s*\(\s*\),qCWarning\($newcategoryname\),"
find -iname "*.cpp" -o -iname "*.h" | xargs perl -pi -e "s,qFatal\s*\(\s*\),qCCritical\($newcategoryname\),"
find -iname "*.cpp" -o -iname "*.h" | xargs perl -pi -e "s,qError\s*\(\s*\),qCCritical\($newcategoryname\),"
find -iname "*.cpp" -o -iname "*.h" | xargs perl -pi -e "s,qCritical\s*\(\s*\),qCCritical\($newcategoryname\),"
find -iname "*.cpp" -o -iname "*.h" | xargs perl -pi -e "s,kDebug\s*\(\s*\),qCDebug\($newcategoryname\),"
find -iname "*.cpp" -o -iname "*.h" | xargs perl -pi -e "s,kWarning\s*\(\s*\),qCWarning\($newcategoryname\),"
find -iname "*.cpp" -o -iname "*.h" | xargs perl -pi -e "s,kFatal\s*\(\s*\),qCCritical\($newcategoryname\),"
find -iname "*.cpp" -o -iname "*.h" | xargs perl -pi -e "s,kError\s*\(\s*\),qCCritical\($newcategoryname\),"


find -iname "*.cpp" -o -iname "*.h" | xargs perl -pi -e "s,#include \<QDebug\>,#include \"$debugnamefile.h\","
find -iname "*.cpp" -o -iname "*.h" | xargs perl -pi -e "s,#include \<qdebug.h\>,#include \"$debugnamefile.h\","
find -iname "*.cpp" -o -iname "*.h" | xargs perl -pi -e "s,#include \<kdebug.h\>,#include \"$debugnamefile.h\","
find -iname "*.cpp" -o -iname "*.h" | xargs perl -pi -e "s,#include \<KDebug\>,#include \"$debugnamefile.h\","


git add $debugnamefile.cpp $debugnamefile.h


# end
echo "1) Add $debugnamefile.cpp to CMakeLists.txt"
echo "2) Add #include <QDebug> + #include \"$debugnamefile.h\" to each file which was changed"
echo "3) Adjust name / email in $debugnamefile.cpp and $debugnamefile.h"
echo "4) Verify that it compiles :)"
echo "5) If you find bugs fix them please"

