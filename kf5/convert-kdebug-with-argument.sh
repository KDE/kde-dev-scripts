#!/bin/sh

if test $# != 4; then
  echo "Usage : $0 <old category number> <new log name> <new category name> <debug name file>"
  echo "example : convert-kdebug-with-argument.sh 7112 log_smtp SMTP_LOG smtp_debug"
  exit 1
fi


oldcategorynumber="$1"
newlogname="$2"
newcategoryname="$3"
debugnamefile="$4"

upname=`echo $debugnamefile | tr 'a-z' 'A-Z'`

# create debug.h file
cat > $debugnamefile.h <<EOF
#ifndef ${upname}_H
#define ${upname}_H

#include <QLoggingCategory>
Q_DECLARE_LOGGING_CATEGORY($newcategoryname)

#endif 

EOF


#create debug.cpp file
cat > $debugnamefile.cpp <<EOF

#include "$debugnamefile.h"
Q_LOGGING_CATEGORY($newcategoryname, "$newlogname")


EOF

#convert debug to new qCDebug/qCWarning/qCCritical

find -iname "*.cpp" -o -iname "*.h" | xargs perl -pi -e "s,kDebug\s*\(\s*$oldcategorynumber\s*\),qCDebug\($newcategoryname\),"
find -iname "*.cpp" -o -iname "*.h" | xargs perl -pi -e "s,kWarning\s*\(\s*$oldcategorynumber\s*\),qCWarning\($newcategoryname\),"
find -iname "*.cpp" -o -iname "*.h" | xargs perl -pi -e "s,kFatal\s*\(\s*$oldcategorynumber\s*\),qCCritical\($newcategoryname\),"

# end
echo "1) Add $debugnamefile.cpp to CMakeLists.txt"
echo "2) Add $debugnamefile.cpp $debugnamefile.h to git"
echo "3) Add license to $debugnamefile.cpp $debugnamefile.h"
echo "4) Add #include <QDebug> + #include \"$debugnamefile.h\" to each file which was changed"
echo "5) Verify that it compiles :)"
echo "6) If you find bugs fix them please"

