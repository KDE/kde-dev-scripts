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

#convert debug to new qCDebug/qCWarning/qCCritical

find -iname "*.cpp" -o -iname "*.h" | xargs sed -ri \
		-e "s|^(\s*#include\s+)<KDebug>|\1<QDebug>\n\1\"${debugnamefile}.h\"|" \
		-e "s,k(Debug|Warning)\s*\(\s*$oldcategorynumber\s*\),qC\1\($newcategoryname\)," \
		-e "s,k(Fatal|Error)\s*\(\s*$oldcategorynumber\s*\),qCCritical\($newcategoryname\),"


# end
echo "1) add ecm_qt_declare_logging_category(<file>_SRCS HEADER $debugnamefile.h IDENTIFIER $newcategoryname CATEGORY_NAME $newlogname) to CMakeLists.txt";
echo "2) #include \"${debugnamefile}.h\" to each file which was changed and where it is missing"
echo "3) Verify that it compiles :)"
echo "4) If you find bugs fix them please"

