#!/bin/sh

if test $# != 3; then
  echo "Usage : $0 <new log name> <new category name> <debug name file>"
  echo "example : convert-qdebug-to-qcdebug.sh org.kde.smtp SMTP_LOG smtp_debug"
  exit 0
fi


newlogname="$1"
newcategoryname="$2"
debugnamefile="$3"

upname=`echo $debugnamefile | tr 'a-z' 'A-Z'`

#convert debug to new qCDebug/qCWarning/qCCritical

find -iname "*.cpp" -o -iname "*.h" | xargs perl -pi -e "s,qDebug\s*\(\s*\),qCDebug\($newcategoryname\),"
find -iname "*.cpp" -o -iname "*.h" | xargs perl -pi -e "s,qWarning\s*\(\s*\),qCWarning\($newcategoryname\),"
find -iname "*.cpp" -o -iname "*.h" | xargs perl -pi -e "s,qFatal\s*\(\s*\),qCCritical\($newcategoryname\),"
find -iname "*.cpp" -o -iname "*.h" | xargs perl -pi -e "s,qError\s*\(\s*\),qCCritical\($newcategoryname\),"
find -iname "*.cpp" -o -iname "*.h" | xargs perl -pi -e "s,qCritical\s*\(\s*\),qCCritical\($newcategoryname\),"
find -iname "*.cpp" -o -iname "*.h" | xargs perl -pi -e "s,\bkDebug\s*\(\s*\),qCDebug\($newcategoryname\),"
find -iname "*.cpp" -o -iname "*.h" | xargs perl -pi -e "s,\bkWarning\s*\(\s*\),qCWarning\($newcategoryname\),"
find -iname "*.cpp" -o -iname "*.h" | xargs perl -pi -e "s,\bkFatal\s*\(\s*\),qCCritical\($newcategoryname\),"
find -iname "*.cpp" -o -iname "*.h" | xargs perl -pi -e "s,\bkError\s*\(\s*\),qCCritical\($newcategoryname\),"


find -iname "*.cpp" -o -iname "*.h" | xargs perl -pi -e "s,#include \<QDebug\>,#include \"$debugnamefile.h\","
find -iname "*.cpp" -o -iname "*.h" | xargs perl -pi -e "s,#include \<qdebug.h\>,#include \"$debugnamefile.h\","
find -iname "*.cpp" -o -iname "*.h" | xargs perl -pi -e "s,#include \<kdebug.h\>,#include \"$debugnamefile.h\","
find -iname "*.cpp" -o -iname "*.h" | xargs perl -pi -e "s,#include \<KDebug\>,#include \"$debugnamefile.h\","

find -iname "*.cpp" -o -iname "*.h" | xargs perl -pi -e "s/k_funcinfo/Q_FUNC_INFO/;"


# end
echo "1) ecm_qt_declare_logging_category(<file>_SRCS HEADER $debugnamefile.h IDENTIFIER $newcategoryname CATEGORY_NAME $newlogname) to CMakeLists.txt"
echo "2) Add #include \"$debugnamefile.h\" to each file which was changed"
echo "3) Verify that it compiles :)"
echo "4) If you find bugs fix them please me"

