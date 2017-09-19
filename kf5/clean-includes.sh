#!/bin/sh

# Laurent Montel <montel@kde.org> (2014-2017)
# cd <directory> ; kde-dev-scripts/kf5/clean-includes.sh


remove_include() {
    echo "$file" |xargs perl -pi -e "s!#include <$newname>\n!!"
    echo "$file" |xargs perl -pi -e "s!#include \"$newname\"\n!!"
}

file_header() {
    filewithoutext=`echo "$file" |perl -pi -e 's!\.h!!'`;
    if test $filewithoutext != $file ; then
       echo "$file is a header file";
       existInclude=`grep $newname $file|grep "#include"|wc -l`;
       echo "$newname : $existInclude";
       if test $existInclude != 0 ; then 
           newNameClass=`grep "$newname\s*\*"  $file|wc -l`;
           echo "number $number ";
           echo "newNameClass $newNameClass";
           number=$(expr $number - 1);
           if test $number = $newNameClass; then
              remove_include;
              classNumber=`grep "class"  $file|wc -l`;
              if test $classNumber != 0 ; then
                 perl -pi  -e "s!class!class $newname;\nclass!g" $file;
                 perl -pi -e "s!#include!#include <$newname>\n#include!g" $filewithoutext.cpp;
              else
                 perl -pi -e "s!#include!class $newname;\n#include!g" $file;
                 perl -pi -e "s!#include!#include <$newname>\n#include!g" $filewithoutext.cpp;
              fi
           fi
       fi
    fi
}

test_include() {
for file in $path/* ;
do
   #echo $file;
   if test -d $file ; then
          echo "Check include into directory : $file";
          path=$file;
          test_include;
   else
       #echo "search in file $file";
       include=`grep "#include" $file`;
       #echo "$include ";
       new=`echo "$include" |perl -pi -e 's!#include !!'`;
       new=`echo "$new" |perl -pi -e 's!\\"!!g'`;
       new=`echo "$new" |perl -pi -e 's!<!!g'`;
       new=`echo "$new" |perl -pi -e 's!>!!g'`;
       new=`echo "$new" |perl -pi -e 's!QtCore/!!g'`;
       new=`echo "$new" |perl -pi -e 's!QtGui/!!g'`;
       new=`echo "$new" |perl -pi -e 's!Qt3Support/!!g'`;
       new=`echo "$new" |perl -pi -e 's!QtDBus/!!g'`;
       new=`echo "$new" |perl -pi -e 's!KDE/!!g'`;
       new=`echo "$new" |perl -pi -e 's!QtTest/!!g'`;
       new=`echo "$new" |perl -pi -e 's!QtSvg/!!g'`;
       new=`echo "$new" |perl -pi -e 's!QtXml/!!g'`;
       new=`echo "$new" |perl -pi -e 's!Plasma/!!g'`;
       new=`echo "$new" |perl -pi -e 's!KNS/!!g'`;
       new=`echo "$new" |perl -pi -e 's!KIO/!!g'`;
       new=`echo "$new" |perl -pi -e 's!KABC/!!g'`;
       new=`echo "$new" |perl -pi -e 's!KMime/!!g'`;
       new=`echo "$new" |perl -pi -e 's!KParts/!!g'`;
       new=`echo "$new" |perl -pi -e 's!KNS3/!!g'`;
       new=`echo "$new" |perl -pi -e 's!KPIMIdentities/!!g'`;
       new=`echo "$new" |perl -pi -e 's!KCalCore/!!g'`;
       new=`echo "$new" |perl -pi -e 's!KPIMUtils/!!g'`;
       new=`echo "$new" |perl -pi -e 's!KCalUtils/!!g'`;
       new=`echo "$new" |perl -pi -e 's!KontactInterface/!!g'`;
       new=`echo "$new" |perl -pi -e 's!KHolidays/!!g'`;
       new=`echo "$new" |perl -pi -e 's!KWallet/!!g'`;
       new=`echo "$new" |perl -pi -e 's!QtNetwork/!!g'`;
       new=`echo "$new" |perl -pi -e 's!KJobWidgets/!!g'`;
       new=`echo "$new" |perl -pi -e 's!KPIMTextEdit/!!g'`;
       new=`echo "$new" |perl -pi -e 's!QtSql/!!g'`;
       new=`echo "$new" |perl -pi -e 's!QtWidgets/!!g'`;
       new=`echo "$new" |perl -pi -e 's!QtPrintSupport/!!g'`;
       new=`echo "$new" |perl -pi -e 's!QtQuick/!!g'`;
       new=`echo "$new" |perl -pi -e 's!QtDBus/!!g'`;
       new=`echo "$new" |perl -pi -e 's!QtDeclarative/!!g'`;
       new=`echo "$new" |perl -pi -e 's!QtDesigner/!!g'`;
       new=`echo "$new" |perl -pi -e 's!QtQml/!!g'`;
       new=`echo "$new" |perl -pi -e 's!QtMultimedia/!!g'`;
       new=`echo "$new" |perl -pi -e 's!QtConcurrent/!!g'`;
       new=`echo "$new" |perl -pi -e 's!QtQuickWidgets/!!g'`;
       new=`echo "$new" |perl -pi -e 's!QtXmlPatterns/!!g'`;
       new=`echo "$new" |perl -pi -e 's!KContacts/!!g'`;
       new=`echo "$new" |perl -pi -e 's!KIdentityManagement/!!g'`;
       new=`echo "$new" |perl -pi -e 's!KAddressBookImportExport/!!g'`;
       new=`echo "$new" |perl -pi -e 's!KaddressbookGrantlee/!!g'`;
       new=`echo "$new" |perl -pi -e 's!QGpgME/!!g'`;
       new=`echo "$new" |perl -pi -e 's!KDAV/!!g'`;

       
       newname=`echo "$new" |perl -pi -e 's!.h!!'`;

       #echo "before go : $new";
       for i in $new ;
       do
          #echo $i;
          newname=`echo "$i" |perl -pi -e 's!.h!!'`;
   
          if test $newname = $i ; then
             #echo "egal $i";
             number=`grep $i $file|wc -l`;
             #echo "number $number";
             if test $number = 1 ; then
                #echo "$file" |xargs perl -pi -e "s!#include <$newname>\n!!";
                echo "number = 1 $newname class $file";
                firstCar=`echo "$i" | perl -pi -e "s/^(.)(.*)(.)$/\1/"`;
		echo "first car : $firstCar";
		if test $firstCar = "Q" || test $firstCar = "K" ; then
                        case $newname in
                        QtDBus)
                           number=`grep QDBusInterface  $file|wc -l`;
                           echo "QDBusInterface $number";
                           if test $number = 0 ; then
                              remove_include;
                           fi
                        ;;
                        QApplication)
                           number=`grep qApp  $file|wc -l`;
                           echo "qApp $number";
                           if test $number = 0 ; then
                                remove_include;
                           fi
                        ;;

                        QDesktopWidget)
                           number=`grep "Application::desktop"  $file|wc -l`;
                           if test $number = 0 ; then
				number=`egrep "qApp->desktop()|kApp->desktop()"  $file|wc -l`;
                                if test $number = 0 ; then
                                   remove_include;
                                fi
                           fi
                        ;;
                        QScrollBar)
                           number=`grep "verticalScrollBar()"  $file|wc -l`;
                           if test $number = 0 ; then
                                number=`grep "horizontalScrollBar()"  $file|wc -l`;
                                if test $number = 0 ; then
                                   remove_include;
                                fi
                           fi
                        ;;
                        QClipboard)
                           number=`egrep "Application::clipboard|clipboard()"  $file|wc -l`;
                           if test $number = 0 ; then
                                remove_include;
                           fi
                        ;;
                        QMetaType)
                           number=`grep "Q_DECLARE_METATYPE"  $file|wc -l`;
                           if test $number = 0 ; then
                                remove_include;
                           fi
                        ;;
                        KLocale)
                           number=`grep "i18n"  $file|wc -l`;
                           if test $number = 0 ; then
                                number=`grep "locale()"  $file|wc -l`;
                                if test $number = 0 ; then
                                   remove_include;
                                fi
                           fi
                        ;;
                        KActionCollection)
                           number=`grep "actionCollection"  $file|wc -l`;
                           if test $number = 0 ; then
                                remove_include;
                           fi
                        ;;
                        KApplication)
                           number=`grep '\bkapp\b'  $file|wc -l`;
                           echo "qApp $number";
                           if test $number = 0 ; then
                                remove_include;
                           fi
                        ;;			
                        KDebug)
                        ;;
                        KPluginFactory)
                        ;;
                        KPluginLoader)
                        ;;
                        KXMLGUIFactory)
                           number=`grep "factory()"  $file|wc -l`;
                           if test $number = 0 ; then
                                remove_include;
                           fi
                        ;;
                        KGenericFactory)
                           number=`grep "K_EXPORT_PLUGIN"  $file|wc -l`;
                           if test $number = 0 ; then
                                remove_include;
                           fi
                        ;;
                        QCoreApplication)
                           number=`grep "qApp"  $file|wc -l`;
                           if test $number = 0 ; then
                                remove_include;
                           fi
                        ;;
                        QTreeView)
                        ;;
                        
                        KToolBar)
                           number=`grep "toolbar()"  $file|wc -l`;
                           if test $number = 0 ; then
                                remove_include;
                           fi
                        ;;
                        QDateTime)
                           number=`grep "QDate::currentDate"  $file|wc -l`;
                           if test $number = 0 ; then
                                remove_include;
                           fi
                        ;;
                        KLineEdit)
                           number=`grep "lineEdit()"  $file|wc -l`;
                           if test $number = 0 ; then
                                remove_include;
                           fi
                        ;;
                        QDBusConnectionInterface)
                        ;;
                        KLocalizedString)
                           number=`egrep "i18n|I18N_NOOP"  $file|wc -l`;
                           if test $number = 0 ; then
                                remove_include;
                           fi
                        ;;
                        QAbstractTextDocumentLayout)
	                ;;
                        QAbstractItemView)
                        ;;
                        QMetaEnum)
                           number=`grep "enumerator"  $file|wc -l`;
                           if test $number = 0 ; then
                                remove_include;
                           fi
                        ;;
 
                        QLayout)
                           number=`grep "layout()"  $file|wc -l`;
                           if test $number = 0 ; then
                                remove_include;
                           fi
                        ;;
                        QAction)
                           number=`grep "\baddAction\b"  $file|wc -l`;
                           if test $number = 0 ; then
                                remove_include;
                           fi
                        ;;
                        QTextDocument)
                           number=`grep "Qt::escape"  $file|wc -l`;
                           if test $number = 0 ; then
                                remove_include;
                           fi
                        ;;
                        QHeaderView)
                           number=`grep "header()"  $file|wc -l`;
                           if test $number = 0 ; then
                                number=`grep "horizontalHeader()" $file|wc -l`;
                                if test $number = 0 ; then
                                   number=`grep "verticalHeader()" $file|wc -l`;
                                   if test $number = 0 ; then
                                      remove_include;
                                   fi
                               fi
                           fi
                        ;;
                        KRecentFilesAction)
                           number=`grep "KStandardAction::openRecent" $file|wc -l`;
                           if test $number = 0 ; then
                              remove_include;
                           fi
                        ;;
                        KStatusBar)
                           number=`grep "statusBar()" $file|wc -l`;
                           if test $number = 0 ; then
                              remove_include;
                           fi
                        ;;
                        QStatusBar)
                           number=`grep "statusBar()" $file|wc -l`;
                           if test $number = 0 ; then
                              remove_include;
                           fi
                        ;;
                        KGlobal)
                           number=`grep "K_GLOBAL_STATIC" $file|wc -l`;
                           if test $number = 0 ; then
                              remove_include;
                           fi
                        ;;
                        KStandardDirs)
                           number=`grep "KGlobal::dirs()->" $file|wc -l`;
                           if test $number = 0 ; then
                              remove_include;
                           fi
                        ;;
                        QTabBar)
                           number=`grep "tabBar()" $file|wc -l`;
                           if test $number = 0 ; then
                              remove_include;
                           fi
                        ;;
                        KCompletionBox)
                           number=`grep "completionBox()" $file|wc -l`;
                           if test $number = 0 ; then
                              remove_include;
                           fi
                        ;;

                        KIconLoader)
                           number=`grep "SmallIcon" $file|wc -l`;
                           if test $number = 0 ; then
                              remove_include;
                           fi
                        ;;

                        KZip)
                           number=`grep "archive()" $file|wc -l`;
                           if test $number = 0 ; then
                              remove_include;
                           fi
                        ;;
                        QDebug)
                           number=`egrep "qDebug|qWarning|qCritical"  $file|wc -l`;
                           if test $number = 0 ; then
                               remove_include;
                           fi
                           #number=`grep "qDebug"  $file|wc -l`;
                           #if test $number = 0 ; then
                           #     number=`grep "qWarning"  $file|wc -l`;
                           #     if test $number = 0 ; then
                           #        remove_include;
                           #     fi
                           #fi
                        ;;
                        QLoggingCategory)
                           number=`grep "Q_DECLARE_LOGGING_CATEGORY" $file|wc -l`;
                           if test $number = 0 ; then
                              number=`egrep "qDebug|qWarning|qCritical"  $file|wc -l`;
                              if test $number = 0 ; then
                                  remove_include;
                              fi
                           fi
                        ;;
                        QScreen)
                           number=`grep "physicalDotsPerInch()" $file|wc -l`;
                           if test $number = 0 ; then
                              remove_include;
                           fi
                        ;;
                        KToolBar)
                           number=`grep "toolBar()" $file|wc -l`;
                           if test $number = 0 ; then
                              remove_include;
                           fi
                        ;;
                        QPixmap)
                           number=`grep "SmallIcon" $file|wc -l`;
                           if test $number = 0 ; then
                              remove_include;
                           fi
                        ;;
                        QHostAddress)
                        ;;
                        QMenuBar)
                           number=`grep "menuBar()" $file|wc -l`;
                           if test $number = 0 ; then
                              remove_include;
                           fi
                        ;;
                        QTest)
                          number=`egrep "QCOMPARE|QVERIFY" $file|wc -l`;
                           if test $number = 0 ; then
                              remove_include;
                           fi
                        ;;

                        QtTest)
                           number=`egrep "QCOMPARE|QVERIFY" $file|wc -l`;
                           if test $number = 0 ; then
                              remove_include;
                           fi
                        ;;
                        *)
                           remove_include; 
                        ;;
                        esac
		fi
	     elif test $number > 1 ; then
		#file_header;
                echo "tot";
             fi
          fi
       done;
   fi
done;
}

path=$PWD;
test_include
git diff .
