#!/bin/sh

# Laurent Montel <montel@kde.org> (2014-2022)
# cd <directory> ; kde-dev-scripts/kf5/clean-includes.sh


remove_include() {
    echo "$file" |xargs perl -pi -e "s!#include <$newname>\n!!"
    echo "$file" |xargs perl -pi -e "s!#include \"$newname\"\n!!"
}

file_header() {
    filewithoutext=$(echo "$file" |perl -pi -e 's!\.h!!');
    if test "$filewithoutext" != "$file" ; then
       echo "$file is a header file";
       existInclude=$(grep -c "$newname" "$file"|grep "#include");
       echo "$newname : $existInclude";
       if test "$existInclude" != 0 ; then
           newNameClass=$(grep -c "$newname\s*\*"  "$file");
           echo "number $number ";
           echo "newNameClass $newNameClass";
           number=$(expr "$number" - 1);
           if test "$number" = "$newNameClass"; then
              remove_include;
              classNumber=$(grep -c "class"  "$file");
              if test "$classNumber" != 0 ; then
                 perl -pi  -e "s!class!class $newname;\nclass!g" "$file";
                 perl -pi -e "s!#include!#include <$newname>\n#include!g" "$filewithoutext".cpp;
              else
                 perl -pi -e "s!#include!class $newname;\n#include!g" "$file";
                 perl -pi -e "s!#include!#include <$newname>\n#include!g" "$filewithoutext".cpp;
              fi
           fi
       fi
    fi
}

test_include() {
for file in $path/* ;
do
   if [[ $file == *"build"* ]]; then
     continue;
   fi
   #echo $file;
   if test -d "$file" ; then
          echo "Check include into directory : $file";
          path=$file;
          test_include;
   else
       #echo "search in file $file";
       include=$(grep "#include" "$file");
       #echo "$include ";
       new=$(echo "$include" |perl -pi -e 's!#include !!');
       new=$(echo "$new" |perl -pi -e 's!\\"!!g');
       new=$(echo "$new" |perl -pi -e 's!<!!g');
       new=$(echo "$new" |perl -pi -e 's!>!!g');
       new=$(echo "$new" |perl -pi -e 's!QtCore/!!g');
       new=$(echo "$new" |perl -pi -e 's!QtGui/!!g');
       new=$(echo "$new" |perl -pi -e 's!Qt3Support/!!g');
       new=$(echo "$new" |perl -pi -e 's!QtDBus/!!g');
       new=$(echo "$new" |perl -pi -e 's!KDE/!!g');
       new=$(echo "$new" |perl -pi -e 's!QtTest/!!g');
       new=$(echo "$new" |perl -pi -e 's!QtSvg/!!g');
       new=$(echo "$new" |perl -pi -e 's!QtXml/!!g');
       new=$(echo "$new" |perl -pi -e 's!Plasma/!!g');
       new=$(echo "$new" |perl -pi -e 's!KNS/!!g');
       new=$(echo "$new" |perl -pi -e 's!KIO/!!g');
       new=$(echo "$new" |perl -pi -e 's!KABC/!!g');
       new=$(echo "$new" |perl -pi -e 's!KMime/!!g');
       new=$(echo "$new" |perl -pi -e 's!KMBox/!!g');
       new=$(echo "$new" |perl -pi -e 's!KParts/!!g');
       new=$(echo "$new" |perl -pi -e 's!KNS3/!!g');
       new=$(echo "$new" |perl -pi -e 's!KNSCore/!!g');
       new=$(echo "$new" |perl -pi -e 's!KPIMIdentities/!!g');
       new=$(echo "$new" |perl -pi -e 's!KCalCore/!!g');
       new=$(echo "$new" |perl -pi -e 's!KPIMUtils/!!g');
       new=$(echo "$new" |perl -pi -e 's!KCalUtils/!!g');
       new=$(echo "$new" |perl -pi -e 's!KontactInterface/!!g');
       new=$(echo "$new" |perl -pi -e 's!KHolidays/!!g');
       new=$(echo "$new" |perl -pi -e 's!KWallet/!!g');
       new=$(echo "$new" |perl -pi -e 's!QtNetwork/!!g');
       new=$(echo "$new" |perl -pi -e 's!KJobWidgets/!!g');
       new=$(echo "$new" |perl -pi -e 's!KPIMTextEdit/!!g');
       new=$(echo "$new" |perl -pi -e 's!QtSql/!!g');
       new=$(echo "$new" |perl -pi -e 's!QtWidgets/!!g');
       new=$(echo "$new" |perl -pi -e 's!QtPrintSupport/!!g');
       new=$(echo "$new" |perl -pi -e 's!QtQuick/!!g');
       new=$(echo "$new" |perl -pi -e 's!QtDBus/!!g');
       new=$(echo "$new" |perl -pi -e 's!QtDeclarative/!!g');
       new=$(echo "$new" |perl -pi -e 's!QtDesigner/!!g');
       new=$(echo "$new" |perl -pi -e 's!QtQml/!!g');
       new=$(echo "$new" |perl -pi -e 's!QtMultimedia/!!g');
       new=$(echo "$new" |perl -pi -e 's!QtConcurrent/!!g');
       new=$(echo "$new" |perl -pi -e 's!QtQuickWidgets/!!g');
       new=$(echo "$new" |perl -pi -e 's!QtXmlPatterns/!!g');
       new=$(echo "$new" |perl -pi -e 's!KContacts/!!g');
       new=$(echo "$new" |perl -pi -e 's!KIdentityManagement/!!g');
       new=$(echo "$new" |perl -pi -e 's!KAddressBookImportExport/!!g');
       new=$(echo "$new" |perl -pi -e 's!KaddressbookGrantlee/!!g');
       new=$(echo "$new" |perl -pi -e 's!QGpgME/!!g');
       new=$(echo "$new" |perl -pi -e 's!KDAV/!!g');
       new=$(echo "$new" |perl -pi -e 's!Qt3DCore/!!g');
       new=$(echo "$new" |perl -pi -e 's!Qt3DAnimation/!!g');
       new=$(echo "$new" |perl -pi -e 's!Qt3DRender/!!g');
       new=$(echo "$new" |perl -pi -e 's!KSieveUi/!!g');
       new=$(echo "$new" |perl -pi -e 's!KTNEF/!!g');
       new=$(echo "$new" |perl -pi -e 's!KCodecs/!!g');
       new=$(echo "$new" |perl -pi -e 's!KSMTP/!!g');
       new=$(echo "$new" |perl -pi -e 's!KGAPI/!!g');
       new=$(echo "$new" |perl -pi -e 's!KItinerary/!!g');
       new=$(echo "$new" |perl -pi -e 's!KRunner/!!g');
       new=$(echo "$new" |perl -pi -e 's!KIMAP/!!g');
       new=$(echo "$new" |perl -pi -e 's!KLDAP/!!g');
       new=$(echo "$new" |perl -pi -e 's!KCoreAddons/!!g');
       new=$(echo "$new" |perl -pi -e 's!KdepimDBusInterfaces/!!g');
       new=$(echo "$new" |perl -pi -e 's!KScreen/!!g');
       new=$(echo "$new" |perl -pi -e 's!KQuickAddons/!!g');
       new=$(echo "$new" |perl -pi -e 's!KIPI/!!g');
       new=$(echo "$new" |perl -pi -e 's!KIOWidgets/!!g');
       new=$(echo "$new" |perl -pi -e 's!KActivities/Stats/!!g');
       new=$(echo "$new" |perl -pi -e 's!KI18n/!!g');
       new=$(echo "$new" |perl -pi -e 's!KTextEditor/!!g');
       new=$(echo "$new" |perl -pi -e 's!KPackage/!!g');
       new=$(echo "$new" |perl -pi -e 's!KPeople/!!g');
       new=$(echo "$new" |perl -pi -e 's!KActivities/!!g');
       new=$(echo "$new" |perl -pi -e 's!KFileMetaData/!!g');
       new=$(echo "$new" |perl -pi -e 's!KCalendarCore/!!g');
       new=$(echo "$new" |perl -pi -e 's!KDeclarative/!!g');
       new=$(echo "$new" |perl -pi -e 's!KUserFeedback/!!g');
       new=$(echo "$new" |perl -pi -e 's!KAlarmCal/!!g');
       newname=$(echo "$new" |perl -pi -e 's!.h!!');

       #echo "before go : $new";
       for i in $new ;
       do
          #echo $i;
          newname=$(echo "$i" |perl -pi -e 's!.h!!');

          if test "$newname" = "$i" ; then
             #echo "egal $i";
             number=$(grep -c "$i" "$file");
             #echo "number $number";
             if test "$number" = 1 ; then
                #echo "$file" |xargs perl -pi -e "s!#include <$newname>\n!!";
                echo "number = 1 $newname class $file";
                firstCar=$(echo "$i" | perl -pi -e "s/^(.)(.*)(.)$/\1/");
		echo "first car : $firstCar";
		if test "$firstCar" = "Q" || test "$firstCar" = "K" ; then
                        case $newname in
                        QtDBus)
                           number=$(grep -c QDBusInterface  "$file");
                           echo "QDBusInterface $number";
                           if test "$number" = 0 ; then
                              remove_include;
                           fi
                        ;;
                        QApplication)
                           number=$(grep -c qApp  "$file");
                           echo "qApp $number";
                           if test "$number" = 0 ; then
                                remove_include;
                           fi
                        ;;
			QGuiApplication)
                           number=$(grep -c qGuiApp  "$file");
                           echo "qApp $number";
                           if test "$number" = 0 ; then
                                remove_include;
                           fi
                        ;;
                        QDesktopWidget)
                           number=$(grep -c "Application::desktop"  "$file");
                           if test "$number" = 0 ; then
				number=$(grep -Ec "qApp->desktop()|kApp->desktop()"  "$file");
                                if test "$number" = 0 ; then
                                   remove_include;
                                fi
                           fi
                        ;;
         		QDBusArgument)
                           number=$(grep -c "qdbus_cast"  "$file");
                           if test "$number" = 0 ; then
                               remove_include;
                           fi
                        ;;
			QDBusPendingCall)
                           number=$(grep -c "QDBusConnection::sessionBus().asyncCall"  "$file");
                           if test "$number" = 0 ; then
                               remove_include;
                           fi
                        ;;

            		QDBusMetaType)
                           number=$(grep -c "qDBusRegisterMetaType"  "$file");
                           if test "$number" = 0 ; then
                               remove_include;
                           fi
                        ;;

                        QScrollBar)
                           number=$(grep -c "verticalScrollBar()"  "$file");
                           if test "$number" = 0 ; then
                                number=$(grep -c  "horizontalScrollBar()"  "$file");
                                if test "$number" = 0 ; then
                                   remove_include;
                                fi
                           fi
                        ;;
                        QClipboard)
                           number=$(grep -Ec "Application::clipboard|clipboard()"  "$file");
                           if test "$number" = 0 ; then
                                remove_include;
                           fi
                        ;;
                        QMetaType)
                           number=$(grep -c "Q_DECLARE_METATYPE"  "$file");
                           if test "$number" = 0 ; then
                                remove_include;
                           fi
                        ;;
                        KLocale)
                           number=$(grep  -c "i18n"  "$file");
                           if test "$number" = 0 ; then
                                number=$(grep  -c "locale()"  "$file");
                                if test "$number" = 0 ; then
                                   remove_include;
                                fi
                           fi
                        ;;
                        KActionCollection)
                           number=$(grep  -c "actionCollection"  "$file");
                           if test "$number" = 0 ; then
                                remove_include;
                           fi
                        ;;
                        KApplication)
                           number=$(grep  -c '\bkapp\b'  "$file");
                           echo "qApp $number";
                           if test "$number" = 0 ; then
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
                           number=$(grep  -c "factory()"  "$file");
                           if test "$number" = 0 ; then
                                number=$(grep  -c "guiFactory()"  "$file");
                                if test "$number" = 0 ; then
                                     remove_include;
                                fi
                           fi
                        ;;
                        KGenericFactory)
                           number=$(grep  -c "K_EXPORT_PLUGIN"  "$file");
                           if test "$number" = 0 ; then
                                remove_include;
                           fi
                        ;;
                        QCoreApplication)
                           number=$(grep  -c "qApp"  "$file");
                           if test "$number" = 0 ; then
                                remove_include;
                           fi
                        ;;
                        QTreeView)
                        ;;

                        QDateTime)
                           number=$(grep  -c "QDate::currentDate"  "$file");
                           if test "$number" = 0 ; then
                                remove_include;
                           fi
                        ;;
                        KLineEdit)
                           number=$(grep  -c "lineEdit()"  "$file");
                           if test "$number" = 0 ; then
                                remove_include;
                           fi
                        ;;
                        QDBusConnectionInterface)
                        ;;
                        KLocalizedString)
                           number=$(grep -E "i18n|I18N_NOOP"  "$file");
                           if test "$number" = 0 ; then
                                remove_include;
                           fi
                        ;;
                        QAbstractTextDocumentLayout)
	                ;;
                        QAbstractItemView)
                        ;;
                        QMetaEnum)
                           number=$(grep  -c "enumerator"  "$file");
                           if test "$number" = 0 ; then
                                remove_include;
                           fi
                        ;;

                        QLayout)
                           number=$(grep  -c "layout()"  "$file");
                           if test "$number" = 0 ; then
                                remove_include;
                           fi
                        ;;
                        QAction)
                           number=$(grep  -c "\baddAction\b"  "$file");
                           if test "$number" = 0 ; then
                                remove_include;
                           fi
                        ;;
                        QTextDocument)
                           number=$(grep  -c "Qt::escape"  "$file");
                           if test "$number" = 0 ; then
                                remove_include;
                           fi
                        ;;
                        QHeaderView)
                           number=$(grep  -c "header()"  "$file");
                           if test "$number" = 0 ; then
                                number=$(grep  -c "horizontalHeader()" "$file");
                                if test "$number" = 0 ; then
                                   number=$(grep  -c "verticalHeader()" "$file");
                                   if test "$number" = 0 ; then
                                      remove_include;
                                   fi
                               fi
                           fi
                        ;;
                        KRecentFilesAction)
                           number=$(grep  -c "KStandardAction::openRecent" "$file");
                           if test "$number" = 0 ; then
                              remove_include;
                           fi
                        ;;
                        KStatusBar)
                           number=$(grep  -c "statusBar()" "$file");
                           if test "$number" = 0 ; then
                              remove_include;
                           fi
                        ;;
                        QStatusBar)
                           number=$(grep  -c "statusBar()" "$file");
                           if test "$number" = 0 ; then
                              remove_include;
                           fi
                        ;;
                        KGlobal)
                           number=$(grep  -c "K_GLOBAL_STATIC" "$file");
                           if test "$number" = 0 ; then
                              remove_include;
                           fi
                        ;;
                        KStandardDirs)
                           number=$(grep  -c "KGlobal::dirs()->" "$file");
                           if test "$number" = 0 ; then
                              remove_include;
                           fi
                        ;;
                        QTabBar)
                           number=$(grep  -c "tabBar()" "$file");
                           if test "$number" = 0 ; then
                              remove_include;
                           fi
                        ;;
                        KCompletionBox)
                           number=$(grep  -c "completionBox()" "$file");
                           if test "$number" = 0 ; then
                              remove_include;
                           fi
                        ;;

                        KIconLoader)
                           number=$(grep -c  "SmallIcon" "$file");
                           if test "$number" = 0 ; then
                              remove_include;
                           fi
                        ;;

                        KZip)
                           number=$(grep  -c "archive()" "$file");
                           if test "$number" = 0 ; then
                              remove_include;
                           fi
                        ;;
                        QDebug)
                           number=$(grep -Ec "qDebug|qWarning|qCritical"  "$file");
                           if test "$number" = 0 ; then
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
                        QtDebug)
                           number=$(grep -Ec "qDebug|qWarning|qCritical"  "$file");
                           if test "$number" = 0 ; then
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
                           number=$(grep -Ec "Q_DECLARE_LOGGING_CATEGORY|Q_LOGGING_CATEGORY" "$file");
                           if test "$number" = 0 ; then
                              number=$(grep -Ec "qDebug|qWarning|qCritical"  "$file");
                              if test "$number" = 0 ; then
                                  remove_include;
                              fi
                           fi
                        ;;
                        QScreen)
                           number=$(grep  -c "physicalDotsPerInch()" "$file");
                           if test "$number" = 0 ; then
                              remove_include;
                           fi
                        ;;
			QMimeData)
                           number=$(grep  -c "mimeData()" "$file");
                           if test "$number" = 0 ; then
                              remove_include;
                           fi
                        ;;
                        KToolBar)
                           number=$(grep  -c "toolBar()" "$file");
                           if test "$number" = 0 ; then
                              remove_include;
                           fi
                        ;;
                        QPixmap)
                           number=$(grep  -c "SmallIcon" "$file");
                           if test "$number" = 0 ; then
                              remove_include;
                           fi
                        ;;
                        QHostAddress)
                        ;;
                        QMenuBar)
                           number=$(grep  -c "menuBar()" "$file");
                           if test "$number" = 0 ; then
                              remove_include;
                           fi
                        ;;
                        QTest)
                          number=$(grep -Ec "QCOMPARE|QVERIFY|QTEST_GUILESS_MAIN|QTEST" "$file");
                           if test "$number" = 0 ; then
                              remove_include;
                           fi
                        ;;
                        QtTestGui)
                          number=$(grep -Ec "QCOMPARE|QVERIFY|QTEST_GUILESS_MAIN|QTEST" "$file");
                           if test "$number" = 0 ; then
                              remove_include;
                           fi
                        ;;
                        QtTestWidgets)
                          number=$(grep -Ec "QCOMPARE|QVERIFY|QTEST_GUILESS_MAIN|QTEST" "$file");
                           if test "$number" = 0 ; then
                              remove_include;
                           fi
                        ;;
                        QtTest)
                           number=$(grep -Ec "QCOMPARE|QVERIFY|QTEST_GUILESS_MAIN|QTEST" "$file");
                           if test "$number" = 0 ; then
                              remove_include;
                           fi
                        ;;
                        *)
                           remove_include;
                        ;;
                        esac
		fi
	     elif [ "$number" -gt 1 ]; then
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
