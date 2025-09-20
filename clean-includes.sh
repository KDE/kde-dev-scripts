#!/bin/sh

# Laurent Montel <montel@kde.org> (2014-2025)
# cd <directory> ; kde-dev-scripts/clean-includes.sh


remove_include() {
	echo "$file" |xargs perl -pi -e "s!#include <$originalStr>\n!!"
	echo "$file" |xargs perl -pi -e "s!#include \"$originalStr\"\n!!"
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
			# echo "search in file $file";
			include=$(grep "#include" "$file");
			new=$(echo "$include" |perl -pi -e 's!#include !!');

			new=$(echo "$new" |perl -pi -e 's!\\"!!g');
			new=$(echo "$new" |perl -pi -e 's!<!!g');
			new=$(echo "$new" |perl -pi -e 's!>!!g');

			newname=$(echo "$new" |perl -pi -e 's!.h!!');


       echo "before go : $new";
       for i in $new ;
       do
			originalStr=$i;
			newname=$(echo "$i" | sed 's,.*/,,');

	       newname=$(echo "$newname" |perl -pi -e 's!.h!!');
           if test 1 ; then
		       echo "egal $i";
		       number=$(grep -c "$newname" "$file");
		       echo "number $number";
		       if test "$number" = 1 ; then
			       #echo "$file" |xargs perl -pi -e "s!#include <$newname>\n!!";
			       # echo "number = 1 $newname class $file";
			       firstCar=$(echo "$i" | perl -pi -e "s/^(.)(.*)(.)$/\1/");
			       echo "first car : $firstCar";
			       if test "$firstCar" = "Q" || test "$firstCar" = "K" ; then
				       case $newname in
				           QtMath)
						       number=$(grep -c "qPow"  "$file");
						       echo "qPow $number";
						       if test "$number" = 0 ; then
							       remove_include;
						       fi
						       ;;

				           QQmlContext)
						       number=$(grep -c "rootContext()"  "$file");
						       echo "rootContext() $number";
						       if test "$number" = 0 ; then
							       remove_include;
						       fi
						       ;;
						   QtQuickTest)
						       number=$(grep -c "QUICK_TEST_MAIN"  "$file");
						       echo "QUICK_TEST_MAIN $number";
						       if test "$number" = 0 ; then
							       remove_include;
						       fi
						       ;;
				           QtConcurrentRun)
						       number=$(grep -c "QtConcurrent::run"  "$file");
						       echo "QtConcurrentRun $number";
						       if test "$number" = 0 ; then
							       remove_include;
						       fi
						       ;;

				           QObject)
						       number=$(grep -c "QT_VERSION_CHECK"  "$file");
						       echo "QObject $number";
						       if test "$number" = 0 ; then
							       number=$(grep -Ec "Q_GADGET"  "$file");
                                                               if test "$number" = 0 ; then
                                                                       remove_include;
                                                               fi
						       fi
						       ;;

				           KJobUiDelegate)
						       number=$(grep -c "uiDelegate()"  "$file");
						       echo "KJobUiDelegate $number";
						       if test "$number" = 0 ; then
							       remove_include;
						       fi
						       ;;
						   QScopeGuard)
						       number=$(grep -c "qScopeGuard"  "$file");
						       echo "QScopeGuard $number";
						       if test "$number" = 0 ; then
							       remove_include;
						       fi
						       ;;

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
							       number=$(grep -c qGuiApp  "$file");
							       if test "$number" = 0 ; then
									remove_include;
								fi
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
                                              QSqlError)
                                                       number=$(grep -c "lastError()"  "$file");
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
						           number=$(grep  -c "qAddPostRoutine"  "$file");
						           if test "$number" = 0 ; then
							           remove_include;
							       fi
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
						           number=$(grep  -c "screen()" "$file");
						           if test "$number" = 0 ; then
							         remove_include;
								   fi
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
					       QWindow)
						       number=$(grep -Ec "windowHandle()" "$file");
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
					       echo "remove it !!!!!";
						       remove_include;
						       ;;
				       esac
			       elif [ "$number" -gt 1 ]; then
				       #file_header;
				       echo "tot";
	     fi
	  fi
	  fi
  done;
   fi
done;
}

path=$PWD;
test_include
git diff .
