#!/bin/sh
# SPDX-FileCopyrightText: 2023 Laurent Montel <montel@kde.org>
# SPDX-License-Identifier: CC0-1.0
#
list='ConfigCore
    KIOCore
    XmlGui
    I18n
    ConfigWidgets
    Codecs
    Purpose
    PurposeWidgets
    NewStuffWidgets
    NewStuff
    ItemViews
    KIOWidgets
    Crash
    TextEditor
    DocTools
    DBusAddons
    Archive
    SyntaxHighlighting
    IconThemes
    KIO
    WidgetsAddons
    TextWidgets
    Completion
    IconThemes
    Contacts
    CalendarCore
    ItemModels
    Service
    Holidays
    GuiAddons
    CoreAddons
    GlobalAccel
    DNSSD
    KCMUtils
    Notifications
    NotifyConfig
    Parts
    WindowSystem
    Config
    JobWidgets
    Prison
    ConfigGui
    KIOFileWidgets
    SonnetUi
    Sonnet
    SonnetCore
    Bookmarks
    Syndication
    KIOGui
    ItemModels
    Wallet
    IdleTime
    ActivitiesStats
    Solid
    FileMetaData
    Baloo
    Plotting
    UnitConversion
    Kirigami2
    Package
    AuthCore
    QuickAddons
    Plasma
    Wayland';
for i in $list ;
do
     echo " **** $i******";
     git grep -l "KF5::$i" | xargs perl -pi -e "s;\bKF5::$i\b;KF\\$\{KF_MAJOR_VERSION\}::$i;g"
     git grep -l "KF5$i"  | egrep -v Config.cmake.in | xargs perl -pi -e "s;\bKF5$i\b;KF\\$\{KF_MAJOR_VERSION\}$i;g"
     find -iname  *Config.cmake.in | xargs perl -pi -e "s;\bKF5$i\b;KF\\@KF_MAJOR_VERSION\\@$i;g"
     git grep -l "KF5$i_FOUND" | xargs perl -pi -e "s,KF5$i_FOUND,KF\\$\{KF_MAJOR_VERSION\}$i_FOUND,"
done
git grep -l "find_package(KF5 " | xargs perl -pi -e "s,find_package\\(KF5 ,find_package\\(KF\\$\{KF_MAJOR_VERSION\} ,"
git grep -l "Qt5.*_QCH" | xargs perl -pi -e 's,Qt5Gui_QCH,Qt\${QT_MAJOR_VERSION}Gui_QCH,'
git grep -l "Qt5.*_QCH" | xargs perl -pi -e 's,Qt5Core_QCH,Qt\${QT_MAJOR_VERSION}Core_QCH,'
git grep -l "Qt5.*_QCH" | xargs perl -pi -e 's,Qt5Widgets_QCH,Qt\${QT_MAJOR_VERSION}Widgets_QCH,'

perl -pi -e 's,include\(ECMDeprecationSettings\),include\(ECMDeprecationSettings\)\nif (QT_MAJOR_VERSION STREQUAL "6")\n    set(QT_REQUIRED_VERSION "6.4.0")\n    set(KF_MIN_VERSION "5.240.0")\n    set(KF_MAJOR_VERSION "6")\nelse()\n    set(KF_MAJOR_VERSION "5")\nendif(),' CMakeLists.txt
perl -pi -e "s;Dependencies:;Dependencies:\n\- \'on\': \[\'Linux/Qt6\', \'FreeBSD/Qt6\', \'Windows/Qt6\', \'Android/Qt6\'\]\n  'require':\n add \\@latest\-kf6\n;" .kde-ci.yml
perl -pi -e "s;\- \'on\': \[\'\@all\'\];\- \'on\': \[\'Linux/Qt5\', \'FreeBSD/Qt5\', \'Windows/Qt5\', \'Android/Qt5\'\];" .kde-ci.yml

# Allow to build against not deprecated Qt 6.4 method
perl -pi -e 's,QT 5.15.2,QT 6.4,' CMakeLists.txt

# Warn about we need to adapt .kde-ci.yml file. 
echo "Adapt $PWD/.kde-ci.yml"
#git di

