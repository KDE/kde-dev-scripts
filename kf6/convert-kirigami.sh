#!/bin/bash
# SPDX-FileCopyrightText: 2022 Volker Krause <vkrause@kde.org>
# SPDX-License-Identifier: CC0-1.0

#
# Determine used namespaces
#
QQC2_NS=`cat $1 | grep '^import QtQuick.Controls 2' | grep ' as ' | perl -p -e 's/import QtQuick.Controls 2.\d{1,2} as (.*)/\1./'`


#
# Changes compatible with Qt 5.15
#

# platform dialogs
perl -p -i -e 's/^import QtQuick.Dialogs 1.\d/import Qt.labs.platform 1.1/' $1
perl -0777 -p -i -e 's/(MessageDialog \{[^\}]*?on(:?Apply|Discard)): /\1Clicked: /sg' $1
perl -0777 -p -i -e 's/(MessageDialog \{[^\}]*?)(standardButtons): /\1buttons: /sg' $1
perl -p -i -e 's/StandardButton\./MessageDialog./g' $1

# migrate from ApplicationWindow.overlay to Overlay.overlay
# (needs current QQC2 import version!)
perl -p -i -e "s/(?:\w+\.)?ApplicationWindow\.overlay/applicationWindow().${QQC2_NS}Overlay.overlay/g" $1
perl -p -i -e "s/applicationWindow\(\)\.overlay/applicationWindow().${QQC2_NS}Overlay.overlay/g" $1


#
# Changes incompatible with Qt 5
#

# icon property changes
perl -0777 -p -i -e 's/(BasicListItem \{[^\}]*?)(icon): /\1icon.name: /sg' $1

# QtGraphicalEffects
# rename import
perl -p -i -e 's/import QtGraphicalEffects 1\.\d+/import Qt5Compat.GraphicalEffects 6.0/' $1
# remove obsolete samples: property
perl -0777 -p -i -e 's/((?:DropShadow|GaussianBlur) \{[^\}].*?)\n\s*samples: .*?\n/\1\n/sg' $1
