#!/bin/bash
# SPDX-FileCopyrightText: 2022 Volker Krause <vkrause@kde.org>
# SPDX-License-Identifier: CC0-1.0

# icon property changes
perl -0777 -p -i -e 's/(BasicListItem \{[^\}]*?)(icon): /\1icon.name: /sg' $1

# QtGraphicalEffects
# rename import
perl -p -i -e 's/import QtGraphicalEffects 1\.\d+/import Qt5Compat.GraphicalEffects 6.0/' $1
# remove obsolete samples: property
perl -0777 -p -i -e 's/((?:DropShadow|GaussianBlur) \{[^\}].*?)\n\s*samples: .*?\n/\1\n/sg' $1
