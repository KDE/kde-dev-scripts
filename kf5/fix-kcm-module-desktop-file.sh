#!/bin/sh
find -iname "*.desktop"|xargs grep kcmshell4 -l | xargs perl -pi -e 's/Exec=kcmshell4/Exec=kcmshell5/'
