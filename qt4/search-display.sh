#!/bin/sh

# search-display.sh  | xargs convert-display.pl

egrep -rl '(qt_xdisplay|qt_xrootwin)'  * | egrep -v '\.(svn|libs|o|moc|l[ao])|Makefile(.in)?'
