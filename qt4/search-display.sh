#!/bin/sh
egrep -rl '(qt_xdisplay|qt_xrootwin)'  * | egrep -v '\.(svn|libs|o|moc|l[ao])|Makefile(.in)?'
