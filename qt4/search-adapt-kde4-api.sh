#!/bin/sh
egrep -rl '(makeMainWidget|addPage|KWin::info|kuniqueapp.h|appStarted|kmdcodec.h)'  * | egrep -v '\.(svn|libs|o|moc|l[ao])|Makefile(.in)?|kopete' 
