#!/bin/sh
egrep -rl '(b[0-8]Pressed|KInputDialog::text|makeMainWidget|addPage|KWin::info|kuniqueapp.h|appStarted|kmdcodec.h|klargefile.h)'  * | egrep -v '\.(svn|libs|o|moc|l[ao])|Makefile(.in)?' 
