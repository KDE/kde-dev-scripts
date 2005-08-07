#!/bin/sh
egrep -rl '(makeMainWidget|addPage|KWin::info|kuniqueapp.h|appStarted|kmdcodec.h)'  * |grep -v "\.svn" |grep -v "\.libs" | grep -v "\.o" | grep -v Makefile | grep -v Makefile.in | grep -v "kopete" | grep -v "\.moc" | grep -v "\.lo" | grep -v "\.la"
