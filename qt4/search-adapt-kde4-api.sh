#!/bin/sh
egrep -rl '(b[0-8]Pressed|KInputDialog::getText|makeMainWidget|plainPage|addPage|KWin::info|kuniqueapp.h|appStarted|kmdcodec.h|klargefile.h|kstddirs.h|kapp.hKMainWindow::memberList|KMainWindow::memberList()|getPid)'  * | egrep -v '\.(svn|libs|o|moc|l[ao])|Makefile(.in)?' 
