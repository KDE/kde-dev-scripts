#!/bin/sh
egrep -rl '(kaccelmanager.h|KStringHandler::matchFilename|KStringHandler::ljust|KStringHandler::rjust)'  * | egrep -v '\.(svn|libs|o|moc|l[ao])|Makefile(.in)?' 
