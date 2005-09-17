#!/bin/sh
egrep -rl '(kaccelmanager.h)'  * | egrep -v '\.(svn|libs|o|moc|l[ao])|Makefile(.in)?' 
