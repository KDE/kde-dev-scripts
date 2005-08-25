#!/bin/sh
egrep -rl '\.eof' * | egrep -v '\.(svn|libs|o|moc|l[ao])|Makefile(.in)?' 
