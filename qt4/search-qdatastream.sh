#!/bin/sh
grep -rl "QDataStream " * | egrep -v '\.(svn|libs|o|moc|l[ao])|Makefile(.in)?'
