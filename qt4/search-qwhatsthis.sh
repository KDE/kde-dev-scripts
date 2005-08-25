#!/bin/sh
grep -rl "Q3WhatsT" * | egrep -v '\.(svn|libs|o|moc|l[ao])|Makefile(.in)?'
