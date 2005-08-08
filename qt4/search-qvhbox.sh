#!/bin/sh
grep -rl "Q3HBox" * | egrep -v '\.(svn|libs|o|moc|l[ao])|Makefile(.in)?|kopete'
