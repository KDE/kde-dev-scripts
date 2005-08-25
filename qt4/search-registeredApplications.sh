#!/bin/sh
grep -rl "registeredApplications" * | egrep -v '\.(svn|libs|o|moc|l[ao])|Makefile(.in)?'
