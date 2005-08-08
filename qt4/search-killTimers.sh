#!/bin/sh
grep -rl "killTimers" * | egrep -v '\.(svn|libs|o|moc|l[ao])|Makefile(.in)?|kopete' 
