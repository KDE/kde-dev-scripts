#!/bin/sh
grep -rl "qHeapSort"  * | egrep -v '\.(svn|libs|o|moc|l[ao])|Makefile(.in)?'
