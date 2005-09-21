#!/bin/sh
grep -rl Q3CString * |egrep -v '(\.svn|\.o|\.a|\.deps|\.libs|Makefile.in|Makefile|\_skel|\.kidl)'
