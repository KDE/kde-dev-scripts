#!/bin/sh
grep -rl "killTimers" * |grep -v "\.svn" |grep -v "\.libs" | grep -v "\.o" | grep -v Makefile | grep -v Makefile.in  | grep -v "\.moc" | grep -v "\.lo" | grep -v "\.la"
