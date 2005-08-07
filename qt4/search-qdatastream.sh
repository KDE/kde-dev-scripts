#!/bin/sh
grep -rl "QDataStream " * |grep -v "\.svn" |grep -v "\.libs" | grep -v "\.o" | grep -v Makefile | grep -v Makefile.in  | grep -v "\.moc" | grep -v "\.lo" | grep -v "\.la" |grep -v "\.Ulo" | grep -v "_skel.cpp" | grep -v "_stub.cpp" | grep -v "kopete"
