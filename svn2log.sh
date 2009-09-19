#!/bin/bash
#
# Shell script for creating a ChangeLog file from SVN
#
# Copyright 2009 Tom Albers <toma@kde.org>
# License: GNU General Public License V2 or later

PARAM=$1;

if test -z "$PARAM"; then
    PARAM=".";
fi;

CURRENT=`dirname $0`;

echo "Fetching committers...";
svn cat svn://anonsvn.kde.org/home/kde/trunk/kde-common/accounts > /tmp/accounts.$PPID

echo "Creating changelog...";
svn log -v --xml $1 | python $CURRENT/svn2log.py --users=/tmp/accounts.$PPID --users-charset=UTF8

rm /tmp/accounts.$PPID
