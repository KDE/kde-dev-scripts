#!/bin/sh
grep -rl "Q3WhatsT" * |grep -v "\.svn" | grep -v "kopete"
