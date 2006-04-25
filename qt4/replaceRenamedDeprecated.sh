#!/bin/bash
# I do not assert any rights on these few trivial lines of code, Reinhold Kainhofer

# replace old function names with new function names, when all parameters stay the same
#e.g.:
# ./replaceRenamedDeprecated.sh setColumnStretch setColumnStretch

old=$1
new=$2

for i in `grep -le $1 -r [ac-z]* | grep -v '\.svn'`; do
  sed "s/$1/$2/g" $i > $i.new;
	echo $i;
	diff $i $i.new;
	mv $i.new $i
done
