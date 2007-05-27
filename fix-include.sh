#!/bin/sh
# fix-include.sh 
#
# By Laurent Montel <montel@kde.org>
# Licenced under the LGPL

# How to test it ? 
# go into <kde4_install_dir>/include/KDE/
# sh fix-include.sh

test_include() {
  for file in $path/* ;
  do
        if test -d $file ; then
		echo "Check include into directory : $file";
		path=$file;
	else
		# Search file which have  "#include" 
        	include=`echo "$file" | xargs grep "#include"`;
		# Get include file
        	new=`echo "$include" |perl -pi -e 's!#include !!'`;
        	new=`echo "$new" |perl -pi -e 's!\\"!!g'`;
        	# Get absolute include
		headerfile=$PWD/$new;
		# Test error
        	if test ! -f "$headerfile" ; then
                	echo "Header <$file> is not correct it try to load <$new>";
        	fi
	fi
  done 
}

path=$PWD;
test_include

