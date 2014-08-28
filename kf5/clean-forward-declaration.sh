#!/bin/sh

# Laurent Montel <montel@kde.org> (2014)
# cd <directory> ; ./clean-forward-declaration.sh 

#list all .h files.
list=`find -name '*.h' | egrep -v '(\.svn|build)'`;
 # list=`ls *.H | egrep -v '(\.svn|build)'`;
for file in $list ; do
	#get class line (don't get it when we have "friend"/"template" or "public" word or *Private class)
	class=`echo "$file" | xargs grep "class" | egrep -v '(public|friend|template|Private)' `;
	if test ! -z "$class" ; then
		echo "filename :$file";
		#By default it's defined as "class Kurl;" => remove "class " and ";"
		newval=`echo "$class" | perl -pi -e 's!class !!g'`;
		newval=`echo "$newval" | perl -pi -e 's!;!!g'`;
		
		#need to have all class name => use awk to split string, separator is "\n";
		splitline=`echo $newval | awk -F' ' 'END { for (i = 1 ; i <= NF ; i++) { tab[i] = $i ; print tab[i] } } '`;
		for i in $splitline ; do
			#Look at into file if there is multiple defined of class name => if number == 1 => it's not used
			val=`echo "$file" | xargs grep "$i" | wc -l`;
			if test "$val" = "1" ; then
 				# Remove "class <classname>;" when it's not used
				perl -pi -e "s!^\s*class $i;\n!!" $file;	
				echo "$i is unique, we will remove it from file";
			fi
		done
	fi
done
#look at diff
git diff .

