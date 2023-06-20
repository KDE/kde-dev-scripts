#!/bin/sh
# Copyright Laurent Montel <montel@kde.org> 2023

list_qml=`find -iname '*.qml'`;
for f in $list_qml; do
	list_import=`grep -e "import" $f | grep -e " as " |  sed 's/^.*as //' | perl -pi -e 's/ //'`;
	for i in $list_import; do
        	#echo "import namespace $i";
	        number=`grep $i $f|wc -l`;
		#echo "number namespace '$i' $number in  $f";
		if test $number == 1 ; then
			echo "remove $i in file $f";
			perl -pi -e "s,import.* as $i\n,," $f;
		fi
	done;
done;

