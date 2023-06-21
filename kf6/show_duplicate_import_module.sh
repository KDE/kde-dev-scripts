#!/bin/sh
# Copyright Laurent Montel <montel@kde.org> 2023

list_qml=`find -iname '*.qml'`;
for f in $list_qml; do
	echo "file $f";
	grep -w "import.*$" $f | sort
	echo "end";
done;

