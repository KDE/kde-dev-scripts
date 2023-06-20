#!/bin/sh
# Copyright Laurent Montel <montel@kde.org> 2023
git grep -l "PlasmaCore.FrameSvgItem" | xargs perl -pi -e 's,PlasmaCore.FrameSvgItem,KSvg.FrameSvgItem,g'
git grep -l "PlasmaCore.SvgItem" | xargs perl -pi -e 's,PlasmaCore.SvgItem,KSvg.SvgItem,g'
git grep -l "PlasmaCore.Svg" | xargs perl -pi -e 's,PlasmaCore.Svg,KSvg.Svg,g'

# Add import org.kde.kirigami 2.20 as Kirigami

list_import=`git grep -l "as PlasmaCore"`;
echo " list file which has import PlasmaCore $list_import";
for i in $list_import; do
        echo "list $i";
        number=`grep 'import org.kde.ksvg 1.0 as KSvg' $i|wc -l`;
        echo $number;
        if test $number == 0 ; then
               echo "import org.kde.ksvg 1.0 as KSvg in $i";
              perl -pi -e 's,as PlasmaCore,as PlasmaCore\nimport org.kde.ksvg 1.0 as KSvg,' $i;
        fi
done;


list_PlasmaKirigami_file=`git grep -l ksvg`;
echo " list file which has kirigami: $list_PlasmaKirigami_file";
for i in $list_PlasmaKirigami_file; do
        echo "list:::: $i";
        number=`grep KSvg $i|wc -l`;
        echo $number;
        if test $number == 1 ; then
              perl -pi -e 's,import org.kde.ksvg 1.0 as KSvg\n,,' $i;
        fi
done;


# Remove import org.kde.plasma.core 2.0 as PlasmaCore if we don't need it
list_PlasmaCore_file=`git grep -l PlasmaCore`;
for i in $list_PlasmaCore_file; do 
	echo $i;
	number=`grep KSvg $i|wc -l`;
	if test $number = 1 ; then
		perl -pi -e 's,import org.kde.plasma.core 2.0 as PlasmaCore\n,,' $i;
	fi
done;

echo " add Svg to CMakeLists.txt file";

