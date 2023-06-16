#!/bin/sh
# Copyright Laurent Montel <montel@kde.org> 2023
git grep -l "PlasmaCore.Theme.smallestFont" | xargs perl -pi -e 's,PlasmaCore.Theme.smallestFont,Kirigami.Theme.smallFont,g'
git grep -l "PlasmaCore.Theme.NormalColorGroup" | xargs perl -pi -e 's,PlasmaCore.Theme.NormalColorGroup,Kirigami.Theme.Window,g'
git grep -l "PlasmaCore.ColorScope.colorGroup" | xargs perl -pi -e 's,PlasmaCore.ColorScope.colorGroup,Kirigami.Theme.colorSet,g'
git grep -l "PlasmaCore.ColorScope.inherit" | xargs perl -pi -e 's,PlasmaCore.ColorScope.inherit,Kirigami.Theme.inherit,g'
git grep -l "PlasmaCore.Units.largeSpacing" | xargs perl -pi -e 's,PlasmaCore.Units.largeSpacing,Kirigami.Units.gridUnit,g'
git grep -l "PlasmaCore.Units." | xargs perl -pi -e 's,PlasmaCore.Units.,Kirigami.Units.,g'
git grep -l "PlasmaCore.ColorScope.inherit" | xargs perl -pi -e 's,PlasmaCore.ColorScope.inherit,Kirigami.Theme.inherit,g'
git grep -l "PlasmaCore.Theme." | xargs perl -pi -e 's,PlasmaCore.Theme.,Kirigami.Theme.,g'

git grep -l "PlasmaCore.Units.devicePixelRatio" | xargs perl -pi -e 's,PlasmaCore.Units.devicePixelRatio,1,g'
git grep -l "height: PlasmaCore.Units.roundToIconSize" | xargs perl -pi -e 's,height: PlasmaCore.Units.roundToIconSize\(.*\),height: Kirigami.Units.iconSizes.sizeForLabels,g'
git grep -l "PlasmaCore.Units.roundToIconSize" | xargs perl -pi -e 's,PlasmaCore.Units.roundT   oIconSize,Kirigami.Units.roundedIconSize,g'


# Add import org.kde.kirigami 2.20 as Kirigami

list_import=`git grep -l "as PlasmaCore"`;
echo " list file which has import PlasmaCore $list_import";
for i in $list_import; do
        echo "list2222222:::: $i";
        number=`grep 'mport org.kde.kirigami 2.20 as Kirigami' $i|wc -l`;
        echo $number;
        if test $number == 0 ; then
               echo "insert import org.kde.kirigami 2.20 as Kirigam in $i";
              perl -pi -e 's,as PlasmaCore,as PlasmaCore\nimport org.kde.kirigami 2.20 as Kirigami,' $i;
        fi
done;


#git grep -l "as PlasmaCore" | xargs perl -pi -e 's,as PlasmaCore,as PlasmaCore\nimport org.kde.kirigami 2.20 as Kirigami,'
list_PlasmaKirigami_file=`git grep -l kirigami`;
echo " list file which has kirigami: $list_PlasmaKirigami_file";
for i in $list_PlasmaKirigami_file; do
        echo "list:::: $i";
        number=`grep Kirigami $i|wc -l`;
        echo $number;
        if test $number == 1 ; then
              perl -pi -e 's,import org.kde.kirigami 2.20 as Kirigami\n,,' $i;
        fi
done;


# Remove import org.kde.plasma.core 2.0 as PlasmaCore if we don't need it
list_PlasmaCore_file=`git grep -l PlasmaCore`;
for i in $list_PlasmaCore_file; do 
	echo $i;
	number=`grep PlasmaCore $i|wc -l`;
	if test $number = 1 ; then
		perl -pi -e 's,import org.kde.plasma.core 2.0 as PlasmaCore\n,,' $i;
	fi
done;

