#!/bin/bash
# icons-kde3to4-renamer.sh
#
# By Danny Allen (dannya@kde.org)
# Licenced under the LGPL

script_base="/home/kde-devel/kde/src/KDE/kdesdk/scripts/qt4/icons-kde3tokde4-renamer"
icon_base="/home/kde-devel/kde/src/KDE/kdeartwork/IconThemes/primary/scalable"
# icon_base="/home/kde-devel/kde/src/KDE/kdeaccessibility/IconThemes/mono/scalable"
old_extension="svgz"
new_extension="svgz"

# check if directories set are valid
if [ ! -d $script_base ] || [ ! -d $icon_base ]; then
    echo "Correctly set the locations at the top of this script first!"
    exit 1;
fi

# set icon types
types="7"
type[0]="actions"
type[1]="apps"
type[2]="categories"
type[3]="devices"
type[4]="emblems"
type[5]="mimetypes"
type[6]="places"

# counters (leave as 0)
implemented="0"
correct="0"
not_implemented="0"
removed="0"


# change to icon location
cd $icon_base


function make_changes {
    area="$1"

    # check if directory exists for type (otherwise make it)
    if [ ! -d $area ]; then
        mkdir $area
        svn add $area

        echo "created $area directory"
    fi

    # change to the type directory
    cd $area

    # report area
    echo "--------------------------------"
    echo "Processing $area..."
    echo "--------------------------------"

    # set filenames
    rename_list="$script_base/$area"_rename.txt
    remove_list="$script_base/$area"_remove.txt
    missing_log="$script_base/missing_"$area.txt

    # remove old logs
    if [ -f "$missing_log" ]; then
        rm $missing_log
    fi

    ########################
    # renamings
    ########################
    # determine number of icons to process
    list_entries=`wc -l $rename_list | grep -o "[0-9][0-9]\?[0-9]\?[0-9]\?[0-9]\? " | grep -o "[0-9][0-9]\?[0-9]\?[0-9]\?[0-9]\?"`

    echo ""
    number="0"
    while [ "$number" -le "$list_entries" ]; do
        original_name=`head --lines=$(($number + 1)) $rename_list | tail --lines=1`
        new_name=`head --lines=$(($number + 2)) $rename_list | tail --lines=1`

        # look for icon, if present, svn mv
        if [ -f "$original_name.$old_extension" ]; then
            # do the move
            svn mv "$original_name.$old_extension" "$new_name.$new_extension"

            echo "$original_name renamed to $new_name"
            echo ""

            # increment implemented counter
            implemented=$(($implemented + 1))
        elif [ -f "$new_name.$new_extension" ]; then
            # icon already renamed
            correct=$(($correct + 1))
        else
            # add to missing_icons log
            echo "$new_name" >> $missing_log

            # increment not implemented counter
            not_implemented=$(($not_implemented + 1))
        fi

        number=$(($number + 3))
    done

    # output status report
    echo ""
    echo "###############"
    echo "$implemented icons renamed."
    echo "$correct were already correctly named."
    if [ $not_implemented -gt "0" ]; then
        echo ""
        echo "Missing icons in this set: $not_implemented"
        echo ""
        echo "See missing_$area.txt for more details"
    fi
    echo "###############"


    ########################
    # removals
    ########################
    # determine number of icons to process
    list_entries=`wc -l $remove_list | grep -o "[0-9][0-9]\?[0-9]\?[0-9]\?[0-9]\? " | grep -o "[0-9][0-9]\?[0-9]\?[0-9]\?[0-9]\?"`

    echo ""
    number="0"
    while [ "$number" -le "$list_entries" ]; do
        original_name=`head --lines=$(($number + 1)) $remove_list | tail --lines=1`

        # look for icon, if present, svn mv
        if [ -f "$original_name.$old_extension" ]; then
            # do the removal
            svn rm "$original_name.$old_extension"

            echo "removed $original_name"
            echo ""

            # increment implemented counter
            removed=$(($removed + 1))
        fi

        number=$(($number + 1))
    done

    # output status report
    echo ""
    echo "###############"
    echo "$removed icons removed."
    echo "###############"

    # change back to parent dir
    type_num=$(($type_num + 1))
    cd ..
}


# do all the changes, or only on a specific area?
if [ "$1" != "" ]; then
    # perform changes on one area
    make_changes $1
else
    # rename filesystems to places if neccessary
    if [ -d "filesystems" ]; then
        svn mv filesystems places

        echo "filesystems/ renamed to places/"
    fi

    # perform changes on all areas
    type_num="0"
    while [ "$type_num" -lt "$types" ]; do
        make_changes ${type[$type_num]}
    done
fi

echo ""
echo ""
echo "Don't forget to 'svn commit' your changes!"
echo ""