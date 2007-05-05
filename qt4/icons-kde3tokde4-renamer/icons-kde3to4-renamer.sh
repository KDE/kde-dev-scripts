#!/bin/bash
# icons-kde3to4-renamer.sh
#
# By Danny Allen (dannya@kde.org)
# Licenced under the LGPL

script_base="/home/danny/work/projects/icon_rename"
icon_base="/home/kde4/svn/kdeartwork/IconThemes/primary/scalable"
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

# change to icon location
cd $icon_base

# rename filesystems to places if neccessary
if [ -d "filesystems" ]; then
    svn mv filesystems places
    echo "filesystems/ renamed to places/"
fi

implemented="0"
correct="0"
not_implemented="0"
removed="0"

type_num="0"
while [ "$type_num" -lt "$types" ]; do
    # check if directory exists for type (otherwise make it)
    if [ ! -d ${type[$type_num]} ]; then
        mkdir ${type[$type_num]}
    fi

    # change to the type directory
    cd ${type[$type_num]}

    # report area
    echo "--------------------------------"
    echo "Processing ${type[$type_num]}..."
    echo "--------------------------------"

    # set filenames
    rename_list="$script_base/${type[$type_num]}_rename.txt"
    remove_list="$script_base/${type[$type_num]}_remove.txt"
    missing_log="$script_base/missing_${type[$type_num]}.txt"

    ########################
    # renamings
    ########################
    # determine number of icons to process
    list_entries=`wc -l $rename_list | grep -o "[0-9][0-9]\?[0-9]\?[0-9]\?[0-9]\?"`

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
        echo "See missing_${type[$type_num]}.txt for more details"
    fi
    echo "###############"


    ########################
    # removals
    ########################
    # determine number of icons to process
    list_entries=`wc -l $remove_list | grep -o "[0-9][0-9]\?[0-9]\?[0-9]\?[0-9]\?"`

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
done