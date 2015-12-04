#!/bin/sh

# This script splits out the kdepim components into separate repositories
# and puts them into ../kdepim-split/.
#
# For each component, first it creates an empty repository and it imports the
# current code, into the original subdirectory. For example, libksieve will be a
# repository containing a libksieve/ directory. Then, in a second commit,
# the code is moved to the root directory of the repository.
#
# Doing the move in two steps like this lets git follow the history better.
# When the old kdepim history is grafted on the new repositories, the history
# will contain a commit that deletes everything except that directory, and then
# another commit moving the code to the root.
#
origproject=kdepim
origbranch=master
origsha1=`git rev-parse HEAD`

if [ ! -d libksieve ]; then
  echo "Run this script from the root of the kdepim repository"
  exit 1
fi

dest=../kdepim-split
mkdir -p $dest
here=$PWD

for dir in calendarsupport eventviews incidenceeditor kdepim-apps-lib kdgantt2 libgravatar libkdepim libkleo libksieve mailcommon mailimporter messagelib pimcommon grantleetheme; do
    cd $here
    if [ -f $dir ]; then
      continue;
    fi
    componentdest=$dest/$dir
    # eg. create empty ../kdepim-split/libksieve
    rm -rf $componentdest
    mkdir -p $componentdest

    # eg. copy libksieve to ../kdepim-split/libksieve/libksieve
    cp -a $dir $componentdest/$dir/
    cd $componentdest

git init
git add .

git commit -q -F - <<EOF

Initial import from the monolithic $origproject.

This is the beginning of revision history for this module. If you
want to look at revision history older than this, please refer to the
techbase wiki for how to use Git history grafting. At the time of
writing, this wiki is located here:

http://community.kde.org/Frameworks/GitOldHistory

If you have already performed the grafting and you don't see any
history beyond this commit, try running "git log" with the "--follow"
argument.

Branched from the monolithic repo, $origproject $origbranch branch, at commit
$origsha1

EOF

# eg. moves libksieve/* to .
git mv $dir/* .
git commit -q -m "Move $dir code to the root directory."

echo "$componentdest done."

done

