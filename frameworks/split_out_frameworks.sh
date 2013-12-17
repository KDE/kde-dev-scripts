#!/bin/sh

# This script splits out all the frameworks from kdelibs into separate repositories
# and puts them into ../frameworks/.
#
# For each framework, first it creates an empty repository and it imports the
# current code, into the original subdirectory. For example, kconfig will be a
# repository containing a tier1/kconfig/ directory. Then, in a second commit,
# the code is moved to the root directory of the repository.
#
# Doing the move in two steps like this lets git follow the history better.
# When the old kdelibs history is grafted on the new repositories, the history
# will contain a commit that deletes everything except that framework, and then
# another commit moving the framework to the root.
#
# Finally, in a third commit, the code is reformatted using astyle.

origproject=kdelibs
origbranch=frameworks
origsha1=`git rev-parse HEAD`

if [ ! -d tier1 ]; then
  "echo Run this script from the toplevel of the monolithic repository, there must be a tier1 subdirectory"
  exit 1
fi

dest=../frameworks
mkdir -p $dest
here=$PWD

for dir in tier1/* tier2/* tier3/* tier4/*; do
    cd $here
    if [ -f $dir ]; then
      continue;
    fi
    frameworkname=`basename $dir`
    frameworkdest=$dest/$frameworkname
    rm -rf $frameworkdest
    # eg. create ../frameworks/kjs/tier1
    mkdir -p $(dirname $frameworkdest/$dir)
    # eg. copy tier1/kjs to ../frameworks/kjs/tier1/kjs
    cp -a $dir $frameworkdest/$dir/
    cd $frameworkdest

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

# eg. moves tier1/kconfig/* to .
git mv $dir/* .
git commit -q -m "Move $frameworkname code to the root directory."

echo "$frameworkdest done."

done

