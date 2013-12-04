#!/bin/sh

# This script splits out all the frameworks from kdelibs into separate repositories
# and puts them into ../frameworks/

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
    frameworkname=`basename $dir`

    if [ $frameworkname = 'CMakeLists.txt' ]; then
      continue;
    fi

    rm -rf $dest/$frameworkname
    cp -a $dir $dest/
    cd $dest/$frameworkname

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

`dirname $0`/../astyle-kdelibs >/dev/null

git commit -q -a -m "Code reformatted using kde-dev-scripts/astyle-kdelibs.
Use git blame -w `git rev-parse --short HEAD` to show authorship as it was before this commit."

echo "$dest/$frameworkname done."

done

