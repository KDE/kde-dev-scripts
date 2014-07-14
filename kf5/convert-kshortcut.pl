#!/usr/bin/perl -w

# Laurent Montel <montel@kde.org> (2014)
# KShortcut => QKeySequence
# find -iname "*.cpp" -o -iname "*.h" -o -iname "*.ui" |xargs kde-dev-scripts/kf5/convert-kshortcut.pl

use strict;
use File::Basename;
use lib dirname($0);
use functionUtilkde;

foreach my $file (@ARGV) {

    my $modified;
    open(my $FILE, "<", $file) or warn "We can't open file $file:$!\n";
    my @l = map {
        my $orig = $_;
        s/KShortcut\s*\(\s*QKeySequence\s*\(/QKeySequence(/;
        s/\bsetShortcuts\s*\(\s*KShortcut\b/setShortcut(QKeySequence/;
        s/KShortcuts\s*\(\s*QKeySequence\s*\(/QKeySequence(/;


        s/\bKShortcut\b/QKeySequence/g;
        s/\<KShortcut\b\>/\<QKeySequence>/ if (/#include/);
        s/\<kshortcut.h\>/\<QKeySequence>/ if (/#include/);
        $modified ||= $orig ne $_;
        $_;
    } <$FILE>;

    if ($modified) {
        open (my $OUT, ">", $file);
        print $OUT @l;
        close ($OUT);
    }
}

functionUtilkde::diffFile( "@ARGV" );
