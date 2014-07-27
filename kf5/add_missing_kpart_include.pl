#!/usr/bin/perl -w

# Laurent Montel <montel@kde.org> (2014)
# now we have a include for each kpart and not just #include <kparts/part.h>
# find -iname "*.cpp" -o -iname "*.h" |xargs kde-dev-scripts/kf5/add_missing_kpart_include.pl

use strict;
use File::Basename;
use lib dirname($0);
use functionUtilkde;

foreach my $file (@ARGV) {

    my $modified;
    open(my $FILE, "<", $file) or warn "We can't open file $file:$!\n";
    my @l = map {
        my $orig = $_;
        if (/public KParts::ReadOnlyPart/) {
           $modified = 1;
        }
        $modified ||= $orig ne $_;
        $_;
    } <$FILE>;

    if ($modified) {
        open (my $OUT, ">", $file);
        print $OUT @l;
        close ($OUT);
        functionUtilkde::addIncludeInFile($file, "kparts/readonlypart.h");
    }
}

functionUtilkde::diffFile( "@ARGV" );
