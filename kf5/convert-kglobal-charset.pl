#!/usr/bin/perl -w

# Laurent Montel <montel@kde.org> (2014)
# KGlobal::charsets() -> KCharsets::charsets()
# find -iname "*.cpp" -o -iname "*.h"|xargs kde-dev-scripts/kf5/convert-kglobal-charset.pl

use strict;
use File::Basename;
use lib dirname($0);
use functionUtilkde;

foreach my $file (@ARGV) {

    my $modified;
    open(my $FILE, "<", $file) or warn "We can't open file $file:$!\n";
    my @l = map {
        my $orig = $_;
        s/\bKGlobal::charsets\s*\(\)/KCharsets::charsets\(\)/;

        $modified ||= $orig ne $_;
        $_;
    } <$FILE>;

    if ($modified) {
        open (my $OUT, ">", $file);
        print $OUT @l;
        close ($OUT);
        functionUtilkde::addIncludeInFile($file, "KCharsets");
    }
}

functionUtilkde::diffFile( "@ARGV" );
