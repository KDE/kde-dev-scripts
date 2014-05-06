#!/usr/bin/perl -w

# Laurent Montel <montel@kde.org> (2014)
# convert kascii function to qt function
# find -iname "*.cpp"|xargs kde-dev-scripts/kf5/convert-kascii.pl

use strict;
use File::Basename;
use lib dirname($0);
use functionUtilkde;

foreach my $file (@ARGV) {

    my $modified;
    open(my $FILE, "<", $file) or warn "We can't open file $file:$!\n";
    my @l = map {
        my $orig = $_;
        s/\bkasciistricmp\b/qstricmp/;

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
