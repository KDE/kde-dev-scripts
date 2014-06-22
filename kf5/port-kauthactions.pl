#!/usr/bin/perl -w

# Laurent Montel <montel@kde.org> (2014)
# Port to new KAuth API
# find -iname "*.cpp" -o -iname "*.h" |xargs kde-dev-scripts/kf5/port-kauthactions.pl

use strict;
use File::Basename;
use lib dirname($0);
use functionUtilkde;

foreach my $file (@ARGV) {

    my $modified;
    open(my $FILE, "<", $file) or warn "We can't open file $file:$!\n";
    my @l = map {
        my $orig = $_;
        s/KDE4_AUTH_HELPER_MAIN/KAUTH_HELPER_MAIN/;

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
