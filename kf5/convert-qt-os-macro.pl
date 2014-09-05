#!/usr/bin/perl -w

# Laurent Montel <montel@kde.org> (2014)
# Q_WS* -> Q_OS*
# find -iname "*.cpp" -o -iname "*.h"|xargs kde-dev-scripts/kf5/convert-qt-os-macro.pl

use strict;
use File::Basename;
use lib dirname($0);
use functionUtilkde;

foreach my $file (@ARGV) {

    my $modified;
    open(my $FILE, "<", $file) or warn "We can't open file $file:$!\n";
    my @l = map {
        my $orig = $_;
        s/\bQ_WS_WIN/Q_OS_WIN/g;
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
