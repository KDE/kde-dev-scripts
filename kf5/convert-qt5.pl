#!/usr/bin/perl -w

# David Faure <faure@kde.org>
# Qt4 -> Qt5

use strict;
use File::Basename;
use lib dirname($0);
use functionUtilkde;

foreach my $file (@ARGV) {

    my $infoVar;
    my $urlVar;

    # I don't use functionUtilkde::substInFile because it touches all files, even those which were not modified.
    my $modified;
    open(my $FILE, "<", $file) or warn "We can't open file $file:$!\n";
    my @l = map {
        my $orig = $_;

        s/qFindChild<([^>]*)>\(\s*(\w+)\s*,\s*/$2->findChild<$1>(/;
        s/qFindChildren<([^>]*)>\(\s*(\w+)\s*,\s*/$2->findChildren<$1>(/;

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
