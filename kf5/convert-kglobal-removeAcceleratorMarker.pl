#!/usr/bin/perl -w

# Laurent Montel <montel@kde.org> (2014)
# KLocale::global()->removeAcceleratorMarker(...) -> KLocalizedString::removeAcceleratorMarker(...)
# find -iname "*.cpp"|xargs kde-dev-scripts/kf5/convert-kglobal-removeAcceleratorMarker.pl

use strict;
use File::Basename;
use lib dirname($0);
use functionUtilkde;

foreach my $file (@ARGV) {

    my $modified;
    open(my $FILE, "<", $file) or warn "We can't open file $file:$!\n";
    my $needKlocalizedString;
    my @l = map {
        my $orig = $_;
        if ( /KLocale::global\(\)\-\>removeAcceleratorMarker/ ) {
           warn "removeAcceleratorMarker found \n";
           s,KLocale::global\(\)\-\>removeAcceleratorMarker\b,KLocalizedString::removeAcceleratorMarker,;
           $modified = 1;
           $needKlocalizedString = 1;
        }

        $modified ||= $orig ne $_;
        $_;
    } <$FILE>;

    if ($modified) {
        open (my $OUT, ">", $file);
        print $OUT @l;
        close ($OUT);
        if ( $needKlocalizedString ) {
           functionUtilkde::addIncludeInFile($file, "KLocalizedString");
        }
    }
}

functionUtilkde::diffFile( "@ARGV" );
