#!/usr/bin/perl -w

# Laurent Montel <montel@kde.org> (2014)
# KGlobal::locale() -> KLocale::global()
# find -iname "*.cpp"|xargs kde-dev-scripts/kf5/convert-kglobal-locale.pl

use strict;
use File::Basename;
use lib dirname($0);
use functionUtilkde;

foreach my $file (@ARGV) {

    my $modified;
    open(my $FILE, "<", $file) or warn "We can't open file $file:$!\n";
    my @l = map {
        my $orig = $_;
        if (/KGlobal::locale\(\)\-\>insertCatalog/) {
           s/KGlobal::locale\(\)\-\>insertCatalog/\/\/QT5 KLocale::global\(\)\-\>insertCatalog/;
        }
        s/\bKGlobal::locale\s*\(\)/KLocale::global\(\)/g;

        $modified ||= $orig ne $_;
        $_;
    } <$FILE>;

    if ($modified) {
        open (my $OUT, ">", $file);
        print $OUT @l;
        close ($OUT);
        functionUtilkde::addIncludeInFile($file, "KLocale");
    }
}

functionUtilkde::diffFile( "@ARGV" );
