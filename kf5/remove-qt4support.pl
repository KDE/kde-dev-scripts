#!/usr/bin/perl -w

# Laurent Montel <montel@kde.org> (2014)
# find -iname "*.cpp"|xargs kde-dev-scripts/kf5/remove-qt4support.pl

use strict;
use File::Basename;
use lib dirname($0);
use functionUtilkde;

foreach my $file (@ARGV) {

    my $modified;
    my $convertDndDelay;
    open(my $FILE, "<", $file) or warn "We can't open file $file:$!\n";
    my @l = map {
        my $orig = $_;
        
        s,::self\(\)\-\>writeConfig\(\),::self\(\)\-\>save\(\),;
        if (/KGlobalSettings::dndEventDelay/) {
           s,KGlobalSettings::dndEventDelay\b,QApplication::startDragDistance,;
           $convertDndDelay = 1;
        }
        $modified ||= $orig ne $_;
        $_;
    } <$FILE>;

    if ($modified) {
        open (my $OUT, ">", $file);
        print $OUT @l;
        close ($OUT);
        if ($convertDndDelay) {
           functionUtilkde::addIncludeInFile($file, "QApplication");
        }
    }
}

functionUtilkde::diffFile( "@ARGV" );
