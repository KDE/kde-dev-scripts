#!/usr/bin/perl -w

# Laurent Montel <montel@kde.org> (2014)
# find -iname "*.cpp"|xargs kde-dev-scripts/kf5/remove-kde4support.pl

use strict;
use File::Basename;
use lib dirname($0);
use functionUtilkde;

foreach my $file (@ARGV) {

    my $modified;
    my $needQApplicationHeader;
    my $needQDesktopWidgetHeader;
    my $needKColorScheme;
    open(my $FILE, "<", $file) or warn "We can't open file $file:$!\n";
    my @l = map {
        my $orig = $_;
        
        s,::self\(\)\-\>writeConfig\(\),::self\(\)\-\>save\(\),;
        s,::self\(\)->readConfig\(\),::self\(\)\-\>load\(\),;
        if (/KGlobalSettings::dndEventDelay/) {
           s,KGlobalSettings::dndEventDelay\b,QApplication::startDragDistance,;
           $needQApplicationHeader = 1;
        }
        if (/KGlobalSettings::desktopGeometry/) {
           s,KGlobalSettings::desktopGeometry\b,QApplication::desktop\(\)\-\>screenGeometry,;
           $needQApplicationHeader = 1;
           $needQDesktopWidgetHeader = 1;
        }
        if (/KGlobalSettings::createApplicationPalette/) {
           s,KGlobalSettings::createApplicationPalette\b,KColorScheme::createApplicationPalette,;
           $needKColorScheme = 1;
        }

        $modified ||= $orig ne $_;
        $_;
    } <$FILE>;

    if ($modified) {
        open (my $OUT, ">", $file);
        print $OUT @l;
        close ($OUT);
        if ($needQApplicationHeader) {
           functionUtilkde::addIncludeInFile($file, "QApplication");
        }
        if ($needQDesktopWidgetHeader) {
           functionUtilkde::addIncludeInFile($file, "QDesktopWidget");
        }
        if ($needKColorScheme) {
           functionUtilkde::addIncludeInFile($file, "KColorScheme");
        }
    }
}

functionUtilkde::diffFile( "@ARGV" );
