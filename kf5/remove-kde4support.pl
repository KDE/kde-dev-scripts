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
    my $needQStandardPaths;
    my $needKFormat;
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
        if (/KGlobalSettings::documentPath/) {
           s,KGlobalSettings::documentPath\(\),QStandardPaths::writableLocation\(QStandardPaths::DocumentsLocation\),;
           $needQStandardPaths = 1;
        }
        if (/KGlobalSettings::desktopPath/) {
           s,KGlobalSettings::desktopPath\(\),QStandardPaths::writableLocation\(QStandardPaths::DesktopLocation\),;
           $needQStandardPaths = 1;
        }
        if (/KGlobalSettings::musicPath/) {
           s,KGlobalSettings::musicPath\(\),QStandardPaths::writableLocation\(QStandardPaths::MusicLocation\),;
           $needQStandardPaths = 1;
        }
        if (/KGlobalSettings::videosPath/) {
           s,KGlobalSettings::videosPath\(\),QStandardPaths::writableLocation\(QStandardPaths::MoviesLocation\),;
           $needQStandardPaths = 1;
        }
        if (/KGlobalSettings::downloadPath/) {
           s,KGlobalSettings::downloadPath\(\),QStandardPaths::writableLocation\(QStandardPaths::DownloadLocation\),;
           $needQStandardPaths = 1;
        }
        if (/KGlobalSettings::picturesPath/) {
           s,KGlobalSettings::picturesPath\(\),QStandardPaths::writableLocation\(QStandardPaths::PicturesLocation\),;
           $needQStandardPaths = 1;
        }
        if (/KLocale::global\(\)\-\>formatByteSize/) {
           s,KLocale::global\(\)\-\>formatByteSize,KFormat\(\).formatByteSize,g;
           $needKFormat = 1;
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
        if ($needQStandardPaths) {
           functionUtilkde::addIncludeInFile($file, "QStandardPaths");
        }
        if ($needKFormat) {
           functionUtilkde::addIncludeInFile($file, "KFormat");
        }
    }
}

functionUtilkde::diffFile( "@ARGV" );
