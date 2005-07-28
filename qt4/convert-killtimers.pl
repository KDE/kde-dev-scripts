#!/usr/bin/perl

# laurent Montel <montel@kde.org>
# This function convert killTimers to qt4 function

use lib qw( . );
use functionUtilkde; #utilisation des fonctions kde qui me sont utiles


foreach my $file (@ARGV) {
    #TODO fix for killTimers with arguments
    functionUtilkde::substInFile {
	s!killTimers\s*\(\s*\);!QAbstractEventDispatcher::instance()->unregisterTimers(this);!;
    } $file;
	functionUtilkde::addIncludeInFile( $file, "QAbstractEventDispatcher");
}
functionUtilkde::diffFile( "@ARGV" );

