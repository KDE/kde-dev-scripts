#!/usr/bin/perl 

# Laurent Montel <montel@kde.org>
# This function convert qt_xdisplay and qt_rootwin to qt4 function
# it added include too

use File::Basename;
use lib dirname( $0 );
use functionUtilkde;
use strict;

foreach my $file (@ARGV) {
	functionUtilkde::substInFile {
	s!qt_xdisplay\s*\(\s*\)!QX11Info::display()!;
	s!qt_xrootwin\s*\(\s*\)!QX11Info::appRootWindow()!;
    } $file;
	functionUtilkde::addIncludeInFile( $file, "QX11Info");
}
functionUtilkde::diffFile( "@ARGV" );
