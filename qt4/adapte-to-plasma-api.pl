#!/usr/bin/perl

# laurent Montel <montel@kde.org>
# This function allows to adapt file to new plasma namespace for applet (just for help)

use lib qw( . );
use functionUtilkde; 


foreach my $file (@ARGV) {
    functionUtilkde::substInFile {
			s!KPanelApplet::Normal!Plasma::Normal!;
			s!KPanelApplet::About!Plasma::About!;
			s!KPanelApplet::Help!Plasma::Help!;
			s!KPanelApplet::Preferences!Plasma::Preferences!;
			s!KPanelApplet::Stretch!Plasma::Stretch!;
			s!include <kpanelapplet.h>!include <plasma/kpanelapplet.h>!;
    } $file;
}
functionUtilkde::diffFile( "@ARGV" );

