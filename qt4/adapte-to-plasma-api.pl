#!/usr/bin/perl

# laurent Montel <montel@kde.org>
# This function allows to adapt file to new plasma namespace for applet (just for help)

use lib qw( . );
use functionUtilkde; 

use strict; 

open(my $F, q(find -name "*" |));
my $file;
while ($file = <$F>) {
 	chomp;
	next if functionUtilkde::excludeFile( $file);

	my $modified;
		
	open(my $FILE, $file) or warn "We can't open file $$!\n";
	my @l = map {
	    my $orig = $_;
			s!KPanelApplet::Normal!Plasma::Normal!;
			s!KPanelApplet::About!Plasma::About!;
			s!KPanelApplet::Help!Plasma::Help!;
			s!KPanelApplet::Preferences!Plasma::Preferences!;
			s!KPanelApplet::Stretch!Plasma::Stretch!;
			s!include <kpanelapplet.h>!include <plasma/kpanelapplet.h>!;
            s!KPanelApplet::pLeft!Plasma::Left!;
            s!KPanelApplet::pRight!Plasma::Right!;
            s!KPanelApplet::pTop!Plasma::Top!;
            s!KPanelApplet::pBottom!Plasma::Bottom!;
			s!KPanelApplet::ReportBug!Plasma::ReportBug!;
	    $modified ||= $orig ne $_;
	    $_;
	} <$FILE>;

	if ($modified) {
	    open (my $OUT, ">$file");
	    print $OUT @l;
	}
}
functionUtilkde::diffFile( <$F> );

