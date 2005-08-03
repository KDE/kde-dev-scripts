#!/usr/bin/perl

# laurent Montel <montel@kde.org>
# This function allows to adapt file to new KDE4 API
# change KDialogBase::addPage return a QFrame now and not Q3Frame
# change KDialogBase::makeMainWidget return a QFrame and not a Q3Frame

use lib qw( . );
use functionUtilkde; #utilisation des fonctions kde qui me sont utiles


foreach my $file (@ARGV) {
    functionUtilkde::substInFile {
	if (my ($prefix, $suite, $end) = /(.*)(addPage.*)\s*$/) {
            my $changes = $prefix;
            $changes =~ s!Q3Frame!QFrame!;
            $_ = $changes . $suite . $end . "\n";
	}
    if (my ($prefix, $suite, $end) = /(.*)(makeMainWidget.*)\s*$/) {
            my $changes = $prefix;
            $changes =~ s!Q3Frame!QFrame!;
            $_ = $changes . $suite . $end . "\n";
    }
    if (my ($prefix, $suite, $end) = /(.*)(plainPage.*)\s*$/) {
            my $changes = $prefix;
            $changes =~ s!Q3Frame!QFrame!;
            $_ = $changes . $suite . $end . "\n";
    }
    if (my ($prefix, $suite, $end) = /(.*)(KWin::info.*)\s*$/) {
            my $changes = $prefix;
            $changes =~ s!KWin::info!KWin::windowInfo!;
            $_ = $changes . $suite . $end . "\n";
    }
    } $file;
}
functionUtilkde::diffFile( "@ARGV" );

