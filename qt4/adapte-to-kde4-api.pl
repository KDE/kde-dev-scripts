#!/usr/bin/perl

# laurent Montel <montel@kde.org>
# This function allows to adapt file to new KDE4 API
# change KDialogBase::addPage return a QFrame now and not Q3Frame
# change KDialogBase::makeMainWidget return a QFrame and not a Q3Frame

use lib qw( . );
use functionUtilkde; 


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
	s!#include <kuniqueapp.h>!#include <kuniqueapplication.h>!;
	s!#include <kapp.h>!#include <kapplication.h>!;
	s!#include <kstddirs.h>!#include <kstandarddirs.h>!;
	s!#include <kcodecs.h>!#include <kcodecs.h>!;
	s!KStartupInfo::appStarted!KStartupInfo::appStarted!;
	s!KInputDialog::getText!KInputDialog::getText!;
	s!#include <kde_file.h>!#include <kde_file.h>!;
    s!cancelPressed!cancelPressed!;
	s!suggestNewNamePressed!suggestNewNamePressed!;
	s!renamePressed!renamePressed!;
	s!skipPressed!skipPressed!;
	s!autoSkipPressed!autoSkipPressed!;
	s!overwritePressed!overwritePressed!;
	s!overwriteAllPressed!overwriteAllPressed!;
	s!resumePressed!resumePressed!;
	s!resumeAllPressed!resumeAllPressed!;
	#KMainWindow
    s/(?<!KMainWindow::memberList\(\))KMainWindow::memberList/KMainWindow::memberList()/;	
	s!KMainWindow::memberList!KMainWindow::memberList()!;
    if ( /kapp->getDisplay/ ) {
            s!kapp->getDisplay\s*\(\s*\)!QX11Info::display()!;
			functionUtilkde::addIncludeInFile( $file, "QX11Info");
    }
	} $file;
}
functionUtilkde::diffFile( "@ARGV" );

