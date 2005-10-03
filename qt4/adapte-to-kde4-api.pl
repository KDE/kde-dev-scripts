#!/usr/bin/perl

# laurent Montel <montel@kde.org> 2005 GPL
# This function allows to adapt file to new KDE4 API

use lib qw( . );
use functionUtilkde;
use strict;

open(my $F, q(find -name "*" |));
my $file;
while ($file = <$F>) {
 	chomp $file;
	next if functionUtilkde::excludeFile( $file);

	my $modified;
	my $necessaryToAddInclude;	
	my $necessaryToAddIncludeRandom;
	open(my $FILE, $file) or warn "We can't open file $file:$!\n";
	my @l = map {
	    my $orig = $_;
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
	    if (my ($prefix, $suite, $end) = /(.*)(addVBoxPage.*)\s*$/) {
		my $changes = $prefix;
		$changes =~ s!Q3Frame!KVBox!;
		$_ = $changes . $suite . $end . "\n";
	    }	
	    if (my ($prefix, $suite, $end) = /(.*)(addHBoxPage.*)\s*$/) {
		my $changes = $prefix;
		$changes =~ s!Q3Frame!KHBox!;
		$_ = $changes . $suite . $end . "\n";
	    }	
	    if (my ($prefix, $suite, $end) = /(.*)(makeVBoxMainWidget.*)\s*$/) {
		my $changes = $prefix;
		$changes =~ s!Q3Frame!KVBox!;
		$_ = $changes . $suite . $end . "\n";
	    }	
	    if (my ($prefix, $suite, $end) = /(.*)(makeHBoxMainWidget.*)\s*$/) {
		my $changes = $prefix;
		$changes =~ s!Q3Frame!KHBox!;
		$_ = $changes . $suite . $end . "\n";
	    }
	    s!#include <kaccelmanager.h>!#include <kacceleratormanager.h>!;
	    s!KStringHandler::matchFilename!KStringHandler::matchFileName!;
	    if ( $_ =~ /KApplication::random/ ) {
		s!KApplication::random!KRandom::random!;
		$necessaryToAddIncludeRandom = 1;
	    }
	    s!KFindDialog::WholeWordsOnly!KFind::WholeWordsOnly!;
	    s!KFindDialog::FromCursor!KFind::FromCursor!;
	    s!KFindDialog::SelectedText!KFind::SelectedText!;
	    s!KFindDialog::CaseSensitive!KFind::CaseSensitive!;
	    s!KFindDialog::FindBackwards!KFind::FindBackwards!;
	    s!KFindDialog::RegularExpression!KFind::RegularExpression!;
	    s!KFindDialog::FindIncremental!KFind::FindIncremental!;
	    s!KFindDialog::MinimumUserOption!KFind::MinimumUserOption!;
	    s!kdatetbl.h!kdatetable.h!;
	    #TODO test it, perhaps remove all before isRestored (for example if( kapp-> isRestored())
	    s!kapp->isRestored!QApplication::isSessionRestored!;
	    s!#include <kuniqueapp.h>!#include <kuniqueapplication.h>!;
	    s!#include <kapp.h>!#include <kapplication.h>!;
	    s!#include <kstddirs.h>!#include <kstandarddirs.h>!;
	    s!#include <kcodecs.h>!#include <kcodecs.h>!;
		s!#include <kdockwidget.h>!#include <k3dockwidget.h>!;
	    s!KStartupInfo::appStarted!KStartupInfo::appStarted!;
	    s!KInputDialog::getText!KInputDialog::getText!;
	    s!#include <kde_file.h>!#include <kde_file.h>!;
		s!#include <kpopupmenu.h>!#include <kmenu.h>!;
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
			$necessaryToAddInclude = 1;
	    }
	    $modified ||= $orig ne $_;
	    $_;
	} <$FILE>;

	if ($modified) {
	    open (my $OUT, ">$file");
	    print $OUT @l;
	}
	if ($necessaryToAddInclude) {
			functionUtilkde::addIncludeInFile( $file, "QX11Info");
	}
	if( $necessaryToAddIncludeRandom ) {
		functionUtilkde::addIncludeInFile( $file, "krandom.h");
	}
    }
functionUtilkde::diffFile( <$F> );

