#!/usr/bin/perl

# laurent Montel <montel@kde.org> 2005 GPL
# This function allows to adapt file to new KDE4 API

use File::Basename;
use lib dirname( $0 );
use functionUtilkde;
use strict;

open(my $F, q(find -name "*" |));
my $file;
my $warning;
while ($file = <$F>) {
 	chomp $file;
	next if functionUtilkde::excludeFile( $file);

	my $modified;
	my $necessaryToAddInclude;	
	my $necessaryToAddIncludeRandom;
	my $necessaryToAddIncludeAuthorize;
	my $necessaryToAddIncludektoolinvocation;
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
		$changes =~ s!Q3VBox!KVBox!;
		$_ = $changes . $suite . $end . "\n";
	    }	
	    if (my ($prefix, $suite, $end) = /(.*)(makeHBoxMainWidget.*)\s*$/) {
		my $changes = $prefix;
		$changes =~ s!Q3Frame!KHBox!;
		$changes =~ s!Q3HBox!KHBox!;
		$_ = $changes . $suite . $end . "\n";
	    }
	    s!#include <kacceleratormanager.h>!#include <kacceleratormanager.h>!;
	    s!KStringHandler::matchFileName!KStringHandler::matchFileName!;
	    if ( $_ =~ /KRandom::random/ ) {
		s!KRandom::random!KRandom::random!;
		$necessaryToAddIncludeRandom = 1;
	    }
	    s!KFind::WholeWordsOnly!KFind::WholeWordsOnly!;
	    s!KFind::FromCursor!KFind::FromCursor!;
	    s!KFind::SelectedText!KFind::SelectedText!;
	    s!KFind::CaseSensitive!KFind::CaseSensitive!;
	    s!KFind::FindBackwards!KFind::FindBackwards!;
	    s!KFind::RegularExpression!KFind::RegularExpression!;
	    s!KFind::FindIncremental!KFind::FindIncremental!;
	    s!KFind::MinimumUserOption!KFind::MinimumUserOption!;
	    s!kdatetable.h!kdatetable.h!;
		s!KMenu!KMenu!g;
	    #TODO test it, perhaps remove all before isRestored (for example if( kapp-> isRestored())
	    s!kapp->isSessionRestored!kapp->isSessionRestored!;
	    s!#include <kuniqueapplication.h>!#include <kuniqueapplication.h>!;
	    s!#include <kapplication.h>!#include <kapplication.h>!;
	    s!#include <kstandarddirs.h>!#include <kstandarddirs.h>!;
	    s!#include <kcodecs.h>!#include <kcodecs.h>!;
		#kde3support
		s!#include <k3dockwidget.h>!#include <k3dockwidget.h>!;
		s!#include <k3mdichildarea.h>!#include <k3mdichildarea.h>!;
		s!#include <k3mdichildfrmcaption.h>!#include <k3mdichildfrmcaption.h>!;
		s!#include <k3mdichildfrm.h>!#include <k3mdichildfrm.h>!;
		s!#include <k3mdichildview.h>!#include <k3mdichildview.h>!;
		s!#include <k3mdidefines.h>!#include <k3mdidefines.h>!;
		s!#include <k3mdidockcontainer.h>!#include <k3mdidockcontainer.h>!;
		s!#include <k3mdidocumentviewtabwidget.h>!#include <k3mdidocumentviewtabwidget.h>!;
		s!#include <k3mdifocuslist.h>!#include <k3mdifocuslist.h>!;
		s!#include <k3mdiguiclient.h>!#include <k3mdiguiclient.h>!;
		s!#include <k3mdiiterator.h>!#include <k3mdiiterator.h>!;
		s!#include <k3mdilistiterator.h>!#include <k3mdilistiterator.h>!;		
		s!#include <k3mdimainfrm.h>!#include <k3mdimainfrm.h>!;
		s!#include <k3mdinulliterator.h>!#include <k3mdinulliterator.h>!;
		s!#include <k3mditaskbar.h>!#include <k3mditaskbar.h>!;
		s!#include <k3mditoolviewaccessor.h>!#include <k3mditoolviewaccessor.h>!;
        if( /K3ColorDrag/ ) {
			s!K3ColorDrag!K3ColorDrag!g;
			$warning = $warning . "Necessary to add \$\(LIB_KDE3SUPPORT\) into Makefile.am when $file is \n";
		}
	
		s!#include <k3colordrag.h>!#include <k3colordrag.h>!;
		s!K3ColorDrag!K3ColorDrag!g;

	    s!KStartupInfo::appStarted!KStartupInfo::appStarted!;
	    s!KInputDialog::getText!KInputDialog::getText!;
	    s!#include <kde_file.h>!#include <kde_file.h>!;
		s!#include <kmenu.h>!#include <kmenu.h>!;
#include <QX11Info>
#include <ktoolinvocation.h>
#include <krandom.h>
#include <kauthorized.h>
	    s!cancelPressed!cancelPressed!;
	    s!suggestNewNamePressed!suggestNewNamePressed!;
	    s!renamePressed!renamePressed!;
	    s!skipPressed!skipPressed!;
	    s!autoSkipPressed!autoSkipPressed!;
	    s!overwritePressed!overwritePressed!;
	    s!overwriteAllPressed!overwriteAllPressed!;
	    s!resumePressed!resumePressed!;
	    s!resumeAllPressed!resumeAllPressed!;
		s!KLocale::setMainCatalog!KLocale::setMainCatalog!;
		s!KGlobal::locale\(\)->insertCatalogue!KGlobal::locale\(\)->insertCatalog!;
		s!KGlobal::locale\(\)->setMainCatalogue!KGlobal::locale\(\)->setMainCatalog!;
		
		s!KLocale::removeCatalog!KLocale::removeCatalog!;
		s!KGlobal::locale\(\)->removeCatalog!KGlobal::locale\(\)->removeCatalog!;
		s!KGlobal::locale\(\)->removeCatalog!KGlobal::locale\(\)->removeCatalog!;

        s!KLocale::setActiveCatalog!KLocale::setActiveCatalog!;
        s!KGlobal::locale\(\)->setActiveCatalog!KGlobal::locale\(\)->setActiveCatalog!;
        s!KGlobal::locale\(\)->setActiveCatalog!KGlobal::locale\(\)->setActiveCatalog!;
		
		s!KCmdLineArgs::addStdCmdLineOptions!KCmdLineArgs::addStdCmdLineOptions!;
		
		if ( /KAuthorized::authorizeKAction/ ) {
			s!KAuthorized::authorizeKAction!KAuthorized::authorizeKAction!;
			$necessaryToAddIncludeAuthorize = 1;
	}
		if ( /KAuthorized::authorizeKAction/ ) {
			s!KAuthorized::authorizeKAction!KAuthorized::authorizeKAction!;
			$necessaryToAddIncludeAuthorize = 1;
		}
		if ( /KToolInvocation::startServiceByDesktopName/ ) {
			s!KToolInvocation::startServiceByDesktopName!KToolInvocation::startServiceByDesktopName!;
			$necessaryToAddIncludektoolinvocation = 1;
		}
		if ( /KToolInvocation::startServiceByDesktopName/ ) {
		        s!KToolInvocation::startServiceByDesktopName!KToolInvocation::startServiceByDesktopName!;
				$necessaryToAddIncludektoolinvocation = 1;
		}
		if ( /KToolInvocation::kdeinitExec/ ) {
				s!KToolInvocation::kdeinitExec!KToolInvocation::kdeinitExec!;
				$necessaryToAddIncludektoolinvocation = 1;
		}
		if ( /KToolInvocation::invokeHelp/ ) {
				s!KToolInvocation::invokeHelp!KToolInvocation::invokeHelp!;
				$necessaryToAddIncludektoolinvocation = 1;
		}
		if ( /KToolInvocation::invokeMailer/ ) {
				s!KToolInvocation::invokeMailer!KToolInvocation::invokeMailer!;
				$necessaryToAddIncludektoolinvocation = 1;
		}
        if ( /KToolInvocation::invokeBrowser/ ) {
                s!KToolInvocation::invokeBrowser!KToolInvocation::invokeBrowser!;
                $necessaryToAddIncludektoolinvocation = 1;
        }
        if ( /KToolInvocation::kdeinitExecWait/ ) {
                s!KToolInvocation::kdeinitExecWait!KToolInvocation::kdeinitExecWait!;
                $necessaryToAddIncludektoolinvocation = 1;
        }
		if ( /KToolInvocation::startServiceByDesktopPath/ ) {
				s!KToolInvocation::startServiceByDesktopPath!KToolInvocation::startServiceByDesktopPath!;
				$necessaryToAddIncludektoolinvocation = 1;
		}
		if ( /KToolInvocation::startServiceByName/ ) {
				s!KToolInvocation::startServiceByName!KToolInvocation::startServiceByName!;
				$necessaryToAddIncludektoolinvocation = 1;
		}
        if ( /KToolInvocation::startServiceByName/ ) {
                s!KToolInvocation::startServiceByName!KToolInvocation::startServiceByName!;
                $necessaryToAddIncludektoolinvocation = 1;
        }	
		if ( /new KRun/ ) {
				$warning = $warning . "Be carrefull perhaps necessary to add parent into constructor in file : $file\n";
		}
		s!KLibLoader::createInstance!KLibLoader::createInstance!;
	    #KMainWindow
	    s/(?<!KMainWindow::memberList()\(\))KMainWindow::memberList/KMainWindow::memberList()/;	
		#s!KMainWindow::memberList()!KMainWindow::memberList()!;
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
	if ( $necessaryToAddIncludektoolinvocation ) {
			functionUtilkde::addIncludeInFile( $file, "ktoolinvocation.h");
	}
	if( $necessaryToAddIncludeRandom ) {
		functionUtilkde::addIncludeInFile( $file, "krandom.h");
	}
	if( $necessaryToAddIncludeAuthorize  ) {
		functionUtilkde::addIncludeInFile( $file, "kauthorized.h");
	}
    }
functionUtilkde::diffFile( <$F> );
warn "Warning: $warning\n";
