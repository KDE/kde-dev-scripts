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
		$changes =~ s!Q3VBox!KVBox!;
		$_ = $changes . $suite . $end . "\n";
	    }
	    if (my ($prefix, $suite, $end) = /(.*)(addHBoxPage.*)\s*$/) {
		my $changes = $prefix;
		$changes =~ s!Q3Frame!KHBox!;
		$changes =~ s!Q3HBox!KHBox!;
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
	    s!#include <kaccelmanager.h>!#include <kacceleratormanager.h>!;
	    s!KStringHandler::matchFilename!KStringHandler::matchFileName!;
	    if ( $_ =~ /KApplication::random/ ) {
		s!KApplication::random!KRandom::random!;
		$necessaryToAddIncludeRandom = 1;
	    }
		if ( $_ =~ /KApplication::randomString/ ) {
        s!KApplication::randomString!KRandom::randomString!;
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
		s!KPopupMenu!KMenu!g;
	    #TODO test it, perhaps remove all before isRestored (for example if( kapp-> isRestored())
	    s!kapp->isRestored!kapp->isSessionRestored!;
	    s!#include <kuniqueapp.h>!#include <kuniqueapplication.h>!;
	    s!#include <kapp.h>!#include <kapplication.h>!;
	    s!#include <kstddirs.h>!#include <kstandarddirs.h>!;
	    s!#include <kcodecs.h>!#include <kcodecs.h>!;
		#kde3support
		s!#include <kdockwidget.h>!#include <k3dockwidget.h>!;
		s!#include <kmdichildarea.h>!#include <k3mdichildarea.h>!;
		s!#include <kmdichildfrmcaption.h>!#include <k3mdichildfrmcaption.h>!;
		s!#include <kmdichildfrm.h>!#include <k3mdichildfrm.h>!;
		s!#include <kmdichildview.h>!#include <k3mdichildview.h>!;
		s!#include <kmdidefines.h>!#include <k3mdidefines.h>!;
		s!#include <kmdidockcontainer.h>!#include <k3mdidockcontainer.h>!;
		s!#include <kmdidocumentviewtabwidget.h>!#include <k3mdidocumentviewtabwidget.h>!;
		s!#include <kmdifocuslist.h>!#include <k3mdifocuslist.h>!;
		s!#include <kmdiguiclient.h>!#include <k3mdiguiclient.h>!;
		s!#include <kmdiiterator.h>!#include <k3mdiiterator.h>!;
		s!#include <kmdilistiterator.h>!#include <k3mdilistiterator.h>!;		
		s!#include <kmdimainfrm.h>!#include <k3mdimainfrm.h>!;
		s!#include <kmdinulliterator.h>!#include <k3mdinulliterator.h>!;
		s!#include <kmditaskbar.h>!#include <k3mditaskbar.h>!;
		s!#include <kmditoolviewaccessor.h>!#include <k3mditoolviewaccessor.h>!;
        
		# remove deprecated header
		s!#include <kcolordlg.h>!#include <kcolordialog.h>!;
		s!#include <kcolorbtn.h>!#include <kcolorbutton.h>!;
		s!#include <kdatepik.h>!#include <kdatepicker.h>!;
		s!#include <kdualcolorbtn.h>!#include <kdualcolorbutton.h>!;
		s!#include <kxmlgui.h>!#include <kxmlguifactory.h>!;	
		if( /K3ColorDrag/ ) {
			s!K3ColorDrag!K3ColorDrag!g;
			$warning = $warning . "Necessary to add \$\(LIB_KDE3SUPPORT\) into Makefile.am when $file is \n";
		}
	
		s!#include <kcolordrag.h>!#include <k3colordrag.h>!;
		s!KColorDrag!K3ColorDrag!g;

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
		s!KLocale::setMainCatalogue!KLocale::setMainCatalog!;
		s!KGlobal::locale\(\)->insertCatalogue!KGlobal::locale\(\)->insertCatalog!;
		s!KGlobal::locale\(\)->setMainCatalogue!KGlobal::locale\(\)->setMainCatalog!;
		
		s!KLocale::removeCatalogue!KLocale::removeCatalog!;
		s!KGlobal::locale\(\)->removeCatalog!KGlobal::locale\(\)->removeCatalog!;
		s!KGlobal::locale\(\)->removeCatalog!KGlobal::locale\(\)->removeCatalog!;

        s!KLocale::setActiveCatalog!KLocale::setActiveCatalog!;
        s!KGlobal::locale\(\)->setActiveCatalog!KGlobal::locale\(\)->setActiveCatalog!;
        s!KGlobal::locale\(\)->setActiveCatalog!KGlobal::locale\(\)->setActiveCatalog!;
		
		s!KApplication::addCmdLineOptions!KCmdLineArgs::addStdCmdLineOptions!;
		
		if ( /kapp->authorizeKAction/ ) {
			s!kapp->authorizeKAction!KAuthorized::authorizeKAction!;
			$necessaryToAddIncludeAuthorize = 1;
	}
		if ( /kapp->authorize/ ) {
			s!kapp->authorize!KAuthorized::authorizeKAction!;
			$necessaryToAddIncludeAuthorize = 1;
		}
		if ( /KApplication::startServiceByDesktopName/ ) {
			s!KApplication::startServiceByDesktopName!KToolInvocation::startServiceByDesktopName!;
			$necessaryToAddIncludektoolinvocation = 1;
		}
		if ( /kapp->startServiceByDesktopName/ ) {
		        s!kapp->startServiceByDesktopName!KToolInvocation::startServiceByDesktopName!;
				$necessaryToAddIncludektoolinvocation = 1;
		}
		if ( /kapp->kdeinitExec/ ) {
				s!kapp->kdeinitExec!KToolInvocation::kdeinitExec!;
				$necessaryToAddIncludektoolinvocation = 1;
		}
		if( /KApplication::kdeinitExec/ ) {
			s!KApplication::kdeinitExec!KToolInvocation::kdeinitExec!;
			$necessaryToAddIncludektoolinvocation = 1;
		}
		if ( /kapp->invokeHelp/ ) {
				s!kapp->invokeHelp!KToolInvocation::invokeHelp!;
				$necessaryToAddIncludektoolinvocation = 1;
		}
		if ( /kapp->invokeMailer/ ) {
				s!kapp->invokeMailer!KToolInvocation::invokeMailer!;
				$necessaryToAddIncludektoolinvocation = 1;
		}
        if ( /kapp->invokeBrowser/ ) {
                s!kapp->invokeBrowser!KToolInvocation::invokeBrowser!;
                $necessaryToAddIncludektoolinvocation = 1;
        }
        if ( /kapp->kdeinitExecWait/ ) {
                s!kapp->kdeinitExecWait!KToolInvocation::kdeinitExecWait!;
                $necessaryToAddIncludektoolinvocation = 1;
        }
		if ( /KApplication::startServiceByDesktopPath/ ) {
				s!KApplication::startServiceByDesktopPath!KToolInvocation::startServiceByDesktopPath!;
				$necessaryToAddIncludektoolinvocation = 1;
		}
		if ( /KApplication::startServiceByName/ ) {
				s!KApplication::startServiceByName!KToolInvocation::startServiceByName!;
				$necessaryToAddIncludektoolinvocation = 1;
		}
        if ( /kapp->startServiceByName/ ) {
                s!kapp->startServiceByName!KToolInvocation::startServiceByName!;
                $necessaryToAddIncludektoolinvocation = 1;
        }	
		if ( /new KRun/ ) {
				$warning = $warning . "Be carrefull perhaps necessary to add parent into constructor in file : $file\n";
		}
		s!KParts::ComponentFactory::createInstanceFromLibrary!KLibLoader::createInstance!;
	    #KMainWindow
		#s/(?<!KMainWindow::memberList\(\))KMainWindow::memberList/KMainWindow::memberList()/;	
		#s!KMainWindow::memberList!KMainWindow::memberList()!;
	    if ( /kapp->getDisplay/ ) {
			s!kapp->getDisplay\s*\(\s*\)!QX11Info::display()!;
			$necessaryToAddInclude = 1;
	    }
	    if( /enableSounds/ ) {
		s!enableSounds\(\)!setEnableSounds\(true\)!;
	    }
	    if( /disableSounds/ ) {
        	s!disableSounds\(\)!setEnableSounds\(false\)!;
	    }
	    my $valuereturn = functionUtilkde::removeObjectName( $_, "KColorButton");
	    if( $valuereturn ) {
		$_ = $valuereturn;
	    }
	    my $valuereturn = functionUtilkde::removeObjectName( $_, "KListView");
            if( $valuereturn ) {
                $_ = $valuereturn;
            }
			
	    my $valuereturn = functionUtilkde::removeObjectName( $_, "KPushButton");
            if( $valuereturn ) {
                $_ = $valuereturn;
		
	    }
		#if( /setStatusText/ ) {
		#	s!setStatusText!setToolTip!;
		#}
        if( /kapp->geometryArgument/ ) {
        	s!kapp->geometryArgument\s*\(\s*\);!QString geometry;\nKCmdLineArgs *args = KCmdLineArgs::parsedArgs("kde");\nif (args->isSet("geometry"))\ngeometry = args->getOption("geometry");\n!;
        }
        s!KFileMetaInfo::KiloBytes!KFileMimeTypeInfo::KibiBytes!;		
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
