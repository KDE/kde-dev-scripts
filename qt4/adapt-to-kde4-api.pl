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
    my @necessaryIncludes = ();
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
		$changes =~ s!QHBox!KHBox!;
	    $_ = $changes . $suite . $end . "\n";
	}	
	if (my ($prefix, $suite, $end) = /(.*)(makeVBoxMainWidget.*)\s*$/) {
	    my $changes = $prefix;
	    $changes =~ s!Q3Frame!KVBox!;
	    $changes =~ s!Q3VBox!KVBox!;
		$changes =~ s!QVBox!KVBox!;
	    $_ = $changes . $suite . $end . "\n";
	}	
	if (my ($prefix, $suite, $end) = /(.*)(makeHBoxMainWidget.*)\s*$/) {
	    my $changes = $prefix;
	    $changes =~ s!Q3Frame!KHBox!;
	    $changes =~ s!Q3HBox!KHBox!;
		$changes =~ s!QHBox!KHBox!;
	    $_ = $changes . $suite . $end . "\n";
	}
	s!#include <kaccelmanager.h>!#include <kacceleratormanager.h>!;
	s!KStringHandler::matchFilename!KStringHandler::matchFileName!;
	if ( $_ =~ /KApplication::random/ ) {
	    s!KApplication::random!KRandom::random!;
	    push(@necessaryIncludes, "krandom.h");
	}
	if ( $_ =~ /kapp->random/ ) {
	    s!kapp->random!KRandom::random!;
	    push(@necessaryIncludes, "krandom.h");
	}
	if ( $_ =~ /Q3StyleSheet::escape/ ) {
		s!Q3StyleSheet::escape!Qt::escape!;
		push(@necessaryIncludes, "QTextDocument");
	}
    if ( $_ =~ /Q3StyleSheet::convertFromPlainText/ ) {
        s!Q3StyleSheet::convertFromPlainText!Qt::convertFromPlainText!;
		s!Q3StyleSheetItem::!Qt::!;
        push(@necessaryIncludes, "QTextDocument");
    }

	if ( $_ =~ /\b(\w+(?:\.|->))htmlURL\(\)/ ) { # KURL::htmlURL() had to disappear
		$var=$1;
		s/${var}htmlURL\(\)/Qt::escape(${var}prettyURL())/;
		push(@necessaryIncludes, "QTextDocument");
	}

	if ( $_ =~ /Q3StyleSheet::mightBeRichText/ ) {
		s!Q3StyleSheet::mightBeRichText!Qt::mightBeRichText!;
		push(@necessaryIncludes, "QTextDocument")
	}
				
	
	if ( $_ =~ /KApplication::randomString/ ) {
	    s!KApplication::randomString!KRandom::randomString!;
	    push(@necessaryIncludes, "krandom.h");
	}
	if ( $_ =~ /kapp->randomString/ ) {
	    s!kapp->randomString!KRandom::randomString!;
	    push(@necessaryIncludes, "krandom.h");
	}
	s!kapp->propagateSessionManager!KWorkSpace::propagateSessionManager!;
	s!kapp->requestShutDown!KWorkSpace::requestShutDown!;
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
	#add some standard version :)
	s!kapp.isRestored!kapp.isSessionRestored!;
	s!app.isRestored!app.isSessionRestored!;
	s!a.isRestored!a.isSessionRestored!;

	s!Qt::WordBreak!Qt::TextWordWrap!;
	s!Qt::SingleLine!Qt::TextSingleLine!;
	s!Qt::DontClip!Qt::TextDontClip!;
	s!Qt::ExpandTabs!Qt::TextExpandTabs!;
	s!Qt::ShowPrefix!Qt::TextShowMnemonic!;
	s!Qt::BreakAnywhere!Qt::TextWrapAnywhere!;
	s!Qt::DontPrint!Qt::TextDontPrint!;
	s!Qt::IncludeTrailingSpaces!Qt::TextIncludeTrailingSpaces!;
	s!Qt::NoAccel!Qt::TextHideMnemonic!;
	s!Qt::Key_BackSpace!Qt::Key_Backspace!;
	s!Qt::Key_BackTab!Qt::Key_Backtab!;
	s!Qt::Key_Prior!Qt::Key_PageUp!;
	s!Qt::Key_Next!Qt::Key_PageDown!;
	s!Qt::Key_MediaPrev([\s*|,])!Qt::Key_MediaPrevious\1!;
	    
	s!Qt::arrowCursor!Qt::ArrowCursor!;
	s!Qt::upArrowCursor!Qt::UpArrowCursor!;
	s!Qt::crossCursor!Qt::CrossCursor!;
	s!Qt::waitCursor!Qt::WaitCursor!;
	s!Qt::ibeamCursor!Qt::IBeamCursor!;
	s!Qt::sizeVerCursor!Qt::SizeHorCursor!;
	s!Qt::sizeHorCursor!Qt::SizeHorCursor!;
	s!Qt::sizeBDiagCursor!Qt::SizeBDiagCursor!;
	s!Qt::sizeFDiagCursor!Qt::SizeFDiagCursor!;
	s!Qt::sizeAllCursor!Qt::SizeAllCursor!;
	s!Qt::blankCursor!Qt::BlankCursor!;
	s!Qt::splitVCursor!Qt::SplitVCursor!;
	s!Qt::splitHCursor!Qt::SplitHCursor!;
	s!Qt::pointingHandCursor!Qt::PointingHandCursor!;
	s!Qt::forbiddenCursor!Qt::ForbiddenCursor!;
	s!Qt::whatsThisCursor!Qt::WhatsThisCursor!;

	s!QSlider::Below!QSlider::TicksBelow!;
	s!QSlider::Above!QSlider::TicksAbove!;
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
    
	s!#include <kpassdlg.h>!#include <kpassworddialog.h>!;
	s!#include <kprogress.h>!#include <kprogressbar.h>!;
	
	#If there is more than one argument add KGuiItem now I think that it will easy to fix it.
	s!setButtonOKText!setButtonOK!;
	s!setButtonApplyText!setButtonApply!;
	s!setButtonCancelText!setButtonCancel!;
		
	# remove deprecated header
	s!#include <kcolordlg.h>!#include <kcolordialog.h>!;
	s!#include <kcolorbtn.h>!#include <kcolorbutton.h>!;
	s!#include <kdatepik.h>!#include <kdatepicker.h>!;
	s!#include <kdualcolorbtn.h>!#include <kdualcolorbutton.h>!;
	s!#include <kxmlgui.h>!#include <kxmlguifactory.h>!;	

	# Qt3 name class
	s!QIconSet!QIcon!g;
	s!QWMatrix!QMatrix!g;
	s!QGuardedPtr!QPointer!g;

        # KURL -> KUrl in all class names [but not in env vars, ifdefs, include guards etc.]
        s!KURLCompletion!KUrlCompletion!g;
        s!KURLCombo!KUrlCombo!g;
        s!KURLRequester!KUrlRequester!g;
        s!KURLPixmapProvider!KUrlPixmapProvider!g;
		s!KURLLabel!KUrlLabel!g;
	s!IO_ReadOnly!QIODevice::ReadOnly!;
	s!IO_WriteOnly!QIODevice::WriteOnly!;
	s!IO_ReadWrite!QIODevice::ReadWrite!;
	s!IO_Append!QIODevice::Append!;
	s!IO_Truncate!QIODevice::Truncate!;
	s!IO_Translate!QIODevice::Text!;

	
	#KKeyNative
	s!KKeyNative::modX\(\s*KKey::SHIFT\s*\)!KKeyNative::modXShift\(\)!;
	s!KKeyNative::modX\(\s*KKey::CTRL\s*\)!KKeyNative::modXCtrl\(\)!;
	s!KKeyNative::modX\(\s*KKey::ALT\s*\)!KKeyNative::modXAlt\(\)!;
	s!KKeyNative::modX\(\s*KKey::ALT\s*\)!KKeyNative::modXAlt\(\)!;
	s!KKeyNative::modX\(\s*KKey::META\s*\)!KKeyNative::modXMeta\(\)!;
	s!KKeyNative::modX\(\s*KKey::WIN\s*\)!KKeyNative::modXWin\(\)!;

	# plasma changes:
	s!KPanelApplet::Normal!Plasma::Normal!;
	s!KPanelApplet::About!Plasma::About!;
	s!KPanelApplet::Help!Plasma::Help!;
	s!KPanelApplet::Preferences!Plasma::Preferences!;
	s!KPanelApplet::Stretch!Plasma::Stretch!;
	#s!include <kpanelapplet.h>!include <plasma/kpanelapplet.h>!;
	s!KPanelApplet::pLeft!Plasma::Left!;
	s!KPanelApplet::pRight!Plasma::Right!;
	s!KPanelApplet::pTop!Plasma::Top!;
	s!KPanelApplet::pBottom!Plasma::Bottom!;
	s!KPanelApplet::ReportBug!Plasma::ReportBug!;

	if ( /K3ColorDrag/ ) {
	    s!K3ColorDrag!K3ColorDrag!g;
	    $warning = $warning . "Necessary to add \$\(LIB_KDE3SUPPORT\) into Makefile.am when $file is \n";
	}
	
	s!#include <kcolordrag.h>!#include <k3colordrag.h>!;
	s!KColorDrag!K3ColorDrag!g;

	s!Q3ValueList\s*<\s*KMimeType::Ptr\s*>!KMimeType::List!;
	s!Q3ValueListIterator\s*<\s*KMimeType::Ptr\s*>!KMimeType::List::const_iterator!;
	s!Q3ValueList\s*<\s*KService::Ptr\s*>!KService::List!;
	s!Q3ValueListIterator\s*<\s*KService::Ptr\s*>!KService::List::const_iterator!;

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
	s!KGlobal::locale\(\)->removeCatalogue!KGlobal::locale\(\)->removeCatalog!;
	s!KGlobal::locale\(\)->removeCatalogue!KGlobal::locale\(\)->removeCatalog!;

        s!KLocale::setActiveCatalogue!KLocale::setActiveCatalog!;
        s!KGlobal::locale\(\)->setActiveCatalogue!KGlobal::locale\(\)->setActiveCatalog!;
        s!KGlobal::locale\(\)->setActiveCatalogue!KGlobal::locale\(\)->setActiveCatalog!;
	
	if ( /KApplication::ShutdownType|KApplication::ShutdownTypeHalt|KApplication::ShutdownMode|KApplication::ShutdownTypeReboot|KApplication::ShutdownTypeNone/ ) {
	    push(@necessaryIncludes, "kworkspace.h");
	}
	s!KApplication::ShutdownTypeHalt!KWorkSpace::ShutdownTypeHalt!;
	s!KApplication::ShutdownTypeReboot!KWorkSpace::ShutdownTypeReboot!;
	s!KApplication::ShutdownTypeNone!KWorkSpace::ShutdownTypeNone!;	
	s!KApplication::ShutdownType!KWorkSpace::ShutdownType!;
	s!KApplication::ShutdownMode!KWorkSpace::ShutdownMode!;
        s!([, (])KMAX\(!\1qMax\(!g;
        s!([, (])KMIN\(!\1qMin\(!g;
        s!([, (])kMin\(!\1qMin\(!g;
	s!([, (])kMax\(!\1qMax\(!g;
	s!([, (])kAbs\(!\1qAbs\(!g;
	# never add kClamp here! it's no easy search & replace there
    
        s!Q_INT8!qint8!g;
        s!Q_UINT8!quint8!g;
        s!Q_INT16!qint16!g;
        s!Q_UINT16!quint16!g;
        s!Q_INT32!qint32!g;
        s!Q_UINT32!quint32!g;
        s!Q_INT64!qint64!g;
        s!Q_UINT64!quint64!g;
        s!Q_LLONG!qint64!g;
        s!Q_ULLONG!quint64!g;
		s!QMAX!qMax!g;
        s!QMIN!qMin!g;
 	
	s!KApplication::addCmdLineOptions!KCmdLineArgs::addStdCmdLineOptions!;
    s!Qt::ShiftButton!Qt::ShiftModifier!;
    s!Qt::ControlButton!Qt::ControlModifier!;
    s!Qt::AltButton!Qt::AltModifier!;
    s!Qt::MetaButton!Qt::MetaModifier!;
    s!Qt::Keypad!Qt::KeypadModifier!;
    s!Qt::KeyButtonMask!Qt::KeyboardModifierMask!;
    s!convertToAbs!makeAbsolute!;
    s!currentDirPath!currentPath!;
    s!homeDirPath!homePath!;
    s!rootDirPath!rootPath!;
    s!cleanDirPath!cleanPath!;
    s!absFilePath!absoluteFilePath!;
    s!QDir::All!QDir::TypeMask!;
    s!QDir::DefaultFilter!QDir::NoFilter!;
    s!QDir::DefaultSort!QDir::NoSort!;
    s!simplifyWhiteSpace!simplified!g;
    s!stripWhiteSpace!trimmed!g;
    s!ucs2!utf16!g;
    s!leftJustify!leftJustified!g;
    s!rightJustify!rightJustified!g;
    s!fromUcs2!fromUtf16!g;
    s!constref!at!g;
    s!changeInterval!start!g;

	s!flushX!flush!;

	if ( /kapp->authorizeKAction/ ) {
	    s!kapp->authorizeKAction!KAuthorized::authorizeKAction!;
	    push(@necessaryIncludes, "kauthorized.h");
	}
	if ( /kapp->authorize/ ) {
	    s!kapp->authorize!KAuthorized::authorizeKAction!;
	    push(@necessaryIncludes, "kauthorized.h");
	}
	if ( /KApplication::startServiceByDesktopName/ ) {
	    s!KApplication::startServiceByDesktopName!KToolInvocation::startServiceByDesktopName!;
	    push(@necessaryIncludes, "ktoolinvocation.h");
	}
	if ( /kapp->startServiceByDesktopName/ ) {
	    s!kapp->startServiceByDesktopName!KToolInvocation::startServiceByDesktopName!;
	    push(@necessaryIncludes, "ktoolinvocation.h");
	}
        if ( /kapp->startServiceByDesktopPath/ ) {
	    s!kapp->startServiceByDesktopPath!KToolInvocation::startServiceByDesktopPath!;
	    push(@necessaryIncludes, "ktoolinvocation.h");
        }
        if ( /KApplication::startServiceByDesktopPath/ ) {
            s!KApplication::startServiceByDesktopPath!KToolInvocation::startServiceByDesktopPath!;
	    push(@necessaryIncludes, "ktoolinvocation.h");
        }
		
	if ( /kapp->kdeinitExec/ ) {
	    s!kapp->kdeinitExec!KToolInvocation::kdeinitExec!;
	    push(@necessaryIncludes, "ktoolinvocation.h");
	}
	if ( /KApplication::kdeinitExec/ ) {
	    s!KApplication::kdeinitExec!KToolInvocation::kdeinitExec!;
	    push(@necessaryIncludes, "ktoolinvocation.h");
	}
	if ( /kapp->invokeHelp/ ) {
	    s!kapp->invokeHelp!KToolInvocation::invokeHelp!;
	    push(@necessaryIncludes, "ktoolinvocation.h");
	}
    if ( /KApplication::kApplication\s*\(\s*\)->invokeHelp/ ) {
        s!KApplication::kApplication\s*\(\s*\)->invokeHelp!KToolInvocation::invokeHelp!;
        push(@necessaryIncludes, "ktoolinvocation.h");
    }
	if ( /KApplication::kApplication\s*\(\s*\)->invokeMailer/ ) {
	    s!KApplication::kApplication\s*\(\s*\)->invokeMailer!KToolInvocation::invokeMailer!;
	    push(@necessaryIncludes, "ktoolinvocation.h");
	}
	if ( /kapp->invokeMailer/ ) {
	    s!kapp->invokeMailer!KToolInvocation::invokeMailer!;
	    push(@necessaryIncludes, "ktoolinvocation.h");
	}
        if ( /kapp->invokeBrowser/ ) {
	    s!kapp->invokeBrowser!KToolInvocation::invokeBrowser!;
	    push(@necessaryIncludes, "ktoolinvocation.h");
        }
        if ( /kapp->kdeinitExecWait/ ) {
	    s!kapp->kdeinitExecWait!KToolInvocation::kdeinitExecWait!;
	    push(@necessaryIncludes, "ktoolinvocation.h");
        }
	if ( /KApplication::startServiceByDesktopPath/ ) {
	    s!KApplication::startServiceByDesktopPath!KToolInvocation::startServiceByDesktopPath!;
	    push(@necessaryIncludes, "ktoolinvocation.h");
	}
	if ( /KApplication::startServiceByName/ ) {
	    s!KApplication::startServiceByName!KToolInvocation::startServiceByName!;
	    push(@necessaryIncludes, "ktoolinvocation.h");
	}
        if ( /kapp->startServiceByName/ ) {
	    s!kapp->startServiceByName!KToolInvocation::startServiceByName!;
	    push(@necessaryIncludes, "ktoolinvocation.h");
        }	
	if ( /kapp->config/ ) {
	    s!kapp->config!KGlobal::config!;
	    push(@necessaryIncludes, "kglobal.h");
	}
	if ( /kapp->miniIcon/ ) {
	    s!kapp->miniIcon\s*\(\s*\)!qApp->windowIcon().pixmap(IconSize(KIcon::Small),IconSize(KIcon::Small))!;
	    push(@necessaryIncludes, "kiconloader.h");
	}

        if ( /kapp->icon/ ) {
            s!kapp->icon\s*\(\s*\)!qApp->windowIcon().pixmap(IconSize(KIcon::Desktop),IconSize(KIcon::Desktop))!;
            push(@necessaryIncludes, "kiconloader.h");
        }
        if ( /app.icon/ ) {
            s!app.icon\s*\(\s*\)!qApp.windowIcon().pixmap(IconSize(KIcon::Desktop),IconSize(KIcon::Desktop))!;
            push(@necessaryIncludes, "kiconloader.h");
        }

		
	if ( /new KRun/ ) {
	    $warning = $warning . "Be carrefull perhaps necessary to add parent into constructor in file : $file\n";
	}
	s!KParts::ComponentFactory::createInstanceFromLibrary!KLibLoader::createInstance!;
	if ( /kapp->getDisplay/ ) {
	    s!kapp->getDisplay\s*\(\s*\)!QX11Info::display()!;
	    push(@necessaryIncludes, "QX11Info");
	}
	if ( /enableSounds/ ) {
	    s!enableSounds\(\)!setEnableSounds\(true\)!;
	}
	if ( /disableSounds/ ) {
	    s!disableSounds\(\)!setEnableSounds\(false\)!;
	}
	if ( /KSeparator/ ) {
	    s!KSeparator::HLine!Qt::Horizontal!;
	    s!KSeparator::VLine!Qt::Vertical!;
		s!Q3Frame::HLine!Qt::Horizontal!;
		s!Q3Frame::VLine!Qt::Vertical!;
	}
	s!QDir::SortSpec!QDir::SortFlags!;
	
	my $valuereturn = functionUtilkde::removeObjectNameTwoArgument( $_, "KColorButton");
	if ( $valuereturn ) {
	    $_ = $valuereturn;
	}
	my $valuereturn = functionUtilkde::removeObjectNameTwoArgument( $_, "KListView");
	if ( $valuereturn ) {
	    $_ = $valuereturn;
	}
			
	my $valuereturn = functionUtilkde::removeObjectNameThreeArgument( $_, "KPushButton");
	if ( $valuereturn ) {
	    $_ = $valuereturn;
		
	}
        my $valuereturn = functionUtilkde::removeObjectNameThreeArgument( $_, "KComboBox");
        if ( $valuereturn ) {
	    $_ = $valuereturn;

	}
        my $valuereturn = functionUtilkde::removeObjectNameThreeArgument( $_, "KLineEdit");
        if ( $valuereturn ) {
	    $_ = $valuereturn;

        }
		
		
	#if( /setStatusText/ ) {
	#	s!setStatusText!setToolTip!;
	#}
        if ( /kapp->geometryArgument/ ) {
	    s!kapp->geometryArgument\s*\(\s*\);!QString geometry;\nKCmdLineArgs *args = KCmdLineArgs::parsedArgs("kde");\nif (args && args->isSet("geometry"))\ngeometry = args->getOption("geometry");\n!;
        }
        s!KFileMetaInfo::KiloBytes!KFileMimeTypeInfo::KibiBytes!;
	s!KIO::convertSizeFromKB!KIO::convertSizeFromKiB!;
	s!KMimeType::iconForURL!KMimeType::iconNameForURL!;
	$modified ||= $orig ne $_;
	$_;
    } <$FILE>;

    if ($modified) {
	open (my $OUT, ">$file");
	print $OUT @l;
    }

    my %alreadyadded = {};
    foreach my $inc (@necessaryIncludes) {
	next if (defined $alreadyadded{$inc});
	$alreadyadded{$inc} = 1;
	
	functionUtilkde::addIncludeInFile( $file, $inc );
    }
}
functionUtilkde::diffFile( <$F> );
warn "Warning: $warning\n";
