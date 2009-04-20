#!/usr/bin/perl

# laurent Montel <montel@kde.org> 2005,2006,2007 GPL
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
    if ( $_ =~ /\b(\w+(?:\.|->))htmlURL\(\)/ ) { # KUrl::htmlURL() had to disappear
        #$var=$1;
        #s/${var}htmlURL\(\)/Qt::escape(${var}prettyURL())/;
        #push(@necessaryIncludes, "QTextDocument");
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
    s!\benableButtonOK\b!enableButtonOk!g;
    s!\benableButtonSeparator\b!showButtonSeparator!;
    #TODO test it, perhaps remove all before isRestored (for example if( kapp-> isRestored())
    s!kapp->isRestored!kapp->isSessionRestored!;
    #add some standard version :)
    s!kapp.isRestored!kapp.isSessionRestored!;
    s!app.isRestored!app.isSessionRestored!;
    s!a.isRestored!a.isSessionRestored!;

    s!KCursor::arrowCursor\(\)!Qt::ArrowCursor!;
    s!KCursor::workingCursor\(\)!Qt::BusyCursor!;
    s!KCursor::waitCursor\(\)!Qt::WaitCursor!;
    s!KCursor::crossCursor\(\)!Qt::CrossCursor!;
    s!KCursor::handCursor\(\)!Qt::PointingHandCursor!;
    s!KCursor::blankCursor\(\)!Qt::BlankCursor!;
    s!KCursor::sizeVerCursor\(\)!Qt::SizeVerCursor!;
    s!KCursor::sizeHorCursor\(\)!Qt::SizeHorCursor!;
    s!KCursor::sizeBDiagCursor\(\)!Qt::SizeBDiagCursor!;
    s!KCursor::sizeFDiagCursor\(\)!Qt::SizeFDiagCursor!;
    s!KCursor::sizeAllCursor\(\)!Qt::SizeAllCursor!;

    s!#include <kuniqueapp.h>!#include <kuniqueapplication.h>!;
    s!#include <kapp.h>!#include <kapplication.h>!;
    s!#include <kstddirs.h>!#include <kstandarddirs.h>!;
    s!#include <kcodecs.h>!#include <kcodecs.h>!;

    s!#include <kwin.h>!#include <kwindowsystem.h>!;
    s!#include <kwinmodule.h>!#include <kwindowsystem.h>!;
    s!\bKWin\b!KWindowSystem!g;
    s!\bKWinModule\b!KWindowSystem!g;

    s!#include <ktempfile.h>!#include <ktemporaryfile.h>!;
    s!\bKTempFile\b!KTemporaryFile!g;

    s!kinstance.h!kcomponentdata.h!;

    #Remove this include
    s!#include <dcopobject.h>!!;
    s!#include <kactionclasses.h>!!;
    # kde3support
    s!#include <kstreamsocket.h>!#include <k3streamsocket.h>!;
    s!#include <ksocketbase.h>!#include <k3socketbase.h>!;
    s!#include <ksocketdevice.h>!#include <k3socketdevice.h>!;
    s!#include <ksocketaddress.h>!#include <k3socketaddress.h>!;
    s!#include <kactivelabel.h>!#include <k3activelabel.h>!;
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
    s!#include <kmdcodec.h>!#include <kcodecs.h>!;
    s!#include <kresolver.h>!#include <k3resolver.h>!;
    s!#include <ksocks.h>!#include <k3socks.h>!;
    s!#include <kbufferedsocket.h>!#include <k3bufferedsocket.h>!;
    s!#include <kserversocket.h>!include <k3serversocket.h>!;
    s!#include <kcommand.h>!#include <k3command.h>!;
    s!\bKCommand\b!K3Command!g;
    s!\bKNamedCommand\b!K3NamedCommand!g;
    s!\bKMacroCommand\b!K3MacroCommand!g;
    s!\bKCommandHistory\b!K3CommandHistory!g;
    s!\bKUndoRedoAction\b!K3UndoRedoAction!g;
    s!#include <kiconview.h>!#include <k3iconview.h>!;
    s!\bKIconViewItem\b!K3IconViewItem!g;
    s!\bKIconView\b!K3IconView!g;
    s!#include <klistview.h>!#include <k3listview.h>!;
    s!#include <klistviewsearchline.h>!#include <k3listviewsearchline.h>!;
    s!\bKListView\b!K3ListView!g;
    s!\bKListViewSearchLine\b!K3ListViewSearchLine!g;
    s!\bKListViewSearchLineWidget\b!K3ListViewSearchLineWidget!g;
    s!\bKListViewItem\b!K3ListViewItem!g;
    s!#include <kaboutapplication.h>!#include <k3aboutapplication.h>!;
    s!\bKAboutApplication\b!K3AboutApplication!g;

    s!\bKDockMainWindow\b!K3DockMainWindow!g;
    s!\bKDockWidget\b!K3DockWidget!g;

    s!KInstance::makeStandardCaption!KDialog::makeStandardCaption!g;

    s!\bDockMainWindow\b!DockMainWindow3!g;

    s!\bKStaticDeleter\b!K3StaticDeleter!g;
    s!#include <kstaticdeleter.h>!#include <k3staticdeleter.h>!;

    if ( /KColorDrag/ ) {
        s!#include <kcolordrag.h>!#include <k3colordrag.h>!;
        s!KColorDrag!K3ColorDrag!g;
        $warning = $warning . "K3ColorDrag used, might need to add \${KDE4_KDE3SUPPORT_LIBRARY} in target that contains $file\n";
    }

    if( /KURLDrag/ ) {
       s!#include <kurldrag.h>!#include <k3urldrag.h>!;
       s!KURLDrag!K3URLDrag!g;
       $warning = $warning . "K3URLDrag used, might need to add \${KDE4_KDE3SUPPORT_LIBRARY} in target that contains $file\n";
    }
    s!#include <kpassdlg.h>!#include <kpassworddialog.h>!;

    #If there is more than one argument add KGuiItem now I think that it will easy to fix it.
    s!setButtonOKText!setButtonOK!;
    s!setButtonApplyText!setButtonApply!;
    s!setButtonCancelText!setButtonCancel!;

    # remove deprecated header
    s!#include <kanimwidget.h>!#include <kanimatedbutton.h>!;
    s!#include <kcolordlg.h>!#include <kcolordialog.h>!;
    s!#include <kcolorbtn.h>!#include <kcolorbutton.h>!;
    s!#include <kdatepik.h>!#include <kdatepicker.h>!;
    s!#include <kdualcolorbtn.h>!#include <kdualcolorbutton.h>!;
    s!#include <kxmlgui.h>!#include <kxmlguifactory.h>!;
    s!#include <kstdaction.h>!#include <kstandardaction.h>!;

    # KUrl -> KUrl in all class names [but not in env vars, ifdefs, include guards etc.]
    s!KURLCompletion!KUrlCompletion!g;
    s!KURLCombo!KUrlCombo!g;
    s!KURLRequester!KUrlRequester!g;
    s!KURLPixmapProvider!KUrlPixmapProvider!g;
    s!KURLLabel!KUrlLabel!g;
    s!leftClickedURL!leftClickedUrl!;
    s!rightClickedURL!rightClickedUrl!;
    s!middleClickedURL!middleClickedUrl!;
    s!setKURL!setUrl!;

    #KUrl renames
    s!\bKURL\b!KUrl!g;
    s!\bhasSubURL\b!hasSubUrl!g;
    s!\bprettyURL\b!prettyUrl!g;
    s!\bpathOrURL\b!pathOrUrl!g;
    s!\bupURL\b!upUrl!g;
    s!\bfromPathOrURL\b!fromPathOrUrl!g;
    s!\bisRelativeURL\b!isRelativeUrl!g;
    s!\brelativeURL\b!relativeUrl!g;

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


    s!#include <kselect.h>!#include <kselector.h>!;
    s!#include "kselect.h"!#include "kselector.h"!;

    s!Q3ValueList\s*<\s*KMimeType::Ptr\s*>!KMimeType::List!;
    s!Q3ValueListIterator\s*<\s*KMimeType::Ptr\s*>!KMimeType::List::const_iterator!;
    s!Q3ValueList\s*<\s*KService::Ptr\s*>!KService::List!;
    s!Q3ValueListIterator\s*<\s*KService::Ptr\s*>!KService::List::const_iterator!;

    s!KStartupInfo::appStarted!KStartupInfo::appStarted!;
    s!KInputDialog::getText!KInputDialog::getText!;
    s!#include <kde_file.h>!#include <kde_file.h>!;
    s!#include <kpopupmenu.h>!#include <kmenu.h>!;
    #s!cancelPressed!cancelPressed!;
    #s!suggestNewNamePressed!suggestNewNamePressed!;
    #s!renamePressed!renamePressed!;
    #s!skipPressed!skipPressed!;
    #s!autoSkipPressed!autoSkipPressed!;
    #s!overwritePressed!overwritePressed!;
    #s!overwriteAllPressed!overwriteAllPressed!;
    #s!resumePressed!resumePressed!;
    #s!resumeAllPressed!resumeAllPressed!;
    s!KLocale::setMainCatalogue!KLocale::setMainCatalog!;
    s!KGlobal::locale\(\)->insertCatalogue!KGlobal::locale\(\)->insertCatalog!;
    s!KGlobal::locale\(\)->setMainCatalogue!KGlobal::locale\(\)->setMainCatalog!;

    s!KLocale::removeCatalogue!KLocale::removeCatalog!;
    s!KGlobal::locale\(\)->removeCatalogue!KGlobal::locale\(\)->removeCatalog!;
    s!KGlobal::locale\(\)->removeCatalogue!KGlobal::locale\(\)->removeCatalog!;

    s!KLocale::setActiveCatalogue!KLocale::setActiveCatalog!;
    s!KGlobal::locale\(\)->setActiveCatalogue!KGlobal::locale\(\)->setActiveCatalog!;
    s!KGlobal::iconLoader!KIconLoader::global!;
    s!KGlobal::instance\(\)->iconLoader!KIconLoader::global!;
    s!KIconLoader::global\s*\(\)->loadIconSet\s*\(\s*"(.+?)"\s*,\s*KIcon::Small\s*\)!KIcon("$1")!;

    s!locateLocal!KStandardDirs::locateLocal! unless (/KStandardDirs::locateLocal/);

    s!KIcon::NoGroup!KIconLoader::NoGroup!g;
    s!KIcon::DefaultState!KIconLoader::DefaultState!g;

    s!KIcon::Desktop!KIconLoader::Desktop!g;
    s!KIcon::Toolbar!KIconLoader::Toolbar!g;
    s!KIcon::Panel!KIconLoader::Panel!g;
    s!KIcon::Dialog!KIconLoader::Dialog!g;

    s!KIcon::SizeMedium!KIconLoader::SizeMedium!g;
    s!KIcon::SizeSmall!KIconLoader::SizeSmall!g;
    s!KIcon::SizeLarge!KIconLoader::SizeLarge!g;
    s!KIcon::SizeSmallMedium!KIconLoader::SizeSmallMedium!g;
    s!KIcon::SizeHuge!KIconLoader::SizeHuge!g;
    s!KIcon::SizeEnormous!KIconLoader::SizeEnormous!g;


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

    s!KApplication::addCmdLineOptions!KCmdLineArgs::addStdCmdLineOptions!;

    if (/kdDebug/ || /kdWarning/ || /kdError/ || /kdFatal/) {
        s!kdDebug!kDebug!g;
        s!kdWarning!kWarning!g;
        s!kdError!kError!g;
        s!kdFatal!kFatal!g;
        s/\s*<< endl//;
    }
    s!kdBacktrace!kBacktrace!g;
    s!kdClearDebugConfig!kClearDebugConfig!g;
    s!kndDebug!kDebugDevNull!g;

    s!\bKStdAction\b!KStandardAction!g;
    s!\bKStdGuiItem\b!KStandardGuiItem!g;
    s!\bKStandardGuiItem::StdItem\b!KStandardGuiItem::StandardItem!g;
    s!\bKStdAccel\b!KStandardShortcut!g;
    s!\bKStandardShortcut::StdAccel\b!KStandardShortcut::StandardShortcut!g;
    s!\bKStandardShortcut::findStdAccel\b!KStandardShortcut::findStandardShortcut!g;
    s!\bKTimezone\b!KTimeZone!g;

    s!\bKTimezones\b!KTimeZones!g;

    s!\bKTimezoneData\b!KTimeZoneData!g;

    s!\bKTimezoneSource\b!KTimeZoneSource!g;

    s!\bKActiveLabel\b!K3ActiveLabel!g;
    s!\bKWidgetAction\b!K3WidgetAction!g;
    s!\bKEditToolbar\b!KEditToolBar!g;
    s!\bKEditToolbarWidget\b!KEditToolBarWidget!g;

    s!\bKListBox\b!K3ListBox!g;
    s!klistbox.h!k3listbox.h!;

    s!KImageIO::registerFormats\s*\(\s*\);!!;

    s!#include <kpixmapeffect.h>!!;
    
    s!#include <ktoolbarbutton.h>!!g;
    s!\baddURL\b!addUrl!g;

    s!kapp->makeStdCaption!KInstance::makeStandardCaption!;
    s!kapp->caption!KInstance::caption!;

    s!\bsetEnableSounds\b!setSoundsEnabled!;
    s!\bisSoundsEnabled\b!soundsEnabled!;
    s!\bsetEnableSqueezedText\b!setSqueezedTextEnabled!;
    s!\bisSqueezedTextEnabled\b!squeezedTextEnabled!;
    s!\bsetEnableContextMenu\b!setContextMenuEnabled!;

    s!\bKProcess\b!K3Process!g;
    s!\bKProcIO\b!K3ProcIO!g;
    s!\bKShellProcess\b!K3ShellProcess!g;
    s!#include <kprocio.h>!#include <k3procio.h>!;
    s!#include <kprocess.h>!#include <k3process.h>!;

    s!#include <kfiletreeview.h>!#include <k3filetreeview.h>!;
    s!#include "kfiletreeview.h"!#include "k3filetreeview.h"!;
    s!KFileTreeView!K3FileTreeView!g;
    s!#include <kfileiconview.h>!#include <k3fileiconview.h>!;
    s!#include "kfileiconview.h"!#include "k3fileiconview.h"!;
    s!KFileIconViewItem!K3FileIconViewItem!g;
    s!KFileIconView!K3FileIconView!g;

#    s!#include <kfiledetailview.h>!#include <k3filedetailview.h>!;
#    s!#include "kfiledetailview.h"!#include "k3filedetailview.h"!;

    s!KFileListViewItem!K3FileListViewItem!g;
    s!KFileDetailView!K3FileDetailView!g;
    s!#include <kfiletreeview.h>!#include <k3filetreeview.h>!;
    s!#include "kfiletreeview.h"!#include "k3filetreeview.h"!;
    s!KFileTreeView!K3FileTreeView!g;
    s!#include <klistbox.h>!#include <k3listbox.h>!;
    s!#include "klistbox.h"!#include "k3listbox.h"!;
    s!KListBox!K3ListBox!g;
    s!#include <kcompletionbox.h>!#include <k3completionbox.h>!;
    s!#include "kcompletionbox.h"!#include "k3completionbox.h"!;
    s!KCompletionBox!K3CompletionBox!g;
    s!#include <kfiletreeviewitem.h>!#include <k3filetreeviewitem.h>!;
    s!#include "kfiletreeviewitem.h"!#include "k3filetreeviewitem.h"!;
    s!currentK3FileTreeViewItem!currentKFileTreeViewItem!g;


    s!\bKHistoryCombo\b!KHistoryComboBox!g;

    s!\bKButtonBox\b!K3ButtonBox!g;
    s!#include <kbuttonbox.h>!#include <k3buttonbox.h>!;
    
    s!\bKDiskFreeSp\b!KDiskFreeSpace!g;
    s!#include <kdiskfreesp.h>!#include <kdiskfreespace.h>!;

    s!\btwoAlphaToLanguageName\b!languageCodeToName!;

    s!#include <kprogress.h>!#include <kprogressdialog.h>!;

    s!#include <klineeditdlg.h>\n!!;

    if ( /K3Process::quote/ ) {
       s!\bK3Process::quote\b!KShell::quoteArg!g;
       push(@necessaryIncludes, "kshell.h");
    }
    if ( /KKeyDialog::configure/ ) {
    	s!\bKKeyDialog::configure\b!KShortcutsDialog::configure!;
	push(@necessaryIncludes, "KShortcutsDialog");
    }
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
        s!kapp->miniIcon\s*\(\s*\)!qApp->windowIcon().pixmap(IconSize(KIconLoader::Small),IconSize(KIconLoader::Small))!;
        push(@necessaryIncludes, "kiconloader.h");
    }

    if ( /kapp->icon/ ) {
	s!kapp->icon\s*\(\s*\)!qApp->windowIcon().pixmap(IconSize(KIconLoader::Desktop),IconSize(KIconLoader::Desktop))!;
	push(@necessaryIncludes, "kiconloader.h");
    }
    if ( /app.icon/ ) {
	s!app.icon\s*\(\s*\)!qApp.windowIcon().pixmap(IconSize(KIconLoader::Desktop),IconSize(KIconLoader::Desktop))!;
	push(@necessaryIncludes, "kiconloader.h");
    }

    if ( /new KRun/ ) {
        $warning = $warning . "new KRun in $file - might need to add parent argument\n";
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
    s!KFileDialog::getExistingUrl!KFileDialog::getExistingDirectoryUrl!;
    s!KFileMetaInfo::KiloBytes!KFileMimeTypeInfo::KibiBytes!;
    s!\bKFileDialog::getOpenURLs\b!KFileDialog::getOpenUrls!;
    s!\bKFileDialog::getOpenURL\b!KFileDialog::getOpenUrl!;
    s!KFileDialog::getImageOpenURL!KFileDialog::getImageOpenUrl!;
    s!KFileDialog::getSaveURL!KFileDialog::getSaveUrl!;
    s!KFileDialog::getExistingURL!KFileDialog::getExistingUrl!;
    s!KIO::convertSizeFromKB!KIO::convertSizeFromKiB!;
    s!KMimeType::iconForURL!KMimeType::iconNameForUrl!;
    s!KMimeType::findByURL!KMimeType::findByUrl!;
    s!const\s+KFileItem\s\*\s*!const KFileItem& !g if (/gotPreview/ || /slotGotPreview/ || /slotPreview/); # KIO::PreviewJob

    # KIO Job API changes
    s/, ?false, ?false/, KIO::HideProgressInfo/ if (/KIO::del/);
    s/, ?false, ?false/, KIO::NoReload, KIO::HideProgressInfo/ if (/KIO::get/ || /KIO::storedGet/);
    s/, ?true, ?false/, KIO::Reload, KIO::HideProgressInfo/ if (/KIO::get/ || /KIO::storedGet/);
    s/, ?false, ?true// if (/KIO::get/ || /KIO::storedGet/);
    s/, ?false/, KIO::HideProgressInfo/ if (/KIO::file_delete/ || /KIO::stat/);
    s/, ?false/, KIO::HideProgressInfo/ if (/KIO::special/ || /KIO::http_post/ || /KIO::rename/ || /KIO::move/);
    s/, ?true// if (/KIO::special/);
    s/, ?false, ?false/, KIO::HideProgressInfo, false/ if (/KIO::listDir/);
    s/, ?false, ?true/, KIO::HideProgressInfo, true/ if (/KIO::listDir/);
    s/, ?false, ?false, ?false/, KIO::HideProgressInfo/ if (/KIO::storedPut/);
    s/, ?true, ?false, ?false/, KIO::Overwrite | KIO::HideProgressInfo/ if (/KIO::file_copy/ || /KIO::file_move/);
    s/, ?true, ?false, ?true/, KIO::Overwrite/ if (/KIO::file_copy/ || /KIO::file_move/);
    s/, ?false, ?false, ?false/, KIO::HideProgressInfo/ if (/KIO::file_copy/ || /KIO::file_move/);

    $_="" if (/include <kipc.h>/);
    s/KIPC::sendMessageAll/KGlobalSettings::self()->emitChange/;
    s/KIPC::/KGlobalSettings::/;
    s/KApplication::SETTINGS_/KGlobalSettings::SETTINGS_/;
    s/KApplication::createApplicationPalette/KGlobalSettings::createApplicationPalette/;

    #KConfig changes
    s/\bentryIsImmutable\b/isEntryImmutable/g;
    s/\bgroupIsImmutable\b/isGroupImmutable/g;
    s/\bgetConfigState\b/accessMode/g;
    s/\bConfigState\b/AccessMode/g;
    s/\b(KConfigGroup|KConfig|KConfigBase)::NLS\b/\1::Localized/g;
    s/\bKConfig::OnlyLocal\b/KConfig::SimpleConfig/g;
    s/\breadPathEntry(\s*)\(([^,)]+?)(\s*)\)/readPathEntry\1(\2, QString()\3)/g;
    s/\breadPathListEntry(\s*)\(([^,)]+,[^,)]+)\)/readPathEntry\1(\2)/g;
    s/\breadPathListEntry(\s*)\(([^,)]+?)(\s*)\)/readPathEntry\1(\2, QStringList()\3)/g;

    #KNotify changes
    s!#include <knotifyclient.h>!#include <knotification.h>!;
    s!#include <knotifydialog.h>!#include <knotifyconfigwidget.h>!;
    s!KNotifyClient::event\((\s*winId\s*\(\s*\)\s*?)\,\s*\"(.*?)\"\s*\,\s*(.*?)\s*\);!KNotification::event\(QString::fromLatin1\(\"$2\"\), $3, QPixmap(), this\);!;
    s!KNotifyClient::event\(\s*([a-zA-Z0-9_]+?)\s*\->\s*winId\s*\(\s*\)\s*\,\s*\"(.*?)\"\s*\,\s*(.*?)\s*\);!KNotification::event\(QString::fromLatin1\(\"$2\"\), $3, QPixmap(), $1\);!;
    s!KNotifyClient::event!KNotification::event!;
    s!KNotifyDialog::configure!KNotifyConfigWidget::configure!;

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
warn "Warning: $warning\n" if ($warning != "");
