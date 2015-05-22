#!/usr/bin/perl -w

# Laurent Montel <montel@kde.org> (2014)
# find -iname "*.cpp"|xargs kde-dev-scripts/kf5/remove-kde4support.pl

use strict;
use File::Basename;
use lib dirname($0);
use functionUtilkde;

foreach my $file (@ARGV) {

    my $modified;
    my $needQApplicationHeader;
    my $needQDesktopWidgetHeader;
    my $needKColorScheme;
    my $needQStandardPaths;
    my $needKFormat;
    my $needQFontDatabase;
    my $needQDir;
    my $needKComponentData;
    my $removeKdemacros;
    my $needQMimeDatabase;
    my $needKHelpClient;
    my $needKlocalizedString;
    open(my $FILE, "<", $file) or warn "We can't open file $file:$!\n";
    my @l = map {
        my $orig = $_;

        s/\bQ_WS_WIN/Q_OS_WIN/g;
        s/\bQ_WS_MAC/Q_OS_MAC/g;


        s/\bkasciistricmp\b/qstricmp/;
        s/KGlobal::config\(\)/KSharedConfig::openConfig()/g;

        # convert KCategoryDrawerV2 and KCategoryDrawerV3 to KCategoryDrawer
        s/KCategoryDrawerV3/KCategoryDrawer/g;
        s/KCategoryDrawerV2/KCategoryDrawer/g;
 
        if ( /KLocale::global\(\)\-\>removeAcceleratorMarker/ ) {
           warn "removeAcceleratorMarker found \n";
           s,KLocale::global\(\)\-\>removeAcceleratorMarker\b,KLocalizedString::removeAcceleratorMarker,;
           $needKlocalizedString = 1;
        }
   
        if (/KToolInvocation::invokeHelp/) {
           s/KToolInvocation::invokeHelp/KHelpClient::invokeHelp/;
           $needKHelpClient = 1;
        }

        s,::self\(\)\-\>writeConfig\(\),::self\(\)\-\>save\(\),;
        s,::self\(\)->readConfig\(\),::self\(\)\-\>load\(\),;
        if (/KGlobalSettings::dndEventDelay/) {
           s,KGlobalSettings::dndEventDelay\b,QApplication::startDragDistance,;
           $needQApplicationHeader = 1;
        }
        if (/KGlobalSettings::desktopGeometry/) {
           s,KGlobalSettings::desktopGeometry\b,QApplication::desktop\(\)\-\>screenGeometry,;
           $needQApplicationHeader = 1;
           $needQDesktopWidgetHeader = 1;
        }
        if (/KGlobalSettings::createApplicationPalette/) {
           s,KGlobalSettings::createApplicationPalette\b,KColorScheme::createApplicationPalette,;
           $needKColorScheme = 1;
        }
        if (/KGlobalSettings::documentPath/) {
           s,KGlobalSettings::documentPath\(\),QStandardPaths::writableLocation\(QStandardPaths::DocumentsLocation\),;
           $needQStandardPaths = 1;
        }
        if (/KGlobalSettings::desktopPath/) {
           s,KGlobalSettings::desktopPath\(\),QStandardPaths::writableLocation\(QStandardPaths::DesktopLocation\),;
           $needQStandardPaths = 1;
        }
        if (/KGlobalSettings::musicPath/) {
           s,KGlobalSettings::musicPath\(\),QStandardPaths::writableLocation\(QStandardPaths::MusicLocation\),;
           $needQStandardPaths = 1;
        }
        if (/KGlobalSettings::videosPath/) {
           s,KGlobalSettings::videosPath\(\),QStandardPaths::writableLocation\(QStandardPaths::MoviesLocation\),;
           $needQStandardPaths = 1;
        }
        if (/KGlobalSettings::downloadPath/) {
           s,KGlobalSettings::downloadPath\(\),QStandardPaths::writableLocation\(QStandardPaths::DownloadLocation\),;
           $needQStandardPaths = 1;
        }
        if (/KGlobalSettings::picturesPath/) {
           s,KGlobalSettings::picturesPath\(\),QStandardPaths::writableLocation\(QStandardPaths::PicturesLocation\),;
           $needQStandardPaths = 1;
        }

        s/\bKGlobal::charsets\s*\(\)/KCharsets::charsets\(\)/;

        if (/KGlobal::locale\(\)\-\>formatByteSize/) {
           s,KGlobal::locale\(\)\-\>formatByteSize,KFormat\(\).formatByteSize,g;
           $needKFormat = 1;
        }

        if (/KGlobal::locale\(\)\-\>prettyFormatDuration/) {
           s,KGlobal::locale\(\)\-\>prettyFormatDuration,KFormat\(\).formatSpelloutDuration,g;
           $needKFormat = 1;
        }

        if (/KGlobal::locale\(\)\-\>insertCatalog/) {
           s/KGlobal::locale\(\)\-\>insertCatalog/\/\/KF5 port: remove this line and define TRANSLATION_DOMAIN in CMakeLists.txt instead\n\/\/KLocale::global\(\)\-\>insertCatalog/;
        }
        s/\bKGlobal::locale\s*\(\)/KLocale::global\(\)/g;

        if (/KGlobalSettings::generalFont/) {
           s,KGlobalSettings::generalFont\s*\(\s*\),QFontDatabase::systemFont\(QFontDatabase::GeneralFont\),g;
           $needQFontDatabase = 1;
        }
        if (/KGlobalSettings::fixedFont/) {
           s,KGlobalSettings::fixedFont\s*\(\s*\),QFontDatabase::systemFont\(QFontDatabase::FixedFont\),g;
           $needQFontDatabase = 1;
        }
        if (/KGlobalSettings::windowTitleFont/) {
           s,KGlobalSettings::windowTitleFont\s*\(\s*\),QFontDatabase::systemFont\(QFontDatabase::TitleFont\),g;
           $needQFontDatabase = 1;
        }
        if (/KGlobalSettings::smallestReadableFont/) {
           s,KGlobalSettings::smallestReadableFont\s*\(\s*\),QFontDatabase::systemFont\(QFontDatabase::SmallestReadableFont\),g;
           $needQFontDatabase = 1;
        }

        if (/KGlobalSettings::contrast\b/) {
           s,KGlobalSettings::contrast\b,KColorScheme::contrast,g;
           $needKColorScheme = 1;
        }
        if (/KGlobalSettings::contrastF\b/) {
           s,KGlobalSettings::contrastF\b,KColorScheme::contrastF,g;
           $needKColorScheme = 1;
        }
        if (/KStandardDirs::makeDir\b/) {
           $needQDir = 1;
           s,KStandardDirs::makeDir\b,QDir\(\)\.mkpath,; 
        }
        if (/KGlobal::activeComponent\b/) {
           s,KGlobal::activeComponent\b,KComponentData::activeComponent,;
           $needKComponentData = 1;
        }
        if (/KGlobal::setActiveComponent\b/) {
           s,KGlobal::setActiveComponent\b,KComponentData::setActiveComponent,;
           $needKComponentData = 1;
        }
        if (/KUrl::fromPath/) {
           s,KUrl::fromPath\b,QUrl::fromLocalFile,;
        }

        s/KUrl::toPercentEncoding\b/QUrl::toPercentEncoding/g;
        s/KUrl::fromPercentEncoding\b/QUrl::fromPercentEncoding/g;

        if (/KDE_EXPORT KCModule/) {
           s,KDE_EXPORT KCModule,Q_DECL_EXPORT KCModule,;
           $removeKdemacros = 1;
        }
        if (/KConfigSkeleton::usrReadConfig\b/) {
           s,KConfigSkeleton::usrReadConfig\b,KConfigSkeleton::usrRead,;
        }
        if (/KApplication::clipboard\b/) {
           s,KApplication::clipboard\b,QApplication::clipboard,;
        }
        if (/KApplication::isRightToLeft\b/) {
           s,KApplication::isRightToLeft\b,QApplication::isRightToLeft,;
        }
        s,KViewStateSaver\b,KConfigViewStateSaver,;
        s/\<KViewStateSaver\b\>/\<KConfigViewStateSaver>/ if (/#include/);
        s/\<kviewstatesaver\.h\>/\<KConfigViewStateSaver\>/ if (/#include/);
        my $regexp = qr/
           ^(\s*)                        # (1) Indentation, possibly "Classname *" (the ? means non-greedy)
           (.*?)                         # (2) Possibly "Classname *" (the ? means non-greedy)
           KMimeType::extractKnownExtension(.*)$                         # (3) afterreg
           /x; # /x Enables extended whitespace mode
        if (my ($indent, $left, $after) = $_ =~ $regexp) {
           $_ = $indent . "QMimeDatabase db;\n";
           $_ .= $indent . "$left" . "db.suffixForFileName" . $after . "\n";
           $needQMimeDatabase = 1;
        }

        my $regexpEscape = qr/
           ^(\s*)            # (1) Indentation
           (.*?)             # (2) Possibly "Classname *" (the ? means non-greedy)
           Qt::escape
           ${functionUtilkde::paren_begin}3${functionUtilkde::paren_end}  # (3) (args)
           (.*)$                        # (4) end              
           /x; # /x Enables extended whitespace mode
        if (my ($indent, $left, $args, $end) = $_ =~ $regexpEscape) {
           #warn "found escape $args \n";
           $args =~ s/^\(\s*//;
           $args =~ s/\s*\)$//;
           if ($args =~ /^\*/) {
             $_ = $indent . $left . "($args).toHtmlEscaped()" . $end . "\n";

           } else {
             $_ = $indent . $left . "$args.toHtmlEscaped()" . $end . "\n";
           }
        }

        $modified ||= $orig ne $_;
        $_;
    } <$FILE>;

    if ($modified) {
        open (my $OUT, ">", $file);
        print $OUT @l;
        close ($OUT);
        if ($needQApplicationHeader) {
           functionUtilkde::addIncludeInFile($file, "QApplication");
        }
        if ($needQDesktopWidgetHeader) {
           functionUtilkde::addIncludeInFile($file, "QDesktopWidget");
        }
        if ($needKColorScheme) {
           functionUtilkde::addIncludeInFile($file, "KColorScheme");
        }
        if ($needQStandardPaths) {
           functionUtilkde::addIncludeInFile($file, "QStandardPaths");
        }
        if ($needKFormat) {
           functionUtilkde::addIncludeInFile($file, "KFormat");
        }
        if ($needQFontDatabase) {
           functionUtilkde::addIncludeInFile($file, "QFontDatabase");
        }
        if ($needQDir) {
           functionUtilkde::addIncludeInFile($file, "QDir");
        }
        if ($needKComponentData) {
           functionUtilkde::addIncludeInFile($file, "KComponentData");
        }
        if ($removeKdemacros) {
           functionUtilkde::removeIncludeInFile($file, "kdemacros.h");
        }
        if ($needQMimeDatabase) {
           functionUtilkde::addIncludeInFile($file, "QMimeDatabase");
        }
        if ($needKHelpClient) {
           functionUtilkde::addIncludeInFile($file, "KHelpClient");
        }
        if ( $needKlocalizedString ) {
           functionUtilkde::addIncludeInFile($file, "KLocalizedString");
        }
    }
}

functionUtilkde::diffFile( "@ARGV" );
