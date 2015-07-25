#!/usr/bin/perl -w

# KParts was cleaned up to have one include file for each class, while in kdelibs4 #include <kparts/part.h> included many things
# Usage: add_missing_kpart_include.pl *.h *.cpp
# Recursive usage: find -iname "*.cpp" -o -iname "*.h" |xargs kde-dev-scripts/kf5/add_missing_kpart_include.pl
#
# Laurent Montel <montel@kde.org> (2014)
# David Faure <faure@kde.org> (2014)

use strict;
use File::Basename;
use lib dirname($0);
use functionUtilkde;

my @classes = (
        'BrowserArguments',
        'BrowserExtension',
        'BrowserHostExtension',
        'BrowserInterface',
        'BrowserOpenOrSaveQuestion',
        'BrowserRun',
        'Event',
        'FileInfoExtension',
        'GUIActivateEvent',
        'HistoryProvider',
        'HtmlExtension',
        'HtmlSettingsInterface',
        'ListingFilterExtension',
        'ListingNotificationExtension',
        'LiveConnectExtension',
        'MainWindow',
        'OpenUrlArguments',
        'OpenUrlEvent',
        'Part',
        'PartActivateEvent',
        'PartBase',
        'PartManager',
        'PartSelectEvent',
        'Plugin',
        'ReadOnlyPart',
        'ReadWritePart',
        'ScriptableExtension',
        'SelectorInterface',
        'StatusBarExtension',
        'TextExtension',
        'WindowArgs'
);

foreach my $file (@ARGV) {

    my %includesToBeAdded = ();
    my %fwdDecls = ();
    my $wasIncludingPartH = 0;
    my $modified;
    open(my $FILE, "<", $file) or warn "We can't open file $file:$!\n";
    my @l = map {
        my $orig = $_;
        foreach my $class (@classes) {
            if (/class $class;/) {
                $fwdDecls{$class} = 1;
            }
            if (/KParts::$class/ && !defined $fwdDecls{$class}) {
                # include the necessary header, unless we saw a fwd decl
                $includesToBeAdded{"KParts/$class"} = 1;
                $modified = 1;
            }
        }
        # get rid of lowercase includes, to avoid duplicates
        if (/^#include .kparts\/(\w+).h/) {
            $wasIncludingPartH = 1 if ($1 eq 'part');
            $_ = "";
        }
        $modified ||= $orig ne $_;
        $_;
    } <$FILE>;

    if ($modified) {
        open (my $OUT, ">", $file);
        print $OUT @l;
        close ($OUT);
        if ($wasIncludingPartH) {
            foreach my $include (keys %includesToBeAdded) {
                functionUtilkde::addIncludeInFile($file, $include);
            }
        }
    }
}

functionUtilkde::diffFile( "@ARGV" );
