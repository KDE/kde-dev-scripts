#!/usr/bin/perl

# David Faure <faure@kde.org>
# This script converts KStringHandler::*EmSqueeze/*PixelSqueeze to QFontMetrics::elidedText
# Based on convert-qstringlist.

use File::Basename;
use lib dirname( $0 );
use functionUtilkde;

foreach my $file (@ARGV) {
    functionUtilkde::substInFile {
        s!KStringHandler::lEmSqueeze\s*\(\s*(.+)\s*,\s*(.+)\s*,\s*(.+)\s*\)!$2.elidedText( $1, Qt::ElideLeft, $2.maxWidth() * $3 )!g;
        s!KStringHandler::cEmSqueeze\s*\(\s*(.+)\s*,\s*(.+)\s*,\s*(.+)\s*\)!$2.elidedText( $1, Qt::ElideMiddle, $2.maxWidth() * $3 )!g;
        s!KStringHandler::rEmSqueeze\s*\(\s*(.+)\s*,\s*(.+)\s*,\s*(.+)\s*\)!$2.elidedText( $1, Qt::ElideRight, $2.maxWidth() * $3 )!g;

        s!KStringHandler::lPixelSqueeze\s*\(\s*(.+)\s*,\s*(.+)\s*,\s*(.+)\s*\)!$2.elidedText( $1, Qt::ElideLeft, $3 )!g;
        s!KStringHandler::cPixelSqueeze\s*\(\s*(.+)\s*,\s*(.+)\s*,\s*(.+)\s*\)!$2.elidedText( $1, Qt::ElideMiddle, $3 )!g;
        s!KStringHandler::rPixelSqueeze\s*\(\s*(.+)\s*,\s*(.+)\s*,\s*(.+)\s*\)!$2.elidedText( $1, Qt::ElideRight, $3 )!g;

    } $file;
    functionUtilkde::removeIncludeInFile( $file, "kstringhandler.h");
}
functionUtilkde::diffFile( "@ARGV" );
